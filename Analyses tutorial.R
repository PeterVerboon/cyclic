#require(dplyr)   # for summarise
require(ggplot2)
require(lme4);   # for lmer
#require(userfriendlyscience)

options(digids=3)

## DATASET 2  CATHERINE  ##

getDat()
# getData(filename="/Users/peterverboon/Documents/Open Universiteit/Onderzoek/Project Cyclic models/SmokingLapse.sav");

dat1 <- data[,c("subjnr","beepnr", "daynr", "NAc", "PAc","Stressc","Zintentie","rookgedrag")]
dat1$intention <- dat1$Zintentie
dat1$positiveAffect <- scale(dat1$PAc)
dat1$stress <- scale(dat1$Stressc)

# work

dat1$intention <- scale(dat1$intention)
dat1$Stress <- NULL



save(dat1, file="smokedat.Rdata")

################ START TUTORIAL ##############################

load("smokedat.Rdata")
names(dat1)

nall <- length(unique(dat1$subjnr))

## Count number of record per subject

dat1$count <- 1
dat2 <- aggregate(dat1[,c("count")],by=list(dat1$subjnr), FUN=sum, na.rm=T);
dat2$subjnr <- dat2$Group.1
dat1$count <- NULL;
dat2$count <- dat2[,2]

dat3 <- merge(dat1,dat2[,c("subjnr","count")], by.x = "subjnr")

rm(dat2)

## remove subjects with less than 50 records 

dat3 <- subset(dat3, dat3$count >= 50)
nfew <- nall - length(unique(dat3$subjnr))

## compute variability of DV per subject

dat2 <- aggregate(dat3$intention,by=list(dat3$subjnr), FUN=sd, na.rm=F)
dat2$subjnr <- dat2$Group.1
dat2$Group.1 <- NULL
dat2 <- merge(dat3, dat2,  by.x = "subjnr")

# remove subjects with very small variation in DV

dat3 <- subset(dat2, dat2$V1 > .10)  
rm(dat2)
nsmall <- (nall - nfew) - length(unique(dat3$subjnr))

cat("Number of subjects in data set: ", nall)
cat("Number of subjects with less than 50 records: ", nfew)
cat("Number of subjects with SD smaller than .10: ", nsmall)
cat("Number of subjects used in analysis: ",length(unique(dat3$subjnr)))



## aggregate over subjects

dat4 <- aggregate(dat3[,c("positiveAffect","stress","intention")],
                  by=list(dat3$beepnr,dat3$daynr), FUN=mean, na.rm=F);    
dat4$daynr <- dat4$Group.2
dat4$beepnr <- dat4$Group.1
dat4$Group.1 <- NULL
dat4$Group.2 <- NULL

### plot raw data

pdat <- dat4   # averaged over all subjects

pdat <- subset(dat3, dat3$subjnr == 15)               # ppn 2, 15, 18 for Intention
                                                      # ppn 41, 50, 15 for positiveAffect,
                                                      # ppn 27, 17 has weekly cycle!
                                                      # ppn 41, 50 for Stressc
npoints <- dim(pdat)[1]
x <- c(1:npoints)
pdat$day <- as.factor(pdat$daynr)


# Plot raw DV of single subject

g <- ggplot(pdat,aes(x=x, y=pdat$intention, colour=pdat$day))
g <- g + geom_point()
g <- g + scale_x_discrete(name ="Time points (beeps within days)",  labels=pdat$beepnr, limits=c(1:npoints))
g <- g + theme(axis.text = element_text(size = 6, colour="black"),legend.position="none")
g



## Analyze cyclic model and plot

a <- fitCyclic(pdat, yvar = "intention", xvar="beepnr", dayNumber = "daynr",  
               ymin = -2.5, ymax = 1.5, step=.30)

a$rawDataPlot
a$meansPlot
a$parameters
summary(a$fit)

## fit extra model with day as covariate 

a <- fitCyclic(pdat,  yvar = "positiveAffect", xvar="beepnr",dayNumber = "daynr", 
               cov = "daynr", ymin=-0.5, ymax=0.5)

a$rawDataPlot
a$meansPlot
a$parameters
summary(a$fit)

pdat <- subset(dat3, dat3$subjnr == 15)   

out <- fitCyclic(pdat,yvar = "stress", xvar="beepnr", dayNumber = "daynr", 
                 ymin = -1.0, ymax = 0.5, step= 0.25)

out$rawDataPlot
out$meansPlot

#### Step 5



## Apply, only for subject 15, the cylic model with a weekly period for stress and positive affect, instead of a daily period. This is shown in Figure 5.


pdat <- subset(dat3, dat3$subjnr == 15)   

out <- fitCyclic(pdat, yvar = "stress", xvar="daynr", dayNumber = "daynr", 
                 ymin = -2.0, ymax = 1.0, step= 0.25)

out$rawDataPlot
out$meansPlot
summary(out$fit)


model1 <- fitCyclicMLA(dat=dat3, form = y ~ 1 + (1 | id), 
                       yvar="intention", id = "subjnr", 
                       ymin = -0.5, ymax = 0.5, step=0.10 )
model1$fit







## Analyze cyclic model with MLA and plot

dat <- dat3

dat$yvar <- dat$positiveAffect
dat$xvar <- dat$beepnr 
P <- max(dat$xvar)
dat$cvar <- cos((2*pi/P)*dat$xvar)
dat$svar <- sin((2*pi/P)*dat$xvar)


fit <- lmer(yvar ~ cvar + svar + (1 +  svar + cvar |subjnr),data = dat)                  # cyclic effect of beeps

summary(fit)

a0 <- fixef(fit)[1]
a1 <- fixef(fit)[2]
a2 <- fixef(fit)[3]
b3 <- fixef(fit)[4]

par <- cycpar(a1,a2)
b <- c(a0,par,b3)
b

dat$ypred = predict(fit)
pdat <- subset(dat, dat$subjnr %in% c(2,15,18))
pdat$subj <- as.factor(pdat$subjnr)

p <- ggplot(pdat, aes(x = beepnr, y = positiveAffect, colour = subj)) + geom_point(size=3)
   + geom_line(aes(y = ypred),size=1) 
print(p)
