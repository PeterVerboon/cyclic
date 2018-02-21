
#### Packages necessary 

require(userfriendlyscience)   # for reading the data
require(dplyr)                 # for summarise
require(ggplot2)               # for gglot
require(lme4);                 # for lmer


options(digids=3)

#### Preliminary STEPS   ####

data <- getData()
# getData(filename="/Users/peterverboon/Documents/Open Universiteit/Onderzoek/Project Cyclic models/SmokingLapse.sav");

dat1 <- data[,c("subjnr","beepnr", "dagnr", "NAc", "PAc","Stressc","Zintentie","rookgedrag")]
dat1$intention <- dat1$Zintentie
dat1$positiveAffect <- scale(dat1$PAc)
dat1$stress <- scale(dat1$Stressc)


## Count number of record per subject

dat1$count <- 1
dat4 <- aggregate(dat1[,c("count")],by=list(dat1$subjnr), FUN=sum, na.rm=T);
dat4$subjnr <- dat4$Group.1

dat1$count <- NULL
dat4$count <- dat4$x
dat4$x <- NULL
dat4$Group.1 <- NULL

a <- merge(dat1,dat4, by.x = "subjnr")

## remove subjects with less than 50 records 

dat3 <- subset(a, a$count >= 50)
length(unique(dat3$subjnr))

## Compute variability of intention per subject and remove subjects with very small variation in intention

a <- summarise(group_by(dat3, subjnr),mean=mean(intention), sd=sd(intention))
a <- merge(dat3, a,  by.x = "subjnr")

dat3 <- subset(a, a$sd > .10)   
length(unique(dat3$subjnr))


## Aggregate over subjects

dat2 <- aggregate(dat3[,c("NAc", "positiveAffect","stress","intention")],by=list(dat3$beepnr,dat3$dagnr), FUN=mean, na.rm=F);    
dat2$dagnr <- dat2$Group.2
dat2$beepnr <- dat2$Group.1
dat2$Group.1 <- NULL
dat2$Group.2 <- NULL

######################    ANALYSES   #########################

### Step 1: plotting the raw data for three subjects and average (Figure 1)

pdat <- dat2   # averaged over all subjects

pdat <- subset(dat3, dat3$subjnr == 2)               # ppn 2, 15, 18 selected for Figure 1
                                                      
npoints <- dim(pdat)[1]
x <- c(1:npoints)
pdat$day <- as.factor(pdat$dagnr)

g <- ggplot(pdat,aes(x=x, y=pdat$intention, colour=pdat$day))
g <- g + geom_point()
g <- g + scale_x_discrete(name ="Time points (beeps within days)",  labels=pdat$beepnr, limits=c(1:npoints))
g <- g + theme(axis.text = element_text(size = 6, colour="black"),legend.position="none")
g

### End step 1

### Repeat step 1 for aggregated data, and subjects 2, 15 and 18


### Step 2: Analyze cyclic model and plot (Figure 2)
 
a <- fitCyclic(pdat, form= "y ~ cvar + svar  ",yvar = "intention", xvar="beepnr", ymin = -1.0, ymax = 0.5, step= 0.25)

a$meansPlot
a$parameters
summary(a$fit)

### End step 2


### Step 3: Fit extra term in the model with day as covariate 

a <- fitCyclic(pdat, form = "y ~ cvar + svar + dagnr", yvar = "intention", xvar="beepnr",ymin=-1.0, ymax=0.5, step = 0.25)

a$parameters
summary(a$fit)

### End step 3


### Step 4: Apply cylic model with daily period for stress and postive affect for subjec 15 (Figure 3)

pdat <- subset(dat3, dat3$subjnr == 15)   

a <- fitCyclic(pdat, form= "y ~ cvar + svar  ",yvar = "stress", xvar="beepnr", ymin = -1.0, ymax = 0.5, step= 0.25)

a$rawDataPlot
a$meansPlot

### End step 4
### Repeat step 4 for positiveAffect and intention


### Step 5: Apply cylic model with weekly period for stress and postive affect for subjec 15 (Figure 4)

pdat <- subset(dat3, dat3$subjnr == 15)   

a <- fitCyclic(pdat, form= "y ~ cvar + svar  ",yvar = "stress", xvar="dagnr", ymin = -2.0, ymax = 1.0, step= 0.25)

a$rawDataPlot
a$meansPlot
summary(a$fit)

### End step 5
### Repeat step 5 for positiveAffect and intention



## Step 6: Analyze cyclic model with MLA and plot

dat <- dat3

dat$yvar <- dat$intention
dat$xvar <- dat$beepnr 
P <- max(dat$xvar)
dat$cvar <- cos((2*pi/P)*dat$xvar)
dat$svar <- sin((2*pi/P)*dat$xvar)

dat$xvar2 <- dat$dagnr 
P2 <- max(dat$xvar2)
dat$cvar2 <- cos((2*pi/P2)*dat$xvar2)
dat$svar2 <- sin((2*pi/P2)*dat$xvar2)



fit0 <- lmer(yvar ~ 1 + (1  |subjnr),data = dat)                  # null model
fit1 <- lmer(yvar ~ cvar + svar + (1 |subjnr),data = dat)                  # cyclic effect of beeps
fit2 <- lmer(yvar ~ cvar + svar + (1 +  svar + cvar |subjnr),data = dat)                  # cyclic effect of beeps

fit3 <- lmer(yvar ~ cvar + svar + cvar2 + svar2 + (1  |subjnr),data = dat)                  # cyclic effect of beeps and days

summary(fit3)
anova(fit2, fit1, fit0)

a0 <- fixef(fit3)[1]
a1 <- fixef(fit3)[2]
a2 <- fixef(fit3)[3]
a3 <- fixef(fit3)[4]
a4 <- fixef(fit3)[5]

b <- c(a0,cycpar(a1,a2, P),cycpar(a3,a4, P))     ## convert to parameters for linear model
b

dat$ypred <-  a0 + b[2]*cos(2*pi/P*(dat$beepnr - b[3]))  + b[4]*cos(2*pi/P2*(dat$dagnr - b[5]))


dat$ypred = predict(fit)

pdat <- subset(dat, dat$subjnr %in% c(15))
pdat$day <- as.factor(pdat$dagnr)
npoints <- dim(pdat)[1]
pdat$xall <- c(1:npoints)

g0 <- ggplot(pdat) + geom_point(aes(x=pdat$xall, y=pdat$intention, colour=pdat$day))
g0 <- g0 + scale_x_discrete(name ="Time points (beeps within days)",  labels=pdat$beepnr, limits=c(1:npoints))
g0 <- g0 + labs(y = yvar)
g0 <- g0 + theme(axis.text = element_text(size = 6, colour="black"),legend.position="none")
g0 <- g0 + geom_line(aes(x=pdat$xall, y=pdat$ypred)) 
g0 <- g0 + coord_cartesian(ylim=c(ymin, ymax)) + scale_y_continuous(breaks=seq(ymin, ymax, step)) 
g0
