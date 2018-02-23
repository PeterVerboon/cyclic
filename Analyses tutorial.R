
#### Packages necessary 

require(userfriendlyscience)   # for reading the data
require(dplyr)                 # for summarise
require(ggplot2)               # for gglot
require(lme4);                 # for lmer


options(digids=3)

## Construct Figure 1 with example data

a0 <- 1
b1 <- 2
b2 <- 10
P <- 24

T <- seq(0, 24, by =0.1)
y <- a0 + b1*cos(2*(pi/P)*(T - b2)) 

g <- ggplot()                   
g <- g + geom_line(aes(x=T, y=y)) 
g <- g + labs(x = "Time points", y = "Dependent variable ")
g <- g + theme(axis.text = element_text(size = 10, colour="black"))
g <- g + scale_x_continuous(breaks = seq(0,24,2)) 
g <- g + geom_line(aes(x=b2, y=seq(1,3,0.1)), lty="dashed") 
g <- g + geom_line(aes(x=seq(0,24,0.1), y=a0)) 
g <- g + annotate("text", x = 1, y = 1.2, label = "b0")
g <- g + annotate("text", x = 10.8, y = 2, label = "b1")
g <- g + annotate("text", x = 10, y = 0.8, label = "b2")
g

#### Preliminary STEPS   ####

data <- getData()
# getData(filename="/Users/peterverboon/Documents/Open Universiteit/Onderzoek/Project Cyclic models/SmokingLapse.sav");

# Select relevant variables and standardize them

dat1 <- data[,c("subjnr","beepnr", "dagnr", "NAc", "PAc","Stressc","Zintentie","rookgedrag")]
dat1$intention <- dat1$Zintentie
dat1$positiveAffect <- scale(dat1$PAc)
dat1$stress <- scale(dat1$Stressc)

## Count number of records per subject and remove subjects with less than 50 records 

dat1$count <- 1
dat2 <- aggregate(dat1[,c("count")],by=list(dat1$subjnr), FUN=sum, na.rm=T);
dat2$subjnr <- dat2$Group.1

dat1$count <- NULL
dat2$count <- dat4$x
dat2$x <- NULL
dat2$Group.1 <- NULL

a <- merge(dat1,dat2, by.x = "subjnr")

dat3 <- subset(a, a$count >= 50)
length(unique(dat3$subjnr))

## Compute variability of intention per subject and remove subjects with very small variation in intention

a <- summarise(group_by(dat3, subjnr),mean=mean(intention), sd=sd(intention))
a <- merge(dat3, a,  by.x = "subjnr")

dat3 <- subset(a, a$sd > .10)   
length(unique(dat3$subjnr))


## Aggregate over subjects

dat4 <- aggregate(dat3[,c("NAc", "positiveAffect","stress","intention")],by=list(dat3$beepnr,dat3$dagnr), FUN=mean, na.rm=F);    
dat4$dagnr <- dat4$Group.2
dat4$beepnr <- dat4$Group.1
dat4$Group.1 <- NULL
dat4$Group.2 <- NULL



######################    ANALYSES   #########################

### Step 1: plotting the raw data for three subjects and average (Figure 1)

pdat <- dat4                                         # averaged over all subjects
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


### Step 3: Fit extra term in the model with day as covariate in aggregated data 

a <- fitCyclic(dat4, form = "y ~ cvar + svar + dagnr", yvar = "intention", xvar="beepnr",ymin=-1.0, ymax=0.5, step = 0.25)

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



## Step 6: Analyze cyclic model with MLA and model comparison

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


fit0 <- lmer(yvar ~ 1 + (1  |subjnr),data = dat)                                                # null model

ICC <- (as.data.frame(VarCorr(fit0))[1,"vcov"]) / sum(as.data.frame(VarCorr(fit0))[,"vcov"])    # compute ICC

fit1 <- lmer(yvar ~ cvar + svar + (1 |subjnr),data = dat)                                       # dayly cyclic effect 
fit2 <- lmer(yvar ~ cvar + svar + (1 +  svar + cvar |subjnr),data = dat)                        # daily random cyclic effect 

fit3 <- lmer(yvar ~ cvar + svar + cvar2 + svar2 + (1 + svar + cvar |subjnr),data = dat)       # dayly and weekly cyclic effect 
fit4 <- lmer(yvar ~ cvar + svar + cvar2 + svar2 + (1 + svar + cvar + cvar2 + svar2 |subjnr),data = dat)       # dayly and weekly cyclic effect 

fit5 <- lmer(yvar ~ cvar + svar + cvar2 + svar2 + stress + (1 + svar + cvar + cvar2 + svar2 + stress |subjnr),data = dat)       # dayly and weekly cyclic effect 


fit <- fit5



summary(fit)

a0 <- fixef(fit)[1]
a1 <- fixef(fit)[2]
a2 <- fixef(fit)[3]
a3 <- fixef(fit)[4]
a4 <- fixef(fit)[5]
a5 <- fixef(fit)[6]


b <- c(a0,cycpar(a1,a2, P),cycpar(a3,a4, P))     ## convert to parameters for linear model
b

anova(fit5, fit4, fit3, fit2, fit1, fit0)                   ## model comparison (Table 1)


### End step 6



### Step 7: Compute additional fit values and make plot of most complex model for aggregated data (fit 4, model 5 in tutorial) 


dat3$fittedIntention <- predict(fit)
cor(dat3$intention, dat3$fittedIntention)

 sum( (dat3$intention - predict(fit)) **2) / sum(dat3$intention ** 2)    ## ratio residuals and DV 

pdat <- dat4

pdat$ypred <-  a0 + b[2]*cos(2*pi/P*(pdat$beepnr - b[3]))  + b[4]*cos(2*pi/P2*(pdat$dagnr - b[5]))
pdat$day <- as.factor(pdat$dagnr)
npoints <- dim(pdat)[1]
pdat$xall <- c(1:npoints)
ymin <- -0.5; ymax <- 0.5; step <- 0.1

p <- ggplot(pdat) + geom_point(aes(x=pdat$xall, y=pdat$intention, colour=pdat$day))
p <- p + scale_x_discrete(name ="Time points (beeps within days)",  labels=pdat$beepnr, limits=c(1:npoints))
p <- p + labs(y = "intention")
p <- p + theme(axis.text = element_text(size = 6, colour="black"),legend.position="none")
p <- p + geom_line(aes(x=pdat$xall, y=pdat$ypred)) 
p <- p + coord_cartesian(ylim=c(ymin, ymax)) + scale_y_continuous(breaks=seq(ymin, ymax, step)) 
p

## End step 7


