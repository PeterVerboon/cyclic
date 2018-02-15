

setwd("~/Onderzoek/Methodologie/Cyclic Models")

setwd("~/Documents/Open Universiteit/Onderzoek/Project Cyclic models")
getwd()

options(digits=3);

# ** artikel Flury & Levri, 1999 **

require(userfriendlyscience)
require(foreign)
require(lme4);   # for lmer
require(gtools);  # for invalid function

data <-read.spss("Werkbestand Jacques.sav", use.value.labels=F, to.data.frame=TRUE)
data <-read.spss("Boonen_kort.sav", use.value.labels=F, to.data.frame=TRUE)
data <-read.spss("BT2014s.sav", use.value.labels=F, to.data.frame=TRUE)

data <- getData()

attach(data)



## DATASET 2  CATHERINE  ##

dat1 <- data[,c("subjnr","beepnr", "dagnr", "NAc", "PAc","Stressc","Zintentie","rookgedrag")]
dat2 <- aggregate(dat1[,c("NAc", "PAc","Stressc","Zintentie")],by=list(subjnr,beepnr), FUN=mean, na.rm=F);    # aggregate across days
dat4 <- aggregate(dat1[,c("NAc", "PAc","Stressc","Zintentie")],by=list(subjnr,dagnr), FUN=mean, na.rm=T);

dat <- dat1

attach(dat)

dat$yvar <- dat$Zintentie
dat$xvar <- dat$beepnr 




##### END DATASETS


##  ****  cyclic analysis  ***

##   give the period (P) 



dat$cvar <- cos((2*pi/P)*dat$xvar)
dat$svar <- sin((2*pi/P)*dat$xvar)
P <- max(dat$xvar)

# Fit the three models and the parsimonious combi model with svar fixed

fitl <- lmer(yvar ~ xvar + (1 + xvar|subjnr), data = dat);                        # linear effect of beeps
fitc <- lmer(yvar ~ cvar + svar + (1 + cvar |subjnr),data = dat)                  # cyclic effect of beeps
fitb <- lmer(yvar ~ cvar + svar + xvar +  (1 + cvar + svar|subjnr), data = dat);  # cyclcic + linear


fitb1 <- lmer(yvar ~ cvar + svar + xvar + (1 + cvar + xvar|subjnr), data = dat);      # cyclcic + linear with svar fixed
fitb1 <- lmer(yvar ~ cvar + svar + xvar + (1 |subjnr), data = dat);                   # cyclcic + linear with all fixed

fitb2 <- lmer(yvar ~ cvar + svar  + Stressc + (1  |subjnr), data = dat);                  # cyclcic  with all fixed

fit3 <-  glmer(rookgedrag ~ cvar + svar + xvar + (1 |subjnr), family=binomial(link="logit"), data=dat);  # dummy as dependent

fit0 <-  glmer(rookgedrag ~  xvar + (1 |subjnr), family=binomial(link="logit"), data=dat);  # dummy as dependent

summary(fitc)
summary(fitl)
summary(fitb)


# compare models 

anova(fitb, fitc)

anova(fitb, fitl)


anova(fitb1, fitb)

fit <- fitl
fit <- fitc
fit <- fitb

# Find the cyclic parameters 



a0 <- fixef(fit)[1]
a1 <- fixef(fit)[2]
a2 <- fixef(fit)[3]
b3 <- fixef(fit)[4]


par <- cycpar(a1,a2)

par

b <- c(a0,par,b3)
b


##
# Select individuals


pdat <- subset(dat1, dat1$subjnr==2)
 
plot(pdat$Zintentie, xaxt = "n",yaxt = "n",xlab = "Beep number", ylab="Intention not to smoke"); 
   axis(1, at=1:dim(pdat)[1], labels=pdat$beepnr, cex.axis=0.5)
   axis(2,cex.axis=0.8)
plot(pdat$beepnr,pdat$Zintentie)

pdat$yvar <- pdat$Zintentie
pdat$xvar <- pdat$beepnr
pdat$cvar <- cos((2*pi/P)*pdat$xvar)
pdat$svar <- sin((2*pi/P)*pdat$xvar)
P <- max(pdat$xvar)
fitp <- lm( yvar ~ cvar + svar , data = pdat)
summary(fitp)
a0 <- fitp$coefficients[1]
a1 <- fitp$coefficients[2]
a2 <- fitp$coefficients[3]

par <- cycpar(a1,a2)

par

b <- c(a0,par,b3)
b

pdat2 <- aggregate(pdat[,c("NAc", "PAc","Stressc","Zintentie","yvar")],by=list(pdat$beepnr), FUN=mean, na.rm=F);   
pdat2$xvar <- pdat2$Group.1

