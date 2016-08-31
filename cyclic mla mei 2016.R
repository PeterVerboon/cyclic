

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

## DATASET 1  JACQUES  ##

dat1 <- data[,c("subjnr","beepnr", "dagnr", "SAold", "SApart","INTIM", "SEXDES")]
dat1 <- dat1[order(subjnr, dagnr, beepnr),]

dat2 <- aggregate(dat1[,c("SAold","SApart","INTIM","SEXDES")],by=list(subjnr,beepnr), FUN=sum, na.rm=T);
dat3 <- aggregate(dat1[,c("SAold","SApart","INTIM","SEXDES")],by=list(beepnr), FUN=sum, na.rm=T);
dat4 <- aggregate(dat1[,c("SAold","SApart","INTIM","SEXDES")],by=list(dagnr,subjnr), FUN=sum, na.rm=T);
dat5 <- aggregate(dat1[,c("SAold","SApart","INTIM","SEXDES")],by=list(dagnr), FUN=mean, na.rm=T);

dat4[(dat4[,"SAold"] > 1), "SAold"] <- 1
dat4[(dat4[,"SApart"] > 1), "SApart"] <- 1

dat <- dat1[dat1[,"beepnr"] != 11,]       # remove all evening and morning questionnaires

dat <- dat[!is.na(dat[,"INTIM"]),]
dat <- dat[dat[,"INTIM"] < 12,]
dat$INTIM <- scale(dat$INTIM, center=TRUE, scale=FALSE)

dat <- dat[!is.na(dat[,"SEXDES"]),]
#dat <- dat[dat[,"SEXDES"] < 5,]
#dat$SEXDES <- scale(dat$SEXDES, center=TRUE, scale=FALSE)
#hist(dat[,"SEXDES"], xlab = ("Sexual Desire"), main = (" "), col = "lightblue")
g=qplot(dat[,"SEXDES"], data=dat, geom="histogram", binwidth = 1) 
g <- g + labs(x = "Sexual Desire score", y = "Frequency", title = " ")
g <- g + ylim(0, 5000) + xlim(1,8) 
g <- g + theme(axis.text = element_text(size = 12, colour="black"))
g
ExportPlot(g,"plot_histogram",width = 5, height = 3)

boxplot(dat[,"SEXDES"])
dat$SEXDES2 <- cut(dat[,"SEXDES"],breaks = c(0,1,10) )
dat$SDDUM <- as.numeric(dat$SEXDES2) - 1


attach(dat)

dat$yvar <- dat$INTIM
dat$yvar <- dat$SDDUM
dat$xvar <- dat$beepnr
dat$subj <- dat$subjnr


## DATASET 2  CATHERINE  ##

dat1 <- data[,c("subjnr","beepnr", "dagnr", "NAc", "PAc","Stressc","Zintentie")]
dat2 <- aggregate(dat1[,c("NAc", "PAc","Stressc","Zintentie")],by=list(subjnr,beepnr), FUN=mean, na.rm=F);    # aggregate across days
dat4 <- aggregate(dat1[,c("NAc", "PAc","Stressc","Zintentie")],by=list(subjnr,dagnr), FUN=mean, na.rm=T);

dat <- dat1

detach(data)
attach(dat)

dat$yvar <- dat$Zintentie
dat$xvar <- dat$beepnr 
dat$subj <- dat$subjnr


## DATASET 3  Bachelor Thesis news  ##

dat1 <- data[,c("Nickname", "Weekday", "Time", "Hour","ThreeHour","PANPOS","PANNEG", "Depres", "Angst")]
dat2 <- aggregate(dat1[,c("PANPOS","PANNEG", "Depres", "Angst")],by=list(Nickname,Weekday), FUN=mean, na.rm=T);
dat4 <- aggregate(dat1[,c("PANPOS","PANNEG", "Depres", "Angst")],by=list(Nickname,Time), FUN=mean, na.rm=T);

dat2$PAf <- scale(dat2$PANPOS, center=T, scale=T)
dat2$NAf <- scale(dat2$PANNEG, center=T, scale=T)
dat2$Dep <- scale(dat2$Depres, center=T, scale=T)
dat2$Fear <- scale(dat2$Angst, center=T, scale=T)


dat <- dat2

detach(data)
attach(dat)

dat$yvar <- dat$Dep
dat$xvar <- dat$Group.2 
dat$subj <- dat$Group.1

##### END DATASETS


##  ****  cyclic analysis  ***

##   give the period (P) 

P <- 10

dat$cvar <- cos((2*pi/P)*dat$xvar)
dat$svar <- sin((2*pi/P)*dat$xvar)


# Fit the three models and the parsimonious combi model with svar fixed

fitl <- lmer(yvar ~ xvar + (1 + xvar|subj), data = dat);                               # linear effect of beeps
fitc <- lmer(yvar ~ cvar + svar + (1 + cvar |subj),data = dat)                  # cyclic effect of beeps
fitb <- lmer(yvar ~ cvar + svar + xvar +  (1 + cvar + xvar|subj), data = dat);         # cyclcic + linear


fitb1 <- lmer(yvar ~ cvar + svar + xvar + (1 + cvar + xvar|subj), data = dat);      # cyclcic + linear with svar fixed
fitb1 <- lmer(yvar ~ cvar + svar + xvar + (1 |subj), data = dat);                   # cyclcic + linear with all fixed

fitb2 <- lmer(yvar ~ cvar + svar  + Stressc + (1  |subj), data = dat);                  # cyclcic  with all fixed

fit3 <-  glmer(yvar ~ cvar + svar + xvar + (1 |subj), family=binomial(link="logit"), data=dat);  # dummy as dependent

fit0 <-  glmer(yvar ~  xvar + (1 |subj), family=binomial(link="logit"), data=dat);  # dummy as dependent

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

b1 <- par[1]
b2 <- par[2]

