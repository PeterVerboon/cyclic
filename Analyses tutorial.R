
## DATASET 2  CATHERINE  ##

data <-read.spss("Boonen_kort.sav", use.value.labels=F, to.data.frame=TRUE)

dat1 <- data[,c("subjnr","beepnr", "dagnr", "NAc", "PAc","Stressc","Zintentie","rookgedrag")]
dat1$intention <- dat1$Zintentie
dat1$positiveAffect <- dat1$PAc


## Count number of record per subject

dat1$count <- 1
dat4 <- aggregate(dat1[,c("count")],by=list(dat1$subjnr), FUN=sum, na.rm=T);
dat4$subjnr <- dat4$Group.1

dat1$count <- NULL
dat4$count <- dat4$x
dat4$x <- NULL
dat4$Group.1 <- NULL

a <- merge(dat1,dat4, by.x = "subjnr")

## select subject with 50 or more records 

dat3 <- subset(a, a$count >= 50)
length(unique(dat3$subjnr))


## aggregate over subjects

dat2 <- aggregate(dat3[,c("NAc", "positiveAffect","Stressc","intention")],by=list(dat3$beepnr,dat3$dagnr), FUN=mean, na.rm=F);    # aggregate across subjects
dat2$dagnr <- dat2$Group.2
dat2$beepnr <- dat2$Group.1
dat2$Group.1 <- NULL
dat2$Group.2 <- NULL



### plot raw data

pdat <- dat2   # averaged over all subjects

pdat <- subset(dat3, dat1$subjnr == 18)

npoints <- dim(pdat)[1]
x <- c(1:npoints)
pdat$day <- as.factor(pdat$dagnr)

g <- ggplot(pdat)
g <- g + geom_point(aes(x=x, y=Zintentie, colour=day))
g <- g + scale_x_discrete(name ="Time points (beeps within days)",  labels=pdat$beepnr, limits=c(1:npoints))
g <- g + theme(axis.text = element_text(size = 6, colour="black"),legend.position="none")
g



## Analyze cyclic model and plot

a <- fitCyclic(pdat, yvar = "Zintentie", xvar="beepnr")

a$rawDataPlot
a$meansPlot
a$parameters
a$fit



## Analyze cyclic model with MLA and plot

require(dplyr)

a <- summarise(group_by(dat3, subjnr),mean=mean(intention), sd=sd(intention))
a <- merge(dat3, a, by.x = "subjnr")

dat3 <- subset(dat3, a$sd > .10)   # remove subject with very small variation in DV
length(unique(dat3$subjnr))
