
require(gtools);  # for invalid function
require(ggplot2)

# plot the predicted values of the model

# Parameters b1 and b2 are obtained from cyclic model analysis

a0 <- b[1]
b1 <- b[2]
b2 <- b[3]

dat <- pdat
dat <- pdat2

# x <- seq(0, 12,by=0.1)                       # construct time axis

x <- dat$xvar
ypred <-  a0 + b1*cos(2*pi/P*(x - b2))           # cyclic only, without linear trend

ypred <-  a0 + b1*cos(2*pi/P*(x - b2)) + b3*x    # cyclic model with linear trend
ylin <- a0 + b3*x                                # linear trend


# Construct plot 

g <- ggplot(dat)                   
g <- g + geom_point(aes(x=x,y=yvar))
g <- g +geom_hline(yintercept=a0, colour="blue")  
g <- g +geom_vline(xintercept=(b2), colour="red") 
g <- g + geom_line(aes(x=x, y=ypred)) 
# g <- g + geom_line(aes(x=x, y=ylin), lty = "dashed")   # show linear trend line
g <- g + labs(x = "Time points", y = "Intention to stop smoking")
g <- g + scale_x_discrete(name ="Time points",  limits=c(1:P))
g <- g + theme(axis.text = element_text(size = 12, colour="black"))
g


## If dependent variable (DV) is dichotomous 

y <- exp(y)/(1+exp(y))                     # from log odds to probabilities


# construct plot of probabilities

g <- ggplot()                   
g <- g + geom_point(aes(x=x,y=y))
g <- g +geom_hline(yintercept=a0, colour="blue")  
g <- g +geom_vline(xintercept=(b2), colour="red") 

g <- g + geom_line(aes(x=x, y=ypred)) 
# g <- g + geom_line(aes(x=x, y=ylin), lty = "dashed")  # show linear trend line
g <- g + labs(x = "Tine Points", y = "Probability of DV")
# g <- g + ylim(0.2, 0.8)
g <- g + theme(axis.text = element_text(size = 12, colour="black"))
g


####

x <- as.numeric(dat2[,"Group.2"])
x <- as.numeric(dat[,"beepnr"])
y <- as.numeric(dat2[,"PAc"])

g <- ggplot(dat, aes(xvar,y))   # plot gefitte waarden
g <- g + stat_smooth(span=1.0)


g <- ggplot(dat, aes(xvar,yvar))   # plot data
g <- g + stat_smooth(span=1.0)
g <- g + geom_line(aes(x=xvar, y=y, width = 6)) 


g 


### plot raw data

pdat <- dat2
pdat <- subset(dat1, dat1$subjnr==2)
npoints <- dim(pdat)[1]
x <- c(1:npoints)
pdat$day <- as.factor(pdat$dagnr)

g <- ggplot(pdat)
g <- g + geom_point(aes(x=x, y=Zintentie, colour=day))
g <- g + scale_x_discrete(name ="Time points",  labels=pdat$beepnr, limits=c(1:npoints))
g <- g + theme(axis.text = element_text(size = 6, colour="black"),legend.position="none")

g



