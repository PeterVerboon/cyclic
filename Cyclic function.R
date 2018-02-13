## Sigmoidal function curves

require(minpack.lm)     # contains function nlsLM for nonlinear fitting
require(ggplot2)
require(dplyr)
require(userfriendlyscience)
require(lme4);   # for lmer

options(digits=4);

# parameters
# b0 = mean of DV
# b1 = amplitude
# b2 = phase
# P  = periodicity
# t  = time indicator

# Define formula for cyclic model

CyclicF <- "y ~ b0 + b1*cos((2*pi/P)*t - b2)"    

CyclicF2 <- "y ~ b0 + b1*cos((2*pi/P)*(t - b2))"    


####  Data construction from cyclic model

makeCyclic <- function(t, b0 = 1, b1 = 5, b2=0.5, P = 7) {
  
   y <- b0 + b1*cos((2*pi/P)*(t - b2))
   
  return(y)
  
}    # end function

t <- rep(c(0:6),7)
y <- makeCyclic(t, b0=-1, b1=-5, b2=-1)
dat <- as.data.frame(cbind(t,y))
e <- 0.2
dat$y <- sqrt(1-e)*dat$y + sqrt(e)*(rnorm(length(t),0,sd(dat$y)))

mean(dat$y)
max(dat$y)
min(dat$y)
plot(dat$t,dat$y)


# optimizing function

out <- nlsLM(CyclicF, data=dat, start=list(b0 = 1, b1 = 1, b2 = 3, P = 7),
                                lower= c(-5,-6,-10, 7),
                                upper= c( 5, 6, 10 ,7))


plot(predict(out,dat), type="o")
plot(resid(out))
deviance(out)
summary(out)


# Linearize the cyclic model aND using MLA


P <- 7
dat$subj <- sort(rep(seq(1:7),7))

dat$cvar <- cos((2*pi/P)*dat$t)
dat$svar <- sin((2*pi/P)*dat$t)


fit <- lmer(y ~ cvar + svar  + (1 |subj), data = dat)     
summary(fit)

a0 <- fixef(fit)[1]
a1 <- fixef(fit)[2]
a2 <- fixef(fit)[3]

b <- c(a0, cycpar(a1,a2))
b


