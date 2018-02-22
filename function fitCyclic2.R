
fitCyclic2 <- function(dat, form = y ~ cvar + svar + cvar2 + svar2,
                       yvar, xvar1, xvar2, ymin = -1.0, ymax = 1.0, step=0.25 ) {  
  
  result <- list() 
  
  dat$y <- dat[,yvar]
  
  P <- max(dat[,xvar])
  dat$cvar <- cos((2*pi/P)*dat[,xvar])
  dat$svar <- sin((2*pi/P)*dat[,xvar])
  
  P2 <- max(dat[,xvar2])
  dat$cvar2 <- cos((2*pi/P2)*dat[,xvar2])
  dat$svar2 <- sin((2*pi/P2)*dat[,xvar2])
  
  
  # fit cyclic model within days across beeps
  
  fitp <- lm(form, data=dat)
  
  a0 <- fitp$coefficients[1]
  a1 <- fitp$coefficients[2]
  a2 <- fitp$coefficients[3] 
  a3 <- fitp$coefficients[4] 
  a4 <- fitp$coefficients[5] 
  
  b <- c(a0,cycpar(a1,a2, P),cycpar(a3,a4, P))     ## convert to parameters for linear model
  names(b) <- c("Intercept", "dayly amplitude", "dayly phase" ,  "weekly amplitude" , "weekly phase" )
  
  # Parameters b1 to b5 are obtained from cyclic model analysis
  
  dat$ypred <-  b[1] + b[2]*cos(2*pi/P*(dat[,xvar1] - b[3]))  + b[4]*cos(2*pi/P2*(dat[,xvar2] - b[5]))
  
  
  npoints <- dim(dat)[1]
  dat$xall <- c(1:npoints)
  dat$day <- as.factor(dat$dagnr)
  
  # raw data plot
  
  g0 <- ggplot(dat) + geom_point(aes(x=xall, y=dat$y, colour=dat$day))
  g0 <- g0 + scale_x_discrete(name ="Time points (beeps within days)",  labels=dat[,xvar1], limits=c(1:npoints))
  g0 <- g0 + labs(y = yvar)
  g0 <- g0 + theme(axis.text = element_text(size = 6, colour="black"),legend.position="none")
  g0 <- g0 + geom_line(aes(x=dat$xall, y=dat$ypred)) 
  g0 <- g0 + coord_cartesian(ylim=c(ymin, ymax)) + scale_y_continuous(breaks=seq(ymin, ymax, step)) 
  g0
  

  result$rawDataPlot <- g0
  result$fit  <- fitp
  result$parameters <- b
  
  return(result)
  
}     # end function


## test


pdat <- subset(dat3, dat3$subjnr==15)

a <- fitCyclic2(pdat,form= "y ~ cvar + svar + cvar2 + svar2", yvar = "intention", xvar1="beepnr", xvar2 = "dagnr" , 
                     ymin=-0.5, ymax = 0.5, step=0.1)

a$rawDataPlot
a$parameters
summary(a$fit)


