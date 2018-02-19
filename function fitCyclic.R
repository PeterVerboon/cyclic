

fitCyclic <- function(dat, yvar, xvar, ymin = -1.0, ymax = 1.0 ) {  
  
    result <- list() 
  
    dat$cvar <- cos((2*pi/P)*dat[,xvar])
    dat$svar <- sin((2*pi/P)*dat[,xvar])
    dat$y <- dat[,yvar]
    dat$x <- dat[,xvar]
    P <- max(dat[,xvar])
    
    fitp <- lm(y ~ cvar + svar , data = dat)

    a0 <- fitp$coefficients[1]
    a1 <- fitp$coefficients[2]
    a2 <- fitp$coefficients[3] 

    par <- cycpar(a1,a2) 

    b <- c(a0,par)
    
    # Parameters b1 and b2 are obtained from cyclic model analysis
    
    b1 <- b[2]
    b2 <- b[3] 
    
    ypred <-  a0 + b1*cos(2*pi/P*(dat$x - b2))   
    
    g <- ggplot(dat)                   
    g <- g + geom_point(aes(x=dat$x,y=dat$y))
    g <- g + geom_hline(yintercept=a0, colour="blue")  
    g <- g + geom_vline(xintercept=b2, colour="red") 
    g <- g + geom_line(aes(x=dat$x, y=ypred)) 
    g <- g + labs(x = "Time points", y = yvar)
    g <- g + scale_x_discrete(name ="Time points",  limits=c(1:P))
    g <- g + ylim(ymin, ymax)
    g <- g + theme(axis.text = element_text(size = 12, colour="black"))
  
  
      pdat2 <- aggregate(dat[,yvar],by=list(dat[,xvar]), FUN=mean, na.rm=F)   
      pdat2$y <- pdat2$x
      pdat2$x <- pdat2$Group.1
     
    ypred2 <-  a0 + b1*cos(2*pi/P*(pdat2$x - b2))   
    
    g1 <- ggplot(pdat2)
    g1 <- g1 + geom_point(aes(x=pdat2$x,y=pdat2$y))
    g1 <- g1 + geom_hline(yintercept=a0, colour="blue")
    g1 <- g1 + geom_vline(xintercept=b2, colour="red")
    g1 <- g1 + geom_line(aes(x=pdat2$x, y=ypred2))
    g1 <- g1 + labs(x = "Time points", y = yvar)
    g1 <- g1 + scale_x_discrete(name ="Time points",  limits=c(1:P))
    g1 <- g1 + ylim(ymin, ymax)
    g1 <- g1 + theme(axis.text = element_text(size = 12, colour="black"))

    result$meansPlot <- g1
    
    result$rawDataPlot <- g
    result$fit  <- fitp
    result$parameters <- b
    
    return(result)

}     # end function


## test


pdat <- subset(dat1, dat1$subjnr==50)

a <- fitCyclic(pdat, yvar = "Zintentie", xvar="beepnr",ymin=-0.5, ymax=0.5)

a$rawDataPlot
a$meansPlot
a$parameters
a$fit



