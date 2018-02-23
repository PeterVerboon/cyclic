
fitCyclic2MLA <- function(dat, form = y ~ cvar + svar + cvar2 + svar2 + (cvar + svar + cvar2 + svar2 | id), yvar, xvar1, xvar2,id, ymin = -1.0, ymax = 1.0, step=0.25 )
  { 
  
  require(lme4)
 
  result <- list() 
  
  dat$y <- dat[,yvar]
  dat$id <- dat[,id]

  P <- max(dat[,xvar])
  dat$cvar <- cos((2*pi/P)*dat[,xvar])
  dat$svar <- sin((2*pi/P)*dat[,xvar])
  
  P2 <- max(dat[,xvar2])
  dat$cvar2 <- cos((2*pi/P2)*dat[,xvar2])
  dat$svar2 <- sin((2*pi/P2)*dat[,xvar2])
  
  # fit cyclic model using MLA
  
  fit <- lmer(form,data = dat)      

summary(fit)

a3 <- 0 ; a4 <- 0; a5 <- 0;

a0 <- fixef(fit)[1]
a1 <- fixef(fit)[2]
a2 <- fixef(fit)[3]
a3 <- fixef(fit)[4]
a4 <- fixef(fit)[5]
a5 <- fixef(fit)[6]

b <- c(a0,cycpar(a1,a2, P),cycpar(a3,a4, P2))     ## convert to parameters for linear model
names(b) <- c("Intercept", "dayly amplitude", "dayly phase" ,  "weekly amplitude" , "weekly phase" )

return(b)

}

fitCyclic2MLA(dat=dat3, form = y ~ cvar + svar + cvar2 + svar2 + (cvar + svar + cvar2 + svar2 | id), 
                   yvar="intention", xvar1="beepnr", xvar2="dagnr",id = "subjnr", ymin = -1.0, ymax = 1.0, step=0.25 )




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

 }  ## End function
