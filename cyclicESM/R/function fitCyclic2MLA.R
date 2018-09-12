
##
##   This function fits the cyclic model, using a multilevel model. 
##   Two cycles are fitted.
##   
##   xvar1 = time variabele for first cyclic pattern
##   xvar2 = time variabele for second cyclic pattern
##   yvar = dependent variable
##   ymin, ymax and steps are parameters that control axes of the plot
##   form = contains a formule which can be extended with  additional variabeles
##   id = the clustering variable
##
##   See "authors" (2018). 
##   Analyzing cyclic patterns in psychological data_a tutorial
##
##   The function needs ggplot()


fitCyclic2MLA <- function(dat, form = y ~ cvar + svar + cvar2 + svar2 + (cvar + svar + cvar2 + svar2 | id), 
                          yvar, xvar1, xvar2,id, 
                          ymin = -1.0, ymax = 1.0, step=0.25 )
  { 
  
  require(lme4)
  require(ggplot2)
 
  result <- list() 
   
  dat$y <- dat[,yvar]
  dat$id <- dat[,id]

  P <- max(dat[,xvar1])
  dat$cvar <- cos((2*pi/P)*dat[,xvar1])
  dat$svar <- sin((2*pi/P)*dat[,xvar1])

  P2 <- max(dat[,xvar2])
  dat$cvar2 <- cos((2*pi/P2)*dat[,xvar2])
  dat$svar2 <- sin((2*pi/P2)*dat[,xvar2]) 
  
  # fit cyclic model using MLA
  
  fit <- lmer(form,data = dat)      

summary(fit)

a3 <- NA ; a4 <- NA; a5 <- NA;

a0 <- fixef(fit)[1]
a1 <- fixef(fit)[2]
a2 <- fixef(fit)[3]
a3 <- fixef(fit)[4]
a4 <- fixef(fit)[5]
a5 <- fixef(fit)[6]

b <- c(a0,cycpar(a1,a2, P),cycpar(a3,a4, P2),a5)     ## convert to parameters for linear model
names(b) <- c("Intercept", "daily amplitude", "daily phase" ,  "weekly amplitude" , "weekly phase","predictor" )


## Aggregate over subjects

datm <- aggregate(dat$y,by=list(dat[,xvar1],dat[,xvar2]), FUN=mean, na.rm=F);    
datm$xvar2 <- datm$Group.2
datm$xvar1 <- datm$Group.1
datm$y <- datm[,3]

datm$ypred <-  a0 + b[2]*cos(2*pi/P*(datm$xvar1 - b[3]))  + b[4]*cos(2*pi/P2*(datm$xvar2 - b[5]))
datm$day <- as.factor(datm$xvar2)
npoints <- dim(datm)[1]
datm$xall <- c(1:npoints)

p <- ggplot(datm) + geom_point(aes(x=datm$xall, y=datm$y, colour=datm$day))
p <- p + scale_x_discrete(name ="Time points (beeps within days)",  labels=datm$xvar1, limits=c(1:npoints))
p <- p + labs(y = yvar)
p <- p + theme(axis.text = element_text(size = 6, colour="black"),legend.position="none")
p <- p + geom_line(aes(x=datm$xall, y=datm$ypred)) 
p <- p + coord_cartesian(ylim=c(ymin, ymax)) + scale_y_continuous(breaks=seq(ymin, ymax, step)) 
p

result$plot <- p
result$fit <- fit
result$parameters <- b

return(result)

 }  ## End function



