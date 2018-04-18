

##
##   This function fits the cyclic model, using a multilevel model. 
##   One cycles is fitted.
##   
##   xvar = time variabele 
##   yvar = dependent variable
##   ymin, ymax and steps are parameters that control axes of the plot
##   form = contains a formule which can be extended with  additional variabeles
##   id = the clustering variable
##
##   See Verboon & Leontjevas (2018). 
##   Analyzing cyclic patterns in psychological data_a tutorial
##
##   The function needs ggplot()


fitCyclicMLA <- function(dat, form = y ~ cvar + svar + (cvar + svar | id), 
                          yvar, xvar1=NULL,xvar2,id,ymin = -1.0, ymax = 1.0, step=0.25 )
{ 
  
  require(lme4)
  require(ggplot2)
  
  result <- list() 
  
  dat$y <- dat[,yvar]
  dat$id <- dat[,id]
  
  if (is.null(xvar1)) {result$fit <- lmer(form,data = dat); return(result)    }
  
  P <- max(dat[,xvar1])
  dat$cvar <- cos((2*pi/P)*dat[,xvar1])
  dat$svar <- sin((2*pi/P)*dat[,xvar1])
  
  # fit cyclic model using MLA
  
  fit <- lmer(form,data = dat)      
  
  summary(fit)
  
  a1<- NA; a2 <- NA; a3 <- NA ; 
  
  a0 <- fixef(fit)[1]
  a1 <- fixef(fit)[2]
  a2 <- fixef(fit)[3]
  a3 <- fixef(fit)[4]

  b <- c(a0,cycpar(a1,a2, P),a3)     ## convert to parameters for linear model
  names(b) <- c("Intercept", " amplitude", " phase" ,  "predictor")
  
  ## Aggregate over subjects
  
  datm <- aggregate(dat$y,by=list(dat[,xvar1],dat[,xvar2]), FUN=mean, na.rm=F);    
  datm$xvar2 <- datm$Group.2
  datm$xvar1 <- datm$Group.1
  datm$y <- datm[,3]
  
  datm$ypred <-  b[1] + b[2]*cos(2*pi/P*(datm$xvar1 - b[3]))
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


# test

# model <- fitCyclicMLA(dat=dat3, form = y ~ cvar + svar + (cvar + svar | id), 
#                         yvar="intention", xvar1="beepnr",xvar2="dagnr", id = "subjnr", 
#                         ymin = -0.5, ymax = 0.5, step=0.10 )
# 
# 
# model$plot
# model$parameters
# model$fit
