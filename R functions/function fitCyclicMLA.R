

fitCyclicMLA <- function(dat, yvar, xvar1 = NULL, xvar2 = NULL, id, ncycle = 1, 
                         random = "intercept", cov = NULL, 
                         ymin = -1.0, ymax = 1.0, step=0.25 )
{

  result <- list()
  
  if (sum(c(xvar1, xvar2, yvar, id) %in% colnames(dat3)) < 4) { stop("Not all specified variables exist in data frame")}
  
  
  if (!ncycle == 1 & is.null(xvar2)) {stop("Two cycles requested, but no day variable is given") }
  
  dat$y <- dat[,yvar]
  dat$id <- dat[,id]
  ifelse (is.null(xvar2), dat$day <- 1  , dat$day <- as.factor(dat[,xvar2]))
  
  
  # Null model
  if (is.null(xvar1)) {
    form <- "y ~ 1 + (1 | id)"
    result$fit <- lmer(form,data = dat); return(result)    
    }

  if (ncycle == 1 & random == "second") {cat("Number of cycles is set to 2");  ncycle <- 2 }
  
  ifelse (ncycle == 1, fixedPart <- paste0("y ~ cvar + svar"), 
                       fixedPart <- paste0("y ~ cvar + svar + cvar2 + svar2") )
  
  covtext <- paste0(cov,  collapse = " + ")
  if (!is.null(cov)) { fixedPart <- paste0(fixedPart," + ", covtext) }
  
  if (random == "intercept") { randomPart <- "(1 | id)" }
  if (random == "first")     { randomPart <- "(cvar + svar | id)"  }
  if (random == "second")    { randomPart <- "(cvar2 + svar2 | id)"  }
  if (random == "cov")       { randomPart <- paste0("( ", cov, " | id)")   }
  if (random == "all" & ncycle == 1)  { randomPart <- paste0("(cvar + svar + ", covtext, "| id)")  }
  if (random == "all" & ncycle == 2)  { randomPart <- paste0("(cvar + svar + cvar2 + svar2 + ", covtext, "| id)")  }
 
  form <- paste0(fixedPart," + ", randomPart)
  
  
  
  P <- max(dat[,xvar1])
  dat$cvar <- cos((2*pi/P)*dat[,xvar1])
  dat$svar <- sin((2*pi/P)*dat[,xvar1])

  if (!ncycle == 1) {
      P2 <- max(dat[,xvar2])
      dat$cvar2 <- cos((2*pi/P2)*dat[,xvar2])
      dat$svar2 <- sin((2*pi/P2)*dat[,xvar2]) 
  }
  
  # fit cyclic model using MLA

  print(form)
  
  fit <- lmer(form,data = dat)


  a0 <- fixef(fit)[1]
  a1 <- fixef(fit)[2]
  a2 <- fixef(fit)[1:3]
  if (ncycle == 1 & !is.null(cov)) a.cov <- fixef(fit)[4:(3+length(cov))]
  if (!ncycle == 1) { 
    a3 <- fixef(fit)[4] ;
    a4 <- fixef(fit)[5] ;
    if (!is.null(cov)) a.cov <- fixef(fit)[6:(5+length(cov))]
  }


  ## convert to parameters for linear model
  if (ncycle == 1) { 
    b <- c(a0,cycpar(a1,a2, P),a.cov)     
    names(b) <- c("Intercep", " amplitude", " phase" ,  cov)
  }
  
  if (!ncycle == 1) {
  b <- c(a0,cycpar(a1,a2, P),cycpar(a3,a4, P2),a.cov)     ## convert to parameters for linear model
  names(b) <- c("  Intercept  ", "amplitude short cycle", "sort cycle phase",  "amplitude long cycle" , "long cycle phase",cov)
  }

  ## Aggregate over subjects

  datm <- aggregate(dat$y,by=list(dat[,xvar1],dat$day), FUN=mean, na.rm=F);
  datm$xvar2 <- datm$Group.2
  datm$xvar1 <- datm$Group.1
  datm$y <- datm[,3]
  
  if (ncycle == 1) {
      if (!is.null(cov))  {
         datm$ypred <-  b[1] + b[2]*cos(2*pi/P*(datm$xvar1 - b[3])) +  as.matrix(dat[,cov]) %*% a.cov 
      }  else { datm$ypred <- b[1] + b[2]*cos(2*pi/P*(datm$xvar1 - b[3])) 
         }
  }
  
  if (!ncycle == 1) {
     if (!is.null(cov))  {
     datm$ypred <-  b[1] + b[2]*cos(2*pi/P*(datm$xvar1 - b[3])) + 
                           b[4]*cos(2*pi/P2*(datm$xvar2 - b[5])) + as.matrix(dat[,cov]) %*% a.cov
     } else { datm$ypred <- b[1] + b[2]*cos(2*pi/P*(datm$xvar1 - b[3])) + 
                                   b[4]*cos(2*pi/P2*(datm$xvar2 - b[5]))
       }
  }
  
  
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

model <- fitCyclicMLA(dat=dat3,  yvar="intention", xvar1="beepnr",xvar2="daynr", id = "subjnr",
                      random = "intercept",  ncycle = 1, cov = c("stress","positiveAffect"),
                      ymin = -0.5, ymax = 0.5, step=0.10 )
#
#
# model$plot
# model$parameters
# model$fit
