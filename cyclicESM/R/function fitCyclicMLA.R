
#' Fits the cyclic model, using a simple linear model.

#'
#' This function fits the cyclic model, using a simple linear model (see Verboon & Leontjevas, 2018)
#' @param xvar time variabele
#' @param yvar dependent variable
#' @param P is the periodicity of the cycle
#' @param ymin,ymax,step parameters that control axes of the plot
#' @param form contains a formule which can be extended with  additional variabeles
#' @keywords cyclic model ESM
#' @return list containing the following elements:
#' @return parameters =      amplitude and phase
#' @return fit =             object from lm()
#' @return rawDataPlot =     plot of predicted values for all observations
#' @return meansPlot =       plot with predictions averaged over subjects,
#' @return oneCyclePlot =    plot with predictions for one cycle,
#' @import ggplot2 
#' @import lme4
#' @export
#' @examples
#' fitCyclicMLA(dat, yvar = dependVar, xvar1 = "beepnr", xvar2 =)
fitCyclicMLA <- function(dat, yvar, xvar1 = NULL, xvar2, id, ncycle = 1, random = "intercept",
                         ymin = -1.0, ymax = 1.0, step=0.25 )
{

  result <- list()

  dat$y <- dat[,yvar]
  dat$id <- dat[,id]
  
  # Null model
  if (is.null(xvar1)) {
    form <- "y ~ 1 + (1 | id)"
    result$fit <- lmer(form,data = dat); return(result)    
    }

  if (ncycle == 1 & random == "second") {cat("Number of cycles is set to 2");  ncycle <- 2 }
  
  ifelse (ncycle == 1, fixedPart <- paste0("y ~ cvar + svar + "), 
                       fixedPart <- paste0("y ~ cvar + svar + cvar2 + svar2 + ") )
  
  covtext <- paste0(cov, " + ", collapse = "")
  if (!is.null(cov)) { fixedPart <- paste0(fixedPart, covtext) }
  
  if (random == "intercept") { randomPart <- "(1 | id)" }
  if (random == "first")     { randomPart <- "(cvar + svar | id)"  }
  if (random == "second")    { randomPart <- "(cvar2 + svar2 | id)"  }
  if (random == "cov")       { randomPart <- paste0("( ", cov, " | id)")   }
  if (random == "all" & ncycle == 1)  { randomPart <- paste0("(cvar + svar + ", covtext, "1 | id)")  }
  if (random == "all" & ncycle == 2)  { randomPart <- paste0("(cvar + svar + cvar2 + svar2 + ", covtext, "1 | id)")  }
 
  form <- paste0(fixedPart, randomPart)
  
  
  
  
  P <- max(dat[,xvar1])
  dat$cvar <- cos((2*pi/P)*dat[,xvar1])
  dat$svar <- sin((2*pi/P)*dat[,xvar1])

  if (!ncycle == 1) {
      P2 <- max(dat[,xvar2])
      dat$cvar2 <- cos((2*pi/P2)*dat[,xvar2])
      dat$svar2 <- sin((2*pi/P2)*dat[,xvar2]) 
  }
  
  # fit cyclic model using MLA

  fit <- lmer(form,data = dat)

  summary(fit)


  a0 <- fixef(fit)[1]
  a1 <- fixef(fit)[2]
  a2 <- fixef(fit)[3]
  if (ncycle == 1 & !is.null(cov)) acov <- fixef(fit)[c(4:(3+length(cov))]
  if (!ncycle == 1) { 
    a3 <- fixef(fit)[4]
    a4 <- fixef(fit)[5] 
    if (!is.null(cov)) acov <- fixef(fit)[c(6:(5+length(cov))]
  }

  ## convert to parameters for linear model
  b <- c(a0,cycpar(a1,a2, P),acov)     
  names(b) <- c("Intercept", " amplitude", " phase" ,  "predictor")
  
  b <- c(a0,cycpar(a1,a2, P),cycpar(a3,a4, P2),acov)     ## convert to parameters for linear model
  names(b) <- c("Intercept", "daily amplitude", "daily phase" ,  "weekly amplitude" , "weekly phase","predictor" )
  

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
