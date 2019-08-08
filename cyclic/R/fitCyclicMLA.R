
#' Fits the cyclic model on ESM data.
#'
#' This function fits the cyclic model on ESM data, using a multilevel model (see Verboon & Leontjevas, 2018)
#' @param dat data frame containing variables
#' @param yvar dependent variable
#' @param xvar1 time variabele indicator of first or only cycle
#' @param xvar2 time variabele indicator of second (longer) cycle
#' @param id group (subjects, clusters) identifier
#' @param cov additional variabels in the model
#' @param ncycle is the number of cyclic processes in the model (1 = 1, otherwise = 2)
#' @param P is the periodicity of the first cyclic process
#' @param P2 is the periodicity of the second cyclic process
#' @param random keyword indicating which terms should be random effect in the model. The following keywords are available:
#' "intercept"= intercept only;
#' "first"= cyclic parameters of first cyclic process;
#' "second" = cyclic parameters of second cyclic process;
#' "cov" = covariates only, no cyclic terms; and finally
#' "all" = all terms
#' @keywords cyclic model ESM
#' @return list containing the following elements:
#' @return parameters =      intercept, amplitude and phase (and other terms in the model)
#' @return fit =             object from lmer()
#' @return formula =     specification of lmer model
#' @return period =      specified periodicity of cyclic process
#' @export
#' @examples
#'  data("smokedat")
#'   model <- fitCyclicMLA(dat=smokedat,  yvar="intention", xvar1="beepnr",
#'   xvar2="daynr", id = "subjnr", random = "all",
#'   ncycle = 1, cov = c("stress", "positiveAffect"),
#'   ymin = -0.5, ymax = 0.5, step=0.10)
fitCyclicMLA <- function(dat, yvar = NULL, xvar1 = NULL, xvar2 = NULL, id = NULL, cov = NULL,
                         ncycle = 1, P = NULL, P2 = NULL, random = "intercept")

{

  result <- list()
  result$input <- as.list(environment())
  result$intermediate$dat <- dat
  result$intermediate$ncycle <- ncycle


  # check basic input
  if (is.null(yvar)) { stop("parameter 'yvar' has not been specified")}
  if (is.null(id))  { stop("parameter 'id' has not been specified")}
  if (!is.null(id))  { if (!id %in% colnames(dat)) { stop(paste0("Variable '", id, "' does not exist"))}}
  if (!is.null(yvar))  { if (!yvar %in% colnames(dat)) { stop(paste0("Variable '", yvar, "' does not exist"))}}
  if (is.null(cov)) a.cov <- NULL

  dat$y <- dat[,yvar]
  dat$id <- dat[,id]

  # Null model
  if (is.null(xvar1) & is.null(cov)) {
    cat("The intercept only model was requested, since parameter 'xvar1' is empty and no covariates are specified")
    form <- "y ~ 1 + (1 | id)"
    result$fit <- lmerTest::lmer(form,data = dat)
    result$intermediate$ncycle <- NULL
    class(result)  <- "fitCyclicMLA"
    return(result)
  }

  # Covariates only model
  if (is.null(xvar1) & !is.null(cov)) {
    cat("The covariates only model was requested, since parameter 'xvar1' is empty")
    form <-   paste0("y ~ 1 + ",paste0(cov, collapse  = " + "),  " + (1 | id)")
    result$fit <- lmerTest::lmer(form,data = dat)
    result$intermediate$ncycle <- NULL
    class(result)  <- "fitCyclicMLA"
    return(result)
  }

  # check input and if possible adjust
  if (!xvar1 %in% colnames(dat)) { stop(paste0("Variable '", xvar1, "' does not exist"))}
  if (!is.null(xvar2)) { if (!is.null(xvar2) & !xvar2 %in% colnames(dat)) { stop(paste0("Variable '", xvar2, "' does not exist"))}}
  if (!ncycle == 1 & is.null(xvar2)) {stop("Two cyclic patterns were requested, but parameter 'xvar2' has not been specified") }
  if (ncycle == 1 & random == "second") {cat("Number of cycles is set to 2");  ncycle <- 2 }
  if (is.null(cov) & random == "cov") {cat("No covariates are specified. Random term is set to 'intercept'");  random <- "intercept" }

  ### If the time variable is actually provided as time instead of as
  ### indices/ranks, convert to numeric first.
  if (!is.numeric(dat[,xvar1])) {
    if (any(class(dat[,xvar1]) %in% c('Date', 'POSIXct', 'POSIXt', 'POSIXt', 'hms'))) {
      dat$h <- round((as.numeric(dat[,xvar1])/3600), digits=0)
      dat[,xvar1] <- dat$h - min(dat$h) + as.numeric(format(min(dat[,xvar1]), format="%H"))
    } else {
      cat("The time variable does not have a class numeric or date.","\n",
          "I am trying to create time variable. Check results carefully.","\n")
      dat$h <- round(as.POSIXct(dat[,xvar1], format="%H:%M:%S", tz="UTC"))
      dat$h <- as.numeric(format(dat$h, format="%H"))
      dat$h[dat$h == 0] <- max(dat$h) + 1
      dat[,xvar1] <- dat$h
    }
    result$intermediate$dat[,xvar1] <- dat[,xvar1]
  }

  if (!is.null(xvar2) & !is.numeric(dat[,xvar2])) {
    if (any(class(dat[,xvar2]) %in% c('Date', 'POSIXct', 'POSIXt', 'POSIXt', 'hms'))) {
      dat$h <- round((as.numeric(dat[,xvar2])/3600), digits=0)
      dat[,xvar2] <- dat$h - min(dat$h) + as.numeric(format(min(dat[,xvar2]), format="%H"))
    } else {
      cat("The time variable does not have a class numeric or date.","\n",
          "I am trying to create time variable. Check results carefully.","\n")
      dat$h <- round(as.POSIXct(dat[,xvar2], format="%H:%M:%S", tz="UTC"))
      dat$h <- as.numeric(format(dat$h, format="%H"))
      dat$h[dat$h == 0] <- max(dat$h) + 1
      dat[,xvar2] <- dat$h
    }
    result$intermediate$dat[,xvar2] <- dat[,xvar2]
  }



  # construct formula
  ifelse (ncycle == 1, fixedPart <- paste0("y ~ cvar + svar"),
          fixedPart <- paste0("y ~ cvar + svar + cvar2 + svar2") )

  covtext <- paste0(cov,  collapse = " + ")
  if (!is.null(cov)) { fixedPart <- paste0(fixedPart," + ", covtext) }

   ifelse (is.null(cov), plus <- "", plus <- " + ")

  if (random == "intercept") { randomPart <- "(1 | id)" }
  if (random == "first")     { randomPart <- "(cvar + svar | id)"  }
  if (random == "second")    { randomPart <- "(cvar2 + svar2 | id)"  }
  if (random == "cov")       { randomPart <- paste0("( ", covtext, " | id)")   }
  if (random == "all" & ncycle == 1)  { randomPart <- paste0("(cvar + svar ", plus , covtext, "| id)")  }
  if (random == "all" & ncycle == 2)  { randomPart <- paste0("(cvar + svar + cvar2 + svar2", plus, covtext, "| id)")  }

  form <- paste0(fixedPart," + ", randomPart)


  # compute cyclic terms

  if (is.null(P)) {P <- max(dat[,xvar1], na.rm = TRUE) - min(dat[,xvar1], na.rm = TRUE) + 1}
  dat$cvar <- cos((2*pi/P)*dat[,xvar1])
  dat$svar <- sin((2*pi/P)*dat[,xvar1])

  if (!ncycle == 1) {
    if (is.null(P2)) {P2 <- max(dat[,xvar2], na.rm = TRUE) - min(dat[,xvar2], na.rm = TRUE) + 1}
    dat$cvar2 <- cos((2*pi/P2)*dat[,xvar2])
    dat$svar2 <- sin((2*pi/P2)*dat[,xvar2])
  }

  xmin1 <- min(dat[,xvar1], na.rm = TRUE)
  xmax1 <- max(dat[,xvar1], na.rm = TRUE)
  range1 <- xmax1 - xmin1
  if (!ncycle == 1) {
    xmin2 <- min(dat[,xvar2], na.rm = TRUE)
    xmax2 <- max(dat[,xvar2], na.rm = TRUE)
    range2 <- xmax2 - xmin2
  }

  # fit cyclic model using MLA

  fit <- lmerTest::lmer(form,data = dat)

  a0 <- lme4::fixef(fit)[1]
  a1 <- lme4::fixef(fit)[2]
  a2 <- lme4::fixef(fit)[3]
  if (ncycle == 1 & !is.null(cov)) a.cov <- lme4::fixef(fit)[4:(3+length(cov))]
  if (!ncycle == 1) {
    a3 <- lme4::fixef(fit)[4] ;
    a4 <- lme4::fixef(fit)[5] ;
    if (!is.null(cov)) a.cov <- lme4::fixef(fit)[6:(5+length(cov))]
  }

  # convert to parameters from linear model
  if (ncycle == 1) {
    b <- c(a0,cycpar(a1,a2, P),a.cov)
 # let the maximum fall into the range of the data
    while (b[3] < xmin1) b[3] <- b[3] + range1
    names(b) <- c("intercept", "amplitude", "phase" ,  cov)
  }

  if (!ncycle == 1) {
    b <- c(a0,cycpar(a1,a2, P),cycpar(a3,a4, P2),a.cov)
 # let the maximum fall into the range of the data
    while (b[3] < xmin1) b[3] <- b[3] + range1
    while (b[5] < xmin2) b[5] <- b[5] + range2
    names(b) <- c("intercept  ", "short_cycle_amplitude", "short_cycle_phase",  "long_cycle_amplitude" , "long_cycle_phase",cov)
  }

  result$fit <- fit
  result$parameters <- b
  result$formule <- form
  result$period <- c(P, P2)

  class(result)  <- "fitCyclicMLA"
  return(result)
}



#'
#' Plots fitCyclicMLA object
#' @param x fitCyclicMLA object
#' @param ... ymin, ymax, step can specified to control axes of the plot
#' @return plots of the aggregated data with the predicted values from the model
#' @method plot fitCyclicMLA
#' @export
plot.fitCyclicMLA <- function(x,...) {

  if (is.null(x$intermediate$ncycle))  return("No plots available, because no cycles were requested")

  dat <- x$intermediate$dat
  yvar <- x$input$yvar
  xvar1 <-  x$input$xvar1
  xvar2 <- x$input$xvar2
  cov <- x$input$cov
  b <- x$parameters
  ncycle <- x$input$ncycle
  P <- x$period[1]
  if (!ncycle == 1) P2 <- x$period[2]

  if (is.null(xvar1)) return("Plotting is not possible because xvar1 has not been specified")
  if (is.null(xvar2)) {
    xvar2 <- "dummy"
    dat[,xvar2] <- 1
  }


  datm <- stats::aggregate(dat[,c(yvar,cov)],by=list(dat[,xvar1],dat[,xvar2]), FUN=mean, na.rm=TRUE);
  datm$xvar2 <- as.numeric(datm$Group.2)
  datm$xvar1 <- as.numeric(datm$Group.1)
  datm$y <- datm[,3]

  args <- list(...)
  ifelse(!is.null(args[['ymin']]), ymin <- args[['ymin']], ymin <- round(min(datm$y, na.rm = TRUE),2))
  ifelse(!is.null(args[['ymax']]), ymax <- args[['ymax']], ymax <- round(max(datm$y, na.rm = TRUE),2))
  ifelse(!is.null(args[['step']]), step <- args[['step']], step <- (ymax - ymin)/10)


  # predict in aggregated data from fitted model
  if (ncycle == 1) {
    if (!is.null(cov))  {
      datm$ypred <-  b[1] + b[2]*cos(2*pi/P*(datm$xvar1 - b[3])) +  as.matrix(datm[,cov]) %*% b[-c(1:3)]
    }  else { datm$ypred <- b[1] + b[2]*cos(2*pi/P*(datm$xvar1 - b[3]))
    }
  }

  if (!ncycle == 1) {
    if (!is.null(cov))  {
      datm$ypred <-  b[1] + b[2]*cos(2*pi/P*(datm$xvar1 - b[3])) +
        b[4]*cos(2*pi/P2*(datm$xvar2 - b[5])) + as.matrix(datm[,cov]) %*% b[-c(1:5)]
    } else { datm$ypred <- b[1] + b[2]*cos(2*pi/P*(datm$xvar1 - b[3])) +
      b[4]*cos(2*pi/P2*(datm$xvar2 - b[5]))
    }
  }

  datm$grp <- as.factor(datm$xvar2)
  npoints <- dim(datm)[1]
  if(xvar2 == "dummy") {
    datm$xall <- datm$xvar1
  } else {
    datm$xall <- c(1:npoints)
  }

  p <- ggplot(datm) + geom_point(aes(x=datm$xall, y=datm$y, colour=datm$grp))
  p <- p + labs(y = yvar, x = "Time points ")
  p <- p + theme(axis.text = element_text(size = 6, colour="black"),legend.position="none")
  p <- p + geom_line(aes(x=datm$xall, y=datm$ypred))
  p <- p + coord_cartesian(ylim=c(ymin, ymax)) + scale_y_continuous(breaks=seq(ymin, ymax, step))

  return(p)
}




#'
#' Prints fitCyclicMLA object
#' @param x fitCyclicMLA object
#' @method print fitCyclicMLA
#' @export
print.fitCyclicMLA <- function(x) {

  if (is.null(x$input$xvar1)) {
    cat("\n","The intercept-only model has been fitted", "\n\n")
    cat(" The R-square of the fitted model is: ",
        round(1-stats::var(stats::residuals(x$fit))/(stats::var(stats::model.response(stats::model.frame(x$fit)))),3), "\n")
    ICC <- (as.data.frame(lme4::VarCorr(x$fit))[1,"vcov"]) / sum(as.data.frame(lme4::VarCorr(x$fit))[,"vcov"])
    cat(" The Intraclass correlation (ICC) is: ", round(ICC, 3), "\n\n")
    print(x$fit)
    return()
  }

  ncycle <- x$input$ncycle
  b <- data.frame(x$parameters)
  colnames(b) <- "estimates"

  cat("The dependent variable is:   ",x$input$yvar, "\n", sep="")
  cat("The first time variable is:  ",x$input$xvar1, "\n", sep="")
  if (!ncycle == 1) cat("The second time variable is: ",x$input$xvar2, "\n", sep="")
  if (!is.null(x$input$cov)) cat("The covariates are:        ",x$input$cov, "\n", sep="  ")
  cat("\n")
  cat("The period of the first cycle is: ", x$period[1] ,"\n")
  if (!ncycle == 1) cat("The period of the second cycle is: ", x$period[2] ,"\n\n")
  cat("The formula used to fit the model is:   ",x$formule, "\n\n")
  cat("The parameters of the fitted model are: ", "\n\n")
  print(b, digits = 2)
  cat("\n\n")
  cat("The deviance of the fitted model is:   ",stats::deviance(x$fit, REML = FALSE), "\n\n")
  cat("The R-square of the fitted model is:   ", 
      1-stats::var(stats::residuals(x$fit))/(stats::var(stats::model.response(stats::model.frame(x$fit)))), "\n\n")
  cat("The standard deviation of and correlation between the random effects are: ", "\n\n")
  print(lme4::VarCorr(x$fit, REML = FALSE), digits = 2)

}


