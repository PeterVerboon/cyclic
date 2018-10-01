
#' Fits the cyclic model on ESM data.
#'
#' This function fits the cyclic model on ESM data, using a multilevel model (see Verboon & Leontjevas, 2018)
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
#' @param ymin,ymax,step parameters that control axes of the plot
#' @keywords cyclic model ESM
#' @return list containing the following elements:
#' @return parameters =      intercept, amplitude and phase (and other terms in the model)
#' @return fit =             object from lmer()
#' @return formula =     specification of lmer model
#' @return period =      specified periodicity of cyclic process
#' @import ggplot2
#' @export
#' @examples
#'  data("smokedat")
#'   model <- fitCyclicMLA(dat=smokedat,  yvar="intention", xvar1="beepnr", xvar2="daynr", id = "subjnr",
#'   random = "all",  ncycle = 1, cov = c("stress", "positiveAffect"),
#'   ymin = -0.5, ymax = 0.5, step=0.10)
fitCyclicMLA <- function(dat, yvar = NULL, xvar1 = NULL, xvar2 = NULL, id = NULL, cov = NULL,
                         ncycle = 1, P = NULL, P2 = NULL, random = "intercept",
                         ymin = -1.0, ymax = 1.0, step=0.25 )
{

  result <- list()
  result$input <- as.list(environment())


  # check basic input
  if (is.null(yvar)) { stop("parameter 'yvar' has not been specified")}
  if (is.null(id))  { stop("parameter 'id' has not been specified")}
  if (!is.null(id))  { if (!id %in% colnames(dat)) { stop(paste0("Variable '", id, "' does not exist"))}}
  if (!is.null(yvar))  { if (!yvar %in% colnames(dat)) { stop(paste0("Variable '", yvar, "' does not exist"))}}
  if (is.null(cov)) a.cov <- NULL

  dat$y <- dat[,yvar]
  dat$id <- dat[,id]

  # Null model
  if (is.null(xvar1)) {
    cat("The intercept only model was requested, since parameter 'xvar1' is empty")
    form <- "y ~ 1 + (1 | id)"
    result$fit <- lme4::lmer(form,data = dat)
    class(result)  <- "fitCyclicMLA"
    return(result)
  }

  # check input and if possible adjust
  if (!xvar1 %in% colnames(dat)) { stop(paste0("Variable '", xvar1, "' does not exist"))}
  if (!is.null(xvar2)) { if (!is.null(xvar2) & !xvar2 %in% colnames(dat)) { stop(paste0("Variable '", xvar2, "' does not exist"))}}
  if (!ncycle == 1 & is.null(xvar2)) {stop("Two cyclic patterns were requested, but parameter 'xvar2' has not been specified") }
  ifelse (is.null(xvar2), dat$day <- 1  , dat$day <- as.factor(dat[,xvar2]))
  if (ncycle == 1 & random == "second") {cat("Number of cycles is set to 2");  ncycle <- 2 }
  if (is.null(cov) & random == "cov") {cat("No covariates are specified. Random term is set to 'intercept'");  random <- "intercept" }

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

  if (is.null(P)) {P <- max(dat[,xvar1])}
  dat$cvar <- cos((2*pi/P)*dat[,xvar1])
  dat$svar <- sin((2*pi/P)*dat[,xvar1])

  if (!ncycle == 1) {
    if (is.null(P2)) {P2 <- max(dat[,xvar2])}
    dat$cvar2 <- cos((2*pi/P2)*dat[,xvar2])
    dat$svar2 <- sin((2*pi/P2)*dat[,xvar2])
  }

  # fit cyclic model using MLA

  fit <- lme4::lmer(form,data = dat)

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
    names(b) <- c("intercept", "amplitude", "phase" ,  cov)
  }

  if (!ncycle == 1) {
    b <- c(a0,cycpar(a1,a2, P),cycpar(a3,a4, P2),a.cov)
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
#' @param ... ymin, ymax, step can specified to control axes of the plot
#' @return plots of the aggregated data with the predicted values from the model
#' @method plot fitCyclicMLA
#' @import ggplot2
#' @export
plot.fitCyclicMLA <- function(x,...) {

  dat <- x$input$dat
  yvar <- x$input$yvar
  xvar1 <-  x$input$xvar1
  xvar2 <- x$input$xvar2
  cov <- x$input$cov
  b <- x$parameters
  ncycle <- x$input$ncycle
  P <- x$period[1]
  if (!ncycle == 1) P2 <- x$period[2]

  if (is.null(xvar1)) return("Plotting is not possible because xvar1 has not been specified")

  args <- list(...)
  ifelse(!is.null(args[['ymin']]), ymin <- args[['ymin']], ymin <- x$input$ymin)
  ifelse(!is.null(args[['ymax']]), ymax <- args[['ymax']], ymax <- x$input$ymax)
  ifelse(!is.null(args[['step']]), step <- args[['step']], step <- x$input$step)


  datm <- aggregate(dat[,c(yvar,cov)],by=list(dat[,xvar1],dat[,xvar2]), FUN=mean, na.rm=F);
  datm$xvar2 <- as.numeric(datm$Group.2)
  datm$xvar1 <- as.numeric(datm$Group.1)
  datm$y <- datm[,3]

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

  datm$day <- as.factor(datm$xvar2)
  npoints <- dim(datm)[1]
  datm$xall <- c(1:npoints)

  p <- ggplot(datm) + geom_point(aes(x=datm$xall, y=datm$y, colour=datm$day))
  p <- p + scale_x_discrete(name ="Time points (beeps within days)",  labels=datm$xvar1, limits=c(1:npoints))
  p <- p + labs(y = yvar)
  p <- p + theme(axis.text = element_text(size = 6, colour="black"),legend.position="none")
  p <- p + geom_line(aes(x=datm$xall, y=datm$ypred))
  p <- p + coord_cartesian(ylim=c(ymin, ymax)) + scale_y_continuous(breaks=seq(ymin, ymax, step))

  return(p)
}




#'
#' Prints fitCyclicMLA object
#' @method print fitCyclicMLA
#' @export
print.fitCyclicMLA <- function(x,digits=2,...) {

  if (is.null(x$input$xvar1)) {
    cat("\n","The intercept-only model has been fitted", "\n\n")
    cat(" The R-square of the fitted model is: ", 1-var(residuals(x$fit))/(var(model.response(model.frame(x$fit)))), "\n")
    ICC <- (as.data.frame(lme4::VarCorr(model1$fit))[1,"vcov"]) / sum(as.data.frame(lme4::VarCorr(model1$fit))[,"vcov"])
    cat(" The Intraclass correlation (ICC) is: ", ICC, "\n\n")
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
  print(b, digits = digits)
  cat("\n\n")
  cat("The deviance of the fitted model is:   ",deviance(x$fit, REML = FALSE), "\n\n")
  cat("The R-square of the fitted model is:   ", 1-var(residuals(x$fit))/(var(model.response(model.frame(x$fit)))), "\n\n")
  cat("The standard deviation of and correlation between the random effects are: ", "\n\n")
  print(lme4::VarCorr(x$fit, REML = FALSE), digits = digits)

}


