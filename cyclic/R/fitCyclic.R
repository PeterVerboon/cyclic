#' Fits the cyclic model, using a simple linear model.

#'
#' This function fits the cyclic model, using a simple linear model (see Verboon & Leontjevas, 2018)
#' @param dat data frame containing the variables
#' @param yvar dependent variable
#' @param xvar time variabele, usually indicates the beeps or other numerical variable. Time format is also possible.
#' @param grp group variabele indicator, usually indicates the days, used in "raw" plot.
#' @param P the periodicity of the cycle. If NULL P will be computed from the data.
#' @param cov vector of names containing additional variabeles (e.g. cov = c("x1", "daynr"))
#' @param ymin,ymax,step parameters that control axes of the plot
#' @param xlabs,ylabs labels respectively for x-axis and y-axis of the plots
#' @keywords cyclic model ESM
#' @return list containing the following elements:
#' @return parameters =      amplitude and phase
#' @return fit =             object from lm()
#' @return rawDataPlot =     plot of predicted values for all observations
#' @return meansPlot =       plot with predictions averaged over subjects,
#' @return oneCyclePlot =    plot with predictions for one cycle,
#' @export
#' @import ggplot2
#' @examples
#' data("pdat")
#' fitCyclic(dat=pdat, yvar = "stress", xvar = "beepnr", grp = "daynr")
fitCyclic <- function(dat, yvar = NULL, xvar = NULL, grp = NULL, cov = NULL , P = NULL,
                      ymin = -1.0, ymax = 1.0, step=0.25, xlabs = NULL, ylabs = NULL ) {

    result <- list()
    result$input <- as.list(environment())

    # check basic input
    if (is.null(yvar)) { stop("parameter 'yvar' has not been specified")}
    if (is.null(xvar)) { stop("parameter 'xvar' has not been specified")}

    if (!is.null(yvar))  { if (!yvar %in% colnames(dat)) { stop(paste0("Variable '", yvar, "' does not exist"))}}
    if (!is.null(xvar))  { if (!xvar %in% colnames(dat)) { stop(paste0("Variable '", xvar, "' does not exist"))}}

    if (is.null(cov)) a.cov <- NULL

    ifelse (is.null(cov),form <- paste0("y ~ cvar + svar"), form <- paste0("y ~ cvar + svar + ", cov) )


    ### If the time variable is actually provided as time instead of as
    ### indices/ranks, convert to numeric first.
    if (!is.numeric(dat[,xvar])) {
      if (any(class(dat[,xvar]) %in% c('Date', 'POSIXct', 'POSIXt', 'POSIXt', 'hms'))) {
        dat$h <- round((as.numeric(dat[,xvar])/3600), digits=0)
        dat[,xvar] <- dat$h - min(dat$h) + as.numeric(format(min(dat[,xvar]), format="%H"))
      } else {
        cat("The time variable does not have a class numeric or date.","\n",
            "I am trying to create time variable. Check results carefully.","\n")
        dat$h <- round(as.POSIXct(dat[,xvar], format="%H:%M:%S", tz="UTC"))
        dat$h <- as.numeric(format(dat$h, format="%H"))
        dat$h[dat$h == 0] <- max(dat$h) + 1
        dat[,xvar] <- dat$h
      }
    }

    if (is.null(P)) {P <- max(dat[,xvar]) - min(dat[,xvar]) + 1}
    range <- max(dat[,xvar]) - min(dat[,xvar])

    dat$cvar <- cos((2*pi/P)*dat[,xvar])
    dat$svar <- sin((2*pi/P)*dat[,xvar])
    dat$y <- dat[,yvar]
    dat$x <- dat[,xvar]
    ifelse (is.null(grp), dat$grp <- " "  , dat$grp <- as.factor(dat[,grp]))

    xmin <- min(dat$x)
    xmax <- max(dat$x)

    # fit cyclic model within days across beeps

    fitp <- stats::lm(form, data=dat)

    a0 <- fitp$coefficients[1]
    a1 <- fitp$coefficients[2]
    a2 <- fitp$coefficients[3]

    if(!is.null(cov)) { a.cov <- fitp$coefficients[4:(3+length(cov))]}

    par <- cycpar(a1,a2, P)

    b <- c(a0,par)
    while (b[3] < xmin) b[3] <- b[3] + range    # let the maximum fall into the range of the data


    # Parameters b1 and b2 are obtained from cyclic model analysis
   
    a0 <- round(a0,3)
    b1 <- round(b[2],3)
    b2 <- round(b[3],3)
   

    if (!is.null(cov))  {
       ypred1 <-  a0 + b1*cos(2*pi/P*(dat$x - b2)) +  as.matrix(dat[,cov]) %*% a.cov
       } else { ypred1 <-  a0 + b1*cos(2*pi/P*(dat$x - b2)) }

    npoints <- dim(dat)[1]
    dat$xall <- c(1:npoints)

    if (is.null(xlabs)) xlabs <- "Time points within days"
    if (is.null(ylabs)) ylabs <- yvar
    
    # raw data plot

    g0 <- ggplot(dat) + geom_point(aes(x=dat$xall, y=dat$y, colour=dat$grp)) +
        geom_line(aes(x=dat$xall, y=ypred1)) +
        scale_x_discrete(name =xlabs,  labels=dat$x, limits=c(1:npoints)) +
        labs(y = ylabs) +
        theme(axis.text = element_text(size = 6, colour="black"),legend.position="none") +
        coord_cartesian(ylim=c(ymin, ymax)) + scale_y_continuous(breaks=seq(ymin, ymax, step))

    # mean plot

      pdat2 <- stats::aggregate(dat[,c(yvar,cov)],by=list(dat[,xvar]), FUN=mean, na.rm=F)
      pdat2$y <- pdat2[,2]
      pdat2$x <- pdat2[,1]

    if (!is.null(cov))  {
       ypred2 <-  a0 + b1*cos(2*pi/P*(pdat2$x - b2)) +  as.matrix(pdat2[,cov]) %*% a.cov
       } else { ypred2 <-  a0 + b1*cos(2*pi/P*(pdat2$x - b2)) }

      captxt <- paste0("Note: ", "Intercept = ", a0, ";  ","Amplitude = ",  b1, ";  ","Phase = ",  b2)
      
    g1 <- ggplot(pdat2) + geom_point(aes(x=pdat2$x,y=pdat2$y)) +
       labs(caption = (captxt)) +
       geom_line(aes(x=pdat2$x, y=ypred2)) +
       geom_hline(yintercept = a0, colour = "grey30", size=0.8, linetype = "solid") +
       geom_vline(xintercept = b2, colour = "grey30", size=1.0, linetype = "dashed") +
       labs(x = xlabs, y = ylabs) +
       scale_x_discrete(name = xlabs,  limits=c(xmin:xmax)) +
       coord_cartesian(ylim=c(ymin, ymax)) + scale_y_continuous(breaks=seq(ymin, ymax, step)) +
       theme(plot.caption = element_text(hjust = 0)) +
       theme(axis.text = element_text(colour = "black", size = 11, 
                                     family="serif", hjust = 0.9, vjust = 0.9), 
            axis.title=element_text(size=11,face="bold",  family="serif"), 
            plot.title = element_text(size = 12, face = "bold",  family="serif")) 
    

    g2 <- g1 + geom_point(data=dat, aes(x=dat$x,y=dat$y),
                          position = position_jitter(width = 0.1), 
                          colour="azure4", size=0.8) 
   
    
    result$meansPlot <- g1
    result$rawDataPlot <- g0
    result$combiPlot <- g2
    result$fit  <- fitp
    result$parameters <- b
    result$formula <- form
    result$period <- P
    result$predictions <- list(raw = ypred1, mean = ypred2)

    class(result) <- "fitCyclic"
    return(result)

}



#'
#' Plots fitCyclic object
#' @param x fitCyclic object
#' @param type vector indicating plot type with elements "raw","means","combi". Default is all.
#' @method plot fitCyclic
#' @export
plot.fitCyclic <- function(x, type = c("raw","means","combi")) {

  if("raw" %in% type) graphics::plot(x$rawDataPlot)
  if("means" %in% type) graphics::plot(x$meansPlot)
  if("combi" %in% type) graphics::plot(x$combi)

}




#'
#' Prints fitCyclic object
#' @param x fitCyclicMLA object
#' @method print fitCyclic
#' @export
print.fitCyclic <- function(x) {

  b <- data.frame(x$parameters)
  colnames(b) <- "estimates"

  cat("The dependent variable is:   ",x$input$yvar, "\n", sep="")
  cat("The time variable is:        ",x$input$xvar, "\n", sep="")
  if (!is.null(x$input$cov)) cat("The covariates are:        ",x$input$cov, "\n", sep="  ")
  cat("\n")
  cat("The period of the cycle is: ", x$period ,"\n")
  cat("The formula used to fit the model is:   ",x$formula, "\n\n")
  cat("The cyclic parameters of the fitted model are: ", "\n\n")
  print(b, digits = 2)
  cat("\n")
  cat("The summary of the model fit is: ", "\n")
  print(summary(x$fit))

}






