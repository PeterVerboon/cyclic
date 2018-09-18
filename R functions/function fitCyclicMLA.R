require(ggplot2)
require(lme4)


fitCyclicMLA <- function(dat, yvar = NULL, xvar1 = NULL, xvar2 = NULL, id = NULL, ncycle = 1, 
                         random = "intercept", cov = NULL, P = NULL, P2 = NULL,
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
    result$fit <- lmer(form,data = dat) 
    return(result)    
  } 

  # check input and if possible adjust
  if (!xvar1 %in% colnames(dat)) { stop(paste0("Variable '", xvar1, "' does not exist"))}
  if (!is.null(xvar2)) { if (!is.null(xvar2) & !xvar2 %in% colnames(dat)) { stop(paste0("Variable '", xvar2, "' does not exist"))}}
  if (!ncycle == 1 & is.null(xvar2)) {stop("Two cyclic patterns were requested, but parameter 'xvar2' has not been specified") }
  ifelse (is.null(xvar2), dat$day <- 1  , dat$day <- as.factor(dat[,xvar2]))
  if (ncycle == 1 & random == "second") {cat("Number of cycles is set to 2");  ncycle <- 2 }
  
  # construct formula
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
  
  fit <- lmer(form,data = dat)

  a0 <- fixef(fit)[1]
  a1 <- fixef(fit)[2]
  a2 <- fixef(fit)[3]
  if (ncycle == 1 & !is.null(cov)) a.cov <- fixef(fit)[4:(3+length(cov))]
  if (!ncycle == 1) { 
    a3 <- fixef(fit)[4] ;
    a4 <- fixef(fit)[5] ;
    if (!is.null(cov)) a.cov <- fixef(fit)[6:(5+length(cov))]
  }

 # convert to parameters from linear model
  if (ncycle == 1) { 
    b <- c(a0,cycpar(a1,a2, P),a.cov)     
    names(b) <- c("intercept", " amplitude", " phase" ,  cov)
  }
  
  if (!ncycle == 1) {
     b <- c(a0,cycpar(a1,a2, P),cycpar(a3,a4, P2),a.cov)     
     names(b) <- c("intercept  ", "short_cycle_amplitude", "short_cycle_phase",  "long_cycle_amplitude" , "long_cycle_phase",cov)
  }
 
  result$fit <- fit
  result$parameters <- data.frame(b)
  result$formule <- form
  result$period <- c(P, P2)
 
  class(result)  <- "fitCyclicMLA"
  return(result)

}  


# test

model <- fitCyclicMLA(dat=dat3,  yvar="intention", xvar1="beepnr",xvar2="daynr", id = "subjnr",
                      random = "intercept",  ncycle = 2, cov =c("stress"), 
                      ymin = -0.5, ymax = 0.5, step=0.10 )
#
#
 model$parameters
 model$fit
 model$formule
 model$input
 model$period
 
 
 
 plot.fitCyclicMLA <- function(x,...) {
  
 dat <- model$input$dat 
 b <- model$parameters
 ncycle <- model$input$ncycle
 P <- model$period[1]
 if (!ncycle == 1) P2 <- model$period[2]
 ymin <- model$input$ymin
 ymax <- model$input$ymax
 step <- model$input$step
 
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
 
 # make plot
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

 plot(model) 
 
 print.fitCyclicMLA <- function(x,digits=3,...) {
   
   ncycle <- model$input$ncycle
  
   cat("The dependent variable is:   ",model$input$yvar, "\n", sep="")
   
   cat("The time variable is:        ",model$input$xvar1, "\n", sep="")
   if (!ncycle == 1) cat("The second time variable is: ",model$input$xvar2, "\n\n", sep="")
   cat("The parameters of the fitted model are: ", "\n")
   print(model$parameters)
   cat("\n\n")
   cat("The period of the first cycle is: ", model$period[1] ,"\n")
   if (!ncycle == 1) cat("The period of the second cycle is: ", model$period[2] ,"\n\n")
   cat("The formula used to fit the model is:   ",model$formule, "\n")
   
   
 }

 print(model) 
 