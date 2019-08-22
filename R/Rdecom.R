#' R-squared based on ANOVA decomposition in multilevel model
#'
#' Function that computes the R-squared measure based on  ANOVA decomposition measure (Edoardo Costantini, 2018).
#' The explained variance is split into the explained variance for level-1 (within subjects), for the level-2 (between subjects)
#' and the total explained variance.
#'
#' @param dat data set
#' @param y dependent variable of the multilevel model
#' @param grpid group identity (cluster variable)
#' @param predicted predicted values from multilevel model
#' @return vector containing three values, named: "level_1_R2","level_2_R2", "total_R2"
#' @export
#'
#' @references
#' Constantini, E. (2018). R-squared measures in Multilevel Modeling:
#' The undesirable property of negative R-squared values.
#' Downloaded from \emph{http://arno.uvt.nl/show.cgi?fid=146739}, August 21, 2019.
#'
Rdecom <- function(dat, y, grpid, predicted) {
  y <- dat[,y]
  grpid <- dat[,grpid]
   yj <- NA
   yj_unique <- stats::aggregate(y, list(grpid), mean)
   for(i in 1:nrow(dat)) {
     #vector of observed outcome group means (each case has its group mean as value)
    yj[i] <- yj_unique[yj_unique$Group.1 == grpid[i], 2]
   }

## SSw_P (Numerator)
SSw_P <- sum((yj-predicted)^2) #SSw (denominator)
SSw <- sum((y-yj)^2) #R1^2
R1_2 <- SSw_P/SSw

## Then, level-2
## Predicted group means
predicted_gm <- stats::aggregate(predicted, list(grpid), mean)[, 2]
yj <- stats::aggregate(y, list(grpid), mean)[, 2] #B (numerator)
SSb_P <- sum((predicted_gm - mean(y))^2)

## SSb (denominator)
SSb <- sum((yj - mean(y))^2) #LVL2 R-squared measure
R2_2 <- SSb_P/SSb

## And finally, the measure of total explained variance
## TSS with predicted
TSSp <- sum((predicted-mean(y))^2) #TSS
TSS <- sum((y-mean(y))^2)          #TOT R-squared
Rtot <- TSSp/TSS

Rdecom <- round(c(R1_2, R2_2, Rtot), 3)
names(Rdecom) <- c("level_1_R2","level_2_R2", "total_R2")

return(Rdecom)
}




