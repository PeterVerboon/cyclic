

#' Computes the cyclic parameters from parameters linear model

#'
#' This function computes the cyclic parameters from parameters linear model (see Verboon & Leontjevas, 2018)
#' @param a1 b1*cos((2pi/P)*b2) ,     parameter of term cos((2pi/P)*T)
#' @param a2 b1*sin((2pi/P)*b2) ,     parameter of term sin((2pi/P)*T)
#' @param P is the periodicity of the cycle
#' @keywords cyclic models
#' @return The amplitude and phase of the cyclic process
#' @export
#' @examples
#' cycpar(a1=.5,a2=1.0,P=24)
cycpar <- function(a1,a2, P) {

  b1 <- sqrt(a1**2 + a2**2)

  if (a1 == 0){  b2 <- P/4 }
  if (a2 == 0){  b2 <- 0 }

  if ((a1 != 0) & (a2 != 0)) {
    if ( sign(a2) == 1)  { b2 <- acos(a1/b1)*(P/(2*pi)) }
    if ( sign(a2) == -1) { b2 <- P - acos(a1/b1)*(P/(2*pi)) }
  }

  out <- c(b1,b2)
  names(out) <- c("amplitude","phase")
  out

} # end function
