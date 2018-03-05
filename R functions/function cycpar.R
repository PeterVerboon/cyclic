
# This function computes the cyclic parameters from parameters linear model (see Verboon & Leontjevas, 2018)

# a1 = b1*cos((2π/P)*b2a)      parameter of term cos((2π/P)*T) 
# a2 = b1*sin((2π/P)*b2a)      parameter of term sin((2π/P)*T)
# P is the periodicity of the cycle
# T indicates time points




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
  
} # END FUNCTION
