#' @title Saturation of O2 in sea water
#' 
#' @details Computes the solubility (saturation) of Oxygen in sea water. Based on the
#' matlab function SW_SATO2 from the CSIRO seawater toolbox.
#' 
#' @author Chantelle Layton
#' @param temperature temperature
#' @param salinity salinity
#' 

swSatO2 <- function(temperature, salinity){
  Tk <- 273.15 + temperature * 1.00024
  # constants for Eqn (4) of Weiss 1970
  a1 <- -173.4292
  a2 <-  249.6339
  a3 <-  143.3483
  a4 <-  -21.8492
  b1 <-   -0.033096
  b2 <-    0.014259
  b3 <-   -0.0017000
  
  lnC <- a1 + 
         a2*(100/Tk) + 
         a3*log(Tk/100) + 
         a4*(Tk/100) + 
         salinity*(b1 + b2*(Tk/100) + b3*((Tk/100)^2))
  
  exp(lnC)
}