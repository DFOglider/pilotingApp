#' @title Convert oxygen frequency to oxygen saturation
#' 
#' @details This function converts oxygen frequency in units Hz to oxygen saturation in units
#' ml/l outputed from SBE 43F Dissolved Oxygen Sensor using the equation
#' supplied on the calibration sheet to oxygen saturation.
#' 
#' @author Chantelle Layton
#' @param temperature Temperature in degrees celcius
#' @param salinity Salinity
#' @param pressure pressure [dbar]
#' @param oxygenFrequency Oxygen frequency instrument output
#' @param Soc Coefficient from calibration file
#' @param Foffset Coefficient from calibration file
#' @param Soc Coefficient from calibration file
#' @param B Coefficient from calibration file
#' @param C Coefficient from calibration file
#' @param Enom Coefficient from calibration file

sbeO2Hz2Sat <- function(temperature, salinity, pressure, 
                        oxygenFrequency, Soc, Foffset, 
                        A, B, C, Enom){
  Tk <- 273.15 + temperature * 1.0002 # convert temperature to units kelvin
  Soc * (oxygenFrequency + Foffset) * 
    (1.0 + A*T + B*T^2 + C*T^3) * 
    swSatO2(temperature = temperature, salinity = salinity) * 
    exp(Enom*pressure/Tk)
}