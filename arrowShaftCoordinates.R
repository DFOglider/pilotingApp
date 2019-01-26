arrowShaftCoordinates <- function (longitude, latitude, L = 10, phi = 0) 
{
  #note : this code is similar to oce's mapCoordinateSystem
  if (missing(longitude)) 
    stop("must supply longitude")
  if (missing(latitude)) 
    stop("must supply latitude")
  ok <- !is.na(phi)
  longitude <- longitude[ok]
  latitude <- latitude[ok]
  phi <- phi[ok]
  R <- 6371
  pi <- 4 * atan2(1, 1)
  phirad <- phi * pi/180 #+ c(0, pi/2)
  kmperlon <- pi * R * cos(latitude * pi/180)/180
  kmperlat <- pi * R/180
  # arrow shaft
  dx <- L * cos(phirad)
  dy <- L * sin(phirad)
  dlon <- dx/kmperlon
  dlat <- dy/kmperlat
  lonend <- longitude + dlon
  latend <- latitude + dlat
  # create data frame of data for polylines
  df <- data.frame(longitude = c(longitude, lonend), 
                   latitude = c(latitude, latend),
                  group = seq(1,length(longitude),1))
  df
}
