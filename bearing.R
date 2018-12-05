# calculate the bearing between point p0 and p1
# where N is 0deg, and E is 90deg
bearing <- function(lon, lat){
  pi <- 4 * atan2(1, 1)
  # bearing = atan2(y,x)
  # y = cos(lat1) * sin(lon0 - lon1)
  # x = cos(lat0) * sin(lat1) - sin(lat0) * cos(lat1) * cos(lon0 - lon1)
  lat0 <- lat[1:(length(lat) - 1)]
  lat1 <- lat[2:length(lat)]
  lon0 <- lon[1:(length(lon) - 1)]
  lon1 <- lon[2:length(lon)]
  y <- cos(lat1) * sin(lon1 - lon0)
  x <- cos(lat0) * sin(lat1) - sin(lat0) * cos(lat1) * cos(lon1 - lon0)
  bearing <- atan2(y = y, x = x) * 180 / pi
  # get all positive values
  idx <- which(bearing < 0)
  bearing[idx] <- bearing[idx] + 360
  bearing
}