readMsn <- function(file) {
  #to convert lat lon values
  conv <- function(x) {
    res <- rep(NA, length(x))
    zeros <- x == "0"
    nas <- is.na(x)
    good <- !(zeros | nas)
    res[good] <- ifelse(substr(x[good], 1, 1) == "-", -1, 1)*
      ((abs(as.numeric(x[good])/100) - floor(abs(as.numeric(x[good])/100)))*100/60 
       + floor(abs(as.numeric(x[good])/100)))
    res[zeros] <- 0
    return(res)
  }
  # test using readLines
  con <- file(file)
  d <- readLines(con)
  close(con)
  # find which line starts with "[path]"
  # and define end index
  start <- which(d == "[path]") + 1
  end <- length(d) - 1
  dd <- d[start:end]
  
  #now start to parse data
  # format of lines
  # NAME=x[longitude];y[latitude];profile;Rval[numeric];NAMENEXT;
  # here we want NAME, longitude, latitude, and the numeric value for Rval
  
  # 1. do a str split on ;
  ss <- strsplit(dd, split = ';')
  # 2. get all the variables
  wpname <- unlist(lapply(ss, function(k) strsplit(k[1], split = '=')[[1]][1]))
  xlon <- unlist(lapply(ss, function(k) strsplit(k[1], split = '=')[[1]][2]))
  lon <- unlist(lapply(xlon, function(k) strsplit(k, split = 'x')[[1]][2]))
  lat <- unlist(lapply(ss, function(k) strsplit(k[2], split = 'y')[[1]][2]))
  rval <- unlist(lapply(ss, function(k) strsplit(k[4], split = 'Rval')[[1]][2]))
  
  df <- data.frame(
    wp = wpname,
    lon = conv(as.numeric(lon)),
    lat = conv(as.numeric(lat)),
    rval = as.numeric(rval)
  )
  df
}
