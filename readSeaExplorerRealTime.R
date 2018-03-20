readSeaExplorerRealTime <- function(datadir, glider, mission){
  dir <- paste(datadir,
               glider,
               mission,
               '',
               sep = '/')
  filelist <- list.files(path = dir, pattern = '*.gli.sub.*.gz')
  okfiles <- !grepl(pattern = '*Copy.gz', x = filelist) #omit these files, creates error below
  files <- paste(dir, as.list(filelist[okfiles]), sep = '')
  
  # to put the files in the right order
  strl <- nchar(files)
  cate <- length(unique(strl))
  if(cate == 1){
    data_all <- lapply(as.list(files), read.table, sep=";",header=TRUE)
  } else if (cate==2){
    first <- which(strl == min(strl), arr.ind = TRUE)
    seco <- which(strl == min(strl)+1, arr.ind = TRUE)
    data_all1 <- lapply(as.list(files[first]), read.table, sep=";",header=TRUE)
    data_all2 <- lapply(as.list(files[seco]), read.table, sep=";",header=TRUE)
    data_all <- c(data_all1,data_all2)
  } else {
    first <- which(strl == min(strl), arr.ind = TRUE)
    seco <- which(strl == min(strl)+1, arr.ind = TRUE)
    third <- which(strl == min(strl)+2, arr.ind = TRUE)
    data_all1 <- lapply(as.list(files[first]), read.table, sep=";",header=TRUE)
    data_all2 <- lapply(as.list(files[seco]), read.table, sep=";",header=TRUE)
    data_all3 <- lapply(as.list(files[third]), read.table, sep=";",header=TRUE)
    data_all <- c(data_all1,data_all2,data_all3)
  }
  
  #Yo num
  profileNum <- unlist(lapply(files, function(x) {
    tmp <- unlist(strsplit(x, '.', fixed=TRUE))[6]
    len <- dim(read.table(x, sep=';', header=TRUE))[1]
    rep(tmp, len)
  }))
  profileNum <- sort(as.numeric(profileNum))
  
  # to read the time in the right format
  time_tmp <- unlist(lapply(data_all, function(k) k$Timestamp))
  time <- as.POSIXct(time_tmp,format='%d/%m/%Y %H:%M:%S',tz='UTC')
  time[time < as.POSIXct('2010-01-01')] <- NA
  
  # to calculate the vertical speed
  times<-as.integer(time) # in seconds since 1970
  depth<-unlist(lapply(data_all, function(k) k$Depth))
  VS=c()
  for (i in c(1:length(depth)-1)){
    VS[i]<-((depth[i+1]-depth[i])/(times[i+1]-times[i]))*-100
  }
  VS[length(depth)]<-NA
  
  #to add the altimeter hit on the depth graph
  alt<-unlist(lapply(data_all, function(k) k$Altitude))
  alt[alt<0]<-0
  altHit<-depth+alt
  
  # to put everything in a dataframe where all the dives are together
  NAV <- data.frame(
    profileNumber=profileNum,
    time=time,
    VertSpeed=VS,
    altHit=altHit,
    NavState=unlist(lapply(data_all, function(k) k$NavState)),
    alarm=unlist(lapply(data_all, function(k) k$SecurityLevel)),
    Heading=unlist(lapply(data_all, function(k) k$Heading)),
    Pitch=unlist(lapply(data_all, function(k) k$Pitch)),
    Roll=unlist(lapply(data_all, function(k) k$Roll)),
    Temperature=unlist(lapply(data_all, function(k) k$Temperature)),
    int_pres=unlist(lapply(data_all, function(k) k$Pa)),
    DesiredHeading=unlist(lapply(data_all, function(k) k$DesiredH)),
    depth=unlist(lapply(data_all, function(k) k$Depth)),
    BallastCmd=unlist(lapply(data_all, function(k) k$BallastCmd)),
    BallastPos=unlist(lapply(data_all, function(k) k$BallastPos)),
    LinCmd=unlist(lapply(data_all, function(k) k$LinCmd)),
    LinPos=unlist(lapply(data_all, function(k) k$LinPos)),
    AngCmd=unlist(lapply(data_all, function(k) k$AngCmd)),
    AngPos=unlist(lapply(data_all, function(k) k$AngPos)),
    BatterieVolt=unlist(lapply(data_all, function(k) k$Voltage)),
    alt=unlist(lapply(data_all, function(k) k$Altitude))
  )
  
  filesci <- paste(dir, as.list(list.files(path = dir, pattern = '*.pld1.sub.*.gz')), sep = '')
  # to put the files in the right order
  strlsci<-nchar(filesci)
  catesci<-length(unique(strlsci))
  if(catesci==1){
    data_allsci <- lapply(as.list(filesci), read.table, sep=";",header=TRUE)
  } else if (catesci==2){
    firstsci<-which(strlsci == min(strlsci), arr.ind = TRUE)
    secosci<-which(strlsci == min(strlsci)+1, arr.ind = TRUE)
    data_all1sci <- lapply(as.list(filesci[firstsci]), read.table, sep=";",header=TRUE)
    data_all2sci <- lapply(as.list(filesci[secosci]), read.table, sep=";",header=TRUE)
    data_allsci <- c(data_all1sci,data_all2sci)
  } else {
    firstsci<-which(strlsci == min(strlsci), arr.ind = TRUE)
    secosci<-which(strlsci == min(strlsci)+1, arr.ind = TRUE)
    thirdsci<-which(strlsci == min(strlsci)+2, arr.ind = TRUE)
    data_all1sci <- lapply(as.list(filesci[firstsci]), read.table, sep=";",header=TRUE)
    data_all2sci <- lapply(as.list(filesci[secosci]), read.table, sep=";",header=TRUE)
    data_all3sci <- lapply(as.list(filesci[thirdsci]), read.table, sep=";",header=TRUE)
    data_allsci <- c(data_all1sci,data_all2sci,data_all3sci)
  }
  
  # to read the time in the right format
  
  time_tmpsci <- unlist(lapply(data_allsci, function(k) k$PLD_REALTIMECLOCK))
  timesci <- as.POSIXct(time_tmpsci,format='%d/%m/%Y %H:%M:%S',tz='UTC')
  timesci[timesci < as.POSIXct('2010-01-01')] <- NA
  
  #calculate distance traveled and glider speed
  # convert lat long to decimal
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
  LonT <- unlist(lapply(data_allsci, function(k) k$NAV_LONGITUDE))
  Lond <- conv(LonT)
  LatT <- unlist(lapply(data_allsci, function(k) k$NAV_LATITUDE))
  Latd <- conv(LatT)
  # Identify each inflacting down (Navstate=110) time and position for calculation
  index115 <- which(NAV$NavState %in% 110)
  NavTime115t <- NAV$time[index115]
  NavTime115 <- NavTime115t[!is.na(NavTime115t)]
  
  index115sci <- rep(NA, length(NavTime115))
  for (j in c(1:length(NavTime115))){
    index115sci[j] <- which.min(abs(timesci - NavTime115[j]))
  }
  PLDTime115 <- timesci[index115sci]
  
  dist <- distGeo(matrix(c(Lond[index115sci], Latd[index115sci]),nrow=length(PLDTime115),ncol=2))
  distsum<- rep(NA, length(dist))
  speed<- rep(NA, length(dist))
  for (j in c(1:length(dist))){
    distsum[j] <- sum(dist[1:j])/1000
    speed[j] <- dist[j]/as.numeric(PLDTime115[j+1]-PLDTime115[j],units='secs')
  }
  timedist <- PLDTime115[2:length(PLDTime115)]
  speed_goodi <- which(speed!=0)
  speed_good <- speed[speed_goodi]
  timespeed <- timedist[speed_goodi]
  #put speed and dist back on NAV$time ref
  indexspeed <- rep(NA, length(timespeed))
  indexdist <- rep(NA, length(timedist))
  for (j in c(1:length(timespeed))){
    indexspeed[j] <- which.min(abs(NAV$time - timespeed[j]))
  }
  for (j in c(1:length(timedist))){
    indexdist[j] <- which.min(abs(NAV$time - timedist[j]))
  }
  ####
  ##
  ##
  ###
  # put 2 new variables in glider data frame
  NAV$speedms <- rep(NA, length(NAV$time))
  NAV$distkm <-  rep(NA, length(NAV$time)) 
  NAV$speedms[indexspeed] <- speed_good
  NAV$distkm[indexdist] <- distsum
  
  # to put everything in a dataframe where all the dive are together
  PLD <- data.frame(
    timesci=timesci,
    Lat=Latd,
    Lon=Lond,
    Depthsci=unlist(lapply(data_allsci, function(k) k$NAV_DEPTH)),
    CHL_count=unlist(lapply(data_allsci, function(k) k$FLBBCD_CHL_COUNT)),
    CHL_scaled=unlist(lapply(data_allsci, function(k) k$FLBBCD_CHL_SCALED)),
    BB_count=unlist(lapply(data_allsci, function(k) k$FLBBCD_BB_700_COUNT)),
    BB_scaled=unlist(lapply(data_allsci, function(k) k$FLBBCD_BB_700_SCALED)),
    CDOM_count=unlist(lapply(data_allsci, function(k) k$FLBBCD_CDOM_COUNT)),
    CDOM_scaled=unlist(lapply(data_allsci, function(k) k$FLBBCD_CDOM_SCALED)),
    Temp=unlist(lapply(data_allsci, function(k) k$GPCTD_TEMPERATURE)),
    Press=unlist(lapply(data_allsci, function(k) k$GPCTD_PRESSURE)),
    DOF=unlist(lapply(data_allsci, function(k) k$GPCTD_DOF)),
    Conduc=unlist(lapply(data_allsci, function(k) k$GPCTD_CONDUCTIVITY))
  )
  # calculate salinity, sigTheta, and oxygen saturation
  PLD$Sal <- swSCTp(conductivity = PLD$Conduc, 
                    temperature = PLD$Temp, 
                    pressure = PLD$Press, 
                    conductivityUnit = "S/m")
  PLD$SigTheta <- swSigmaTheta(salinity = PLD$Sal,
                               temperature = PLD$Temp,
                               pressure = PLD$Press)
  # calculate oxygen saturation
  # TO-DO implement new oxygen sensor for SEA032
  # use coefficients from calibration files
  okcalib <- which(names(oxycalib) == glider)
  cal <- oxycalib[[okcalib]]
  PLD$OxySat <- sbeO2Hz2Sat(temperature = PLD$Temp, salinity = PLD$Sal, 
                            pressure = PLD$Press, oxygenFrequency = PLD$DOF,
                            Soc = cal[['Soc']], Foffset = cal[['Foffset']], 
                            A = cal[['A']], B = cal[['B']],
                            C = cal[['C']], Enom = cal[['Enom']])
  
  bad <- is.na(PLD$timesci)
  PLD <- PLD[!bad,]
  bad <- is.na(NAV$time)
  NAV <- NAV[!bad,]
  
  invisible(list(PLD = PLD, NAV = NAV))
}