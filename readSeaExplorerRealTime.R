readSeaExplorerRealTime <- function(datadir, glider, mission){
  dir <- paste(datadir,
               glider,
               mission,
               '',
               sep = '/')


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

  filelist <- list.files(path = dir, pattern = '*.gli.sub.*.gz')
  okfiles <- !grepl(pattern = '*Copy.gz', x = filelist) #omit these files, creates error below
  files <- paste(dir, as.list(filelist[okfiles]), sep = '') 

  
  if(exists('files')){
  # to put the files in the right order    
  fileidx <-   as.numeric(unlist(lapply(files, function(x) unlist(strsplit(x, '.', fixed=TRUE))[6])))
  o <- order(fileidx)
  data_all <- lapply(files[o], read.table, sep = ";", header = TRUE)

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
  
  #remove 2018-07-12 dates after firmware
  # not sure of first 10 missions for SEA035, so put firmware upgrade mission as 9 for now
  gliderfirmname <- c('SEA019', 'SEA021', 'SEA022', 'SEA024', 'SEA032', 'SEA035')
  gliderfirmmiss <- c(54, 39, 32, 29, 23, 9)
  missionnum <- as.numeric(strsplit(mission, split = 'M')[[1]][2])
  for(i in 1:length(gliderfirmname)){
    if(glider == gliderfirmname[i] & missionnum > gliderfirmmiss[i]){
      time[time < as.POSIXct('2018-07-14')] <- NA
    }
  }
  
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

      ##calculate battery percentage using spline
      load('sx_spline.rda')
      batteryPerc <- sx(unlist(lapply(data_all, function(k) k$Voltage)))
      
  # to put everything in a dataframe where all the dives are together
  NAV <- data.frame(
    profileNumber=profileNum,
    time=time,
    VertSpeed=VS,
    altHit=altHit,
    NavState=unlist(lapply(data_all, function(k) k$NavState)),
    alarm=unlist(lapply(data_all, function(k) k$SecurityLevel)),
    Heading=unlist(lapply(data_all, function(k) k$Heading)),
    Declination = if("Declination" %in% names(data_all[[1]])) unlist(lapply(data_all, function(k) k$Declination)) else unlist(lapply(data_all, function(k) rep(0, length(k$Heading)))),
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
    BatteriePerc=batteryPerc,
    alt=unlist(lapply(data_all, function(k) k$Altitude)),
    Lat=conv(unlist(lapply(data_all, function(k) k$Lat))),
    Lon=conv(unlist(lapply(data_all, function(k) k$Lon)))
  )
    bad <- is.na(NAV$VertSpeed)
    NAV <- NAV[!bad,]  
  
  }
  
  #############################
  
  #calculate distance traveled and glider speed
  # Identify each inflacting down (Navstate=110) time and position for calculation
  index110 <- which(NAV$NavState %in% 110)
  NavTime110t <- NAV$time[index110]
  NavTime110 <- NavTime110t[!is.na(NavTime110t)]
  
  dist <- distGeo(matrix(c(NAV$Lon[index110], NAV$Lat[index110]),nrow=length(NavTime110),ncol=2))
  distsum<- rep(NA, length(dist))
  speed<- rep(NA, length(dist))
  maxdist <- ifelse(exists('NAVold'), max(NAVold$distkm, na.rm = TRUE), 0)
  for (j in c(1:length(dist))){
    distsum[j] <- sum(dist[1:j])/1000 + maxdist
    speed[j] <- dist[j]/as.numeric(NavTime110[j+1]-NavTime110[j],units='secs')
  }
  
  timedist <- NavTime110[2:length(NavTime110)]
  speed_goodi <- which(speed!=0)
  speed_good <- speed[speed_goodi]
  timespeed <- timedist[speed_goodi]
  
  indexspeed <- rep(NA, length(timespeed))
  indexdist <- rep(NA, length(timedist))
  for (j in c(1:length(timespeed))){
    indexspeed[j] <- which.min(abs(NAV$time - timespeed[j]))
  }
  for (j in c(1:length(timedist))){
    indexdist[j] <- which.min(abs(NAV$time - timedist[j]))
  }
  
  
  # put 2 new variables in glider data frame
  NAV$speedms <- rep(NA, length(NAV$time))
  NAV$distkm <-  rep(NA, length(NAV$time))
  NAV$speedms[indexspeed] <- speed_good
  NAV$distkm[indexdist] <- distsum[!is.na(distsum)]
  
  
  
  #############################
  
  ### READ PLD FILES
  
  filelistsci <- list.files(path = dir, pattern = '*.pld1.sub.*.gz')
  okfilesci <- !grepl(pattern = '*Copy.gz', x = filelistsci) #omit these files, creates error below
  filesci <- if(length(okfilesci) != 0) paste(dir, as.list(filelistsci[okfilesci]), sep = '') 

  {if(exists('filesci') & length(filesci) != 0){
  # to put the files in the right order
    fileidx <-   as.numeric(unlist(lapply(filesci, function(x) unlist(strsplit(x, '.', fixed=TRUE))[6])))
    o <- order(fileidx)
    data_allsci <- lapply(filesci[o], read.table, sep = ";", header = TRUE)
  
  #sci profile Numbers
  profileNumSci <- unlist(lapply(filesci, function(x) {
    tmp <- unlist(strsplit(x, '.', fixed=TRUE))[6]
    len <- dim(read.table(x, sep=';', header=TRUE))[1]
    rep(tmp, len)
  }))
  profileNumSci <- sort(as.numeric(profileNumSci))
  
  # to read the time in the right format
  
  time_tmpsci <- unlist(lapply(data_allsci, function(k) k$PLD_REALTIMECLOCK))
  timesci <- as.POSIXct(time_tmpsci,format='%d/%m/%Y %H:%M:%S',tz='UTC')
  timesci[timesci < as.POSIXct('2010-01-01')] <- NA
  #remove 2018-07-12 dates see lines 94-96 for gliderfirm[name,miss]
  for(i in 1:length(gliderfirmname)){
    if(glider == gliderfirmname[i] & missionnum > gliderfirmmiss[i]){
      timesci[timesci < as.POSIXct('2018-07-14')] <- NA
    }
  }
  
  #calculate distance traveled and glider speed
  LonT <- unlist(lapply(data_allsci, function(k) k$NAV_LONGITUDE))
  Lond <- conv(LonT)
  LatT <- unlist(lapply(data_allsci, function(k) k$NAV_LATITUDE))
  Latd <- conv(LatT)
 
  # to put everything in a dataframe where all the dive are together
  # PLD with CTD and ecopuck
  # check if there is an eco puck variable (chl, bb or cdom) and a physical variable, (temp, cond, press)
  if('FLBBCD_CHL_COUNT' %in% names(data_allsci[[1]]) && 'GPCTD_TEMPERATURE' %in% names(data_allsci[[1]])){
  PLD <- data.frame(
    profileNumSci = profileNumSci,
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
    #DOF=unlist(lapply(data_allsci, function(k) k$GPCTD_DOF)),
    Conduc=unlist(lapply(data_allsci, function(k) k$GPCTD_CONDUCTIVITY))
  )
  # set 9999.00 values to NA before calculation of other variables
  # think these values are only in PLD files
  bad99 <- PLD == 9999.00
  PLD[bad99] <- NA
  
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
  # use coefficients from calibration files for SBE 43 DO sensor
  if(glider != 'SEA032' & glider != 'SEA035'){
   okcalib <- which(names(oxycalib) == glider)
    cal <- oxycalib[[okcalib]]
    calDate <- as.POSIXct(unlist(lapply(cal, function(x) x[['date']])), origin = '1970-01-01', tz = 'UTC')
    # get first time idx, there was an instance of NA, so find first not na value
    # do it just on first 10 values for now
    t10 <- head(PLD$timesci, 10)  
    ok <- !is.na(t10)
    t1 <- t10[ok][1]
    okcal <- which(t1 > calDate)
    oxycal <- cal[[okcal[length(okcal)]]]

  
  # # oxygen calibration for 24 and 21 oxygen sensor both occured in July 2018
  # if (glider == 'SEA024' | glider == 'SEA021') {
  # 
  # 
  #   if(t1 > as.POSIXct('2018-07-01 00:00:00', tz = 'UTC')){
  #     cal <- cal[[2]]
  #     }
  #   if(t1 < as.POSIXct('2018-07-01 00:00:00', tz = 'UTC')){
  #     cal <- cal[[1]]
  #  }
  # }
  
    DOF <- unlist(lapply(data_allsci, function(k) k$GPCTD_DOF))
    PLD$OxyConc <- sbeO2Hz2Sat(temperature = PLD$Temp, salinity = PLD$Sal, 
                               pressure = PLD$Press, oxygenFrequency = DOF,
                               Soc = oxycal[['Soc']], Foffset = oxycal[['Foffset']], 
                               A = oxycal[['A']], B = oxycal[['B']],
                               C = oxycal[['C']], Enom = oxycal[['Enom']])
    PLD$OxySat <- (PLD$OxyConc / swSatO2(temperature = PLD$Temp, salinity = PLD$Sal))*100
  }
  
  if(glider == 'SEA032' | glider == 'SEA035'){
    rinkodo <- unlist(lapply(data_allsci, function(k) k$AROD_FT_DO))
    nado <- which(rinkodo == 9999.00)
    rinkodo[nado] <- NA
    # 1 ml/l = 10^3/22.391 = 44.661 umol/l from http://ocean.ices.dk/tools/unitconversion.aspx
    rinkooxyconc <-  rinkodo / 44.661
    oxytemp <- unlist(lapply(data_allsci, function(k) k$AROD_FT_TEMP))
    nat <- which(oxytemp == 9999.00)
    oxytemp[nat] <- NA
    PLD$OxySat <- (rinkooxyconc / swSatO2(temperature = oxytemp, salinity = rep(0, length(oxytemp)))) * 100
    #remove 9999.0 from ctd temp and sal for calculation
    #assuming that the 9999.00 are the same for temp and sal
    nactd <- which(PLD$Temp == 9999.00)
    ctdTemp <- PLD$Temp
    ctdTemp[nactd] <- NA
    ctdSal <- PLD$Sal
    ctdSal[nactd] <- NA
    PLD$OxyConc <- (PLD$OxySat * swSatO2(temperature = ctdTemp, salinity = ctdSal))/100
  }

  
  bad <- is.na(PLD$timesci)
  PLD <- PLD[!bad,]
  bad <- is.na(NAV$time)
  NAV <- NAV[!bad,]
  
  ## approx pressure by using depth from navigation for simulation
  if(diff(range(PLD$Press, na.rm = TRUE)) < 5){
    newPress <- approx(x = NAV$time, y = NAV$depth, xout = PLD$timesci, rule = 1)
    PLD$Press <- newPress$y
    }
  } # closes if there is a CTD and ECOpuck, note that it would have to be moved if using profile stuff below
  
  # check if there are porpoise variables
  # unlike CTD and ECOpuck, only need to check one var
  if('PORPOISE_DISK_MOUNTED' %in% names(data_allsci[[1]])){
    PLD <- data.frame(
      profileNumSci = profileNumSci,
      timesci=timesci,
      Lat=Latd,
      Lon=Lond,
      events = unlist(lapply(data_allsci, function(k) k$PORPOISE_EVTS)),
      status = unlist(lapply(data_allsci, function(k) k$PORPOISE_STATUS)),
      diskMounted = unlist(lapply(data_allsci, function(k) k$PORPOISE_DISK_MOUNTED)),
      disksUsage = unlist(lapply(data_allsci, function(k) k$PORPOISE_DISKS_USAGE)),
      disksFull = unlist(lapply(data_allsci, function(k) k$PORPOISE_DISKS_FULL)),
      samplingStatus = unlist(lapply(data_allsci, function(k) k$PORPOISE_SAMPLING_STATUS)),
      acousticRecording = unlist(lapply(data_allsci, function(k) k$PORPOISE_ACOUSTIC_RECORDING))
    )
  } # closes if its a porpoise
  } #closes if there are new files
  
  # if there are no files, then create an empty data frame with all variables for CTD and ecoPUCK setup
  else{
    PLD <- data.frame(
      profileNumSci = NA,
      timesci= NA,
      Lat=NA,
      Lon=NA,
      Depthsci=NA,
      CHL_count=NA,
      CHL_scaled=NA,
      BB_count=NA,
      BB_scaled=NA,
      CDOM_count=NA,
      CDOM_scaled=NA,
      Temp=NA,
      Press=NA,
      Conduc=NA,
      OxySat = NA,
      OxyConc = NA,
      Sal = NA,
      SigTheta = NA
    )
  }
  }
  invisible(list(PLD = PLD, NAV = NAV))
                 #, dnctd = dnctd, upctd = upctd))
  }
