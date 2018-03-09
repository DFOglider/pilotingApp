rm(list=ls())
graphics.off()
library(oce)
#setwd('/Users/BelzileM/Documents/Gliders/Rdata') # CL: not needed

#load functions to convert oxygen frequency to saturation
source('swSatO2.R')
source('sbeO2Hz2Sat.R')

# REAL TIME
##### LOAD NAV DATA #####

#dataDir <- '/Users/BelzileM/Documents/Gliders/Socib/Data/SEA019/M29/all_data/'
dataDir <- 'R:/Shared/Gliders/SEA019/Data/M29/Navigation/logs/'
#ncname <- 'sea019.29.gli.sub.4'
#ncfname <- paste(dataDir,ncname,'.gz', sep = '')
#data <- read.table(ncfname, sep=";")


files_tmp <- dir(path='R:/Shared/Gliders/SEA019/Data/M29/Navigation/logs/',pattern='*.gli.sub.*.gz')
files <- paste(dataDir,as.list(files_tmp),sep = '')

# to put the files in the right order
strl<-nchar(files)
cate<-length(unique(strl))
if(cate==1){
  data_all <- lapply(as.list(files), read.table, sep=";",header=TRUE)
} else if (cate==2){
  first<-which(strl == min(strl), arr.ind = TRUE)
  seco<-which(strl == min(strl)+1, arr.ind = TRUE)
  data_all1 <- lapply(as.list(files[first]), read.table, sep=";",header=TRUE)
  data_all2 <- lapply(as.list(files[seco]), read.table, sep=";",header=TRUE)
  data_all <- c(data_all1,data_all2)
} else {
  first<-which(strl == min(strl), arr.ind = TRUE)
  seco<-which(strl == min(strl)+1, arr.ind = TRUE)
  third<-which(strl == min(strl)+2, arr.ind = TRUE)
  data_all1 <- lapply(as.list(files[first]), read.table, sep=";",header=TRUE)
  data_all2 <- lapply(as.list(files[seco]), read.table, sep=";",header=TRUE)
  data_all3 <- lapply(as.list(files[third]), read.table, sep=";",header=TRUE)
  data_all <- c(data_all1,data_all2,data_all3)
}
#Yo num
profileNum <- unlist(lapply(files, function(x) {
      tmp <- unlist(strsplit(x, '.', fixed=TRUE))[5]
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

# to put everything in a dataframe where all the dive are together
glider <- data.frame(
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


##### LOAD SCI DATA #####
dataDirsci <- 'R:/Shared/Gliders/SEA019/Data/M29/Payload/logs/logs/'
filesci_tmp <- dir(path='R:/Shared/Gliders/SEA019/Data/M29/Payload/logs/logs/',pattern='*.pld1.sub.*.gz')
filesci <- paste(dataDirsci,as.list(filesci_tmp),sep = '')

# to put the files in the right order
strlsci<-nchar(filesci)
catesci<-length(unique(strlsci))
if(catesci==1){
  data_allsci <- lapply(as.list(filesci), read.table, sep=";",header=TRUE)
} else if (cate==2){
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
LonT <- unlist(lapply(data_allsci, function(k) k$NAV_LONGITUDE))
LonT[LonT == 0] <- NA
Lontmp <-  sub("\\$", "", sub('(.{3})', '\\1 ', LonT))


LatT <- unlist(lapply(data_allsci, function(k) k$NAV_LATITUDE))
LatT[LatT == 0] <- NA
Lattmp <-  sub("\\$", "", sub('(.{2})', '\\1 ', LatT))



# to put everything in a dataframe where all the dive are together
PLD <- data.frame(
  timesci=timesci,
  Lat=unlist(lapply(data_allsci, function(k) k$NAV_LATITUDE)),
  Lon=unlist(lapply(data_allsci, function(k) k$NAV_LONGITUDE)),
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
# use coefficients from calibration files located in
# R:/Shared/Gliders/SEA019/Calibration_files/43-3338 Oxygen cal.pdf [as of 2018-03-08]
PLD$OxySat <- sbeO2Hz2Sat(temperature = PLD$Temp, salinity = PLD$Sal, 
                          pressure = PLD$Press, oxygenFrequency = PLD$DOF,
                          Soc = 3.2305e-4, Foffset = -830.74, A = -3.4562e-3, B = 1.1709e-4,
                          C = -1.7302e-6, Enom = 0.036)

#save(list = ls(all = TRUE), file= "currentMission.RData")
save('data_all', 'glider','data_allsci','PLD', file= "R:/Shared/Gliders/SEA019/Data/M29/currentMission.RData")

