rm(list=ls())
library(oce)
library(ocedata)
library(readxl)
library(geosphere)
load('sx_spline.rda')
options(oceEOS='unesco') # prevent error for calculated values using swSigmaTheta, etc
datadir <- './data'

source('swSatO2.R') # for use in sbeO2Hz2Sat.R
source('readSeaExplorerRealTime.R') # read in real time seaExplorer data
source('oxygenCalibrationCoefficients2.R') # used to convert oxygen from units of Hz to ml/l

# read in mission tracking sheet
file <- 'GliderMission.xlsx'
gm <- data.frame(read_xlsx(file, skip = 1))
# remove \\. from headers for easier use
names(gm) <- gsub('\\.', '', names(gm))
# only want rows that have a glider and mission
ok <- !is.na(gm$Glider) & !is.na(gm$Mission)
gm <- gm[ok, ]
# pull out information about oxygen calibration coeff
oxycalibMeta <- data.frame(serialNumber = unlist(lapply(oxycalib, function(k) k[['serialNumber']])),
                           calibrationDate = unlist(lapply(oxycalib, function(k) k[['calibrationDate']])))

input <- list()
input$Glider <- 'SEA022'
input$Mission <- '000053'

# get oxygen coefficients, if applicable
okgm <- gm$Glider == input$Glider & gm$Mission %in% (as.numeric(input$Mission) + c(0, 1)) # in attempts to capture simulations
gmcurrent <- gm[okgm, ]
if(dim(gmcurrent)[1] != 0){ # meaning the sheet has been updated
    okoxycalib <- oxycalibMeta$serialNumber == gmcurrent$GPCTDDOSN & oxycalibMeta$calibrationDate == gsub(' ', '', gmcurrent$GPCTDDOcaldate)
    if(!all(is.na(okoxycalib))){
        if(!all(!okoxycalib)){ # there is a SBE43 attached
            currentCalibration <- oxycalib[okoxycalib][[1]][['calibrationCoefficients']]
        } else { # another type oxygen sensor
            currentCalibration <- NULL
        }
    } else { # no oxygen sensor
        currentCalibration <- NULL
    }
} else { # sheet has not been updated
    currentCalibration <- NULL
}

data <- readSeaExplorerRealTime(datadir = datadir, 
                                glider = input$Glider, 
                                mission = input$Mission,
                                oxygenCalibCoef = currentCalibration)
PLD <- data$PLD
NAV <- data$NAV
output <- data.frame(time = PLD$timesci,
                     pressure = PLD$Press,
                     temperature = PLD$Temp,
                     salinity = PLD$Sal,
                     oxygenConcentration = PLD$OxyConc,
                     oxygenSaturation = PLD$OxySat,
                     rinkoOxygenConcentration = PLD$rinkOxygenConcentration,
                     AROD_FT_DO = PLD$AROD_FT_DO,
                     AROD_FT_TEMP = PLD$AROD_FT_TEMP)
write.csv(x = output,
          file = './sandbox/cl/SEA022M053_realTime.csv',
          row.names = FALSE)
