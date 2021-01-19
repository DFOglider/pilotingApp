rm(list=ls())
library(shiny)
library(oce)
library(ocedata)
library(leaflet)
library(RCurl)
library(geosphere)
library(XML)
library(readxl)
load('sx_spline.rda')
data(ctd) # for initial plotProfile tests, delete later
options(oceEOS='unesco') # prevent error for calculated values using swSigmaTheta, etc

source('addMouseCoordinates.R') # from mapview package, some had issues with mapview
source('readSeaExplorerRealTime.R') # read in real time seaExplorer data
source('readSeaExplorerKml.R') # gets lon lat from kml file
source('readSeaExplorerKmlGlimpse.R')
source('oxygenCalibrationCoefficients2.R') # used to convert oxygen from units of Hz to ml/l
source('swSatO2.R') # for use in sbeO2Hz2Sat.R
source('sbeO2Hz2Sat.R') # calculate oxygen from Hz to ml/l from seaBird instrument
source('downloadData.R') # obtain glidernames and missions from ftp and downloads
source('findProfilesSOCIB.R') # finds downcast and upcasts from a yo
source('arrowShaftCoordinates.R') # draw arrows on leaflet map
source('compass2polar.R') # convert compass heading to polar degrees
source('bearing.R') #calculate bearing between two points
source('readMsn.R')

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
# make some icons for map
returnIcon <- makeIcon(iconUrl = 'icon1.bmp',
                       iconWidth = 13,
                       iconHeight = 13)

mooringIcon <- makeIcon(iconUrl = 'anchor.png',
                        iconWidth = 20,
                        iconHeight = 20)
# convert lat long to decimal
# from readSeaExplorerRealTime.R
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

# log2 adjusted
log2adjusted <- function(x){
    x[x==0] <- 0.5
    log2(x)
}

mardef <- c(3.1, 3.1, 1.1, 2.1) # default margins
marcm <- c(3.1, 3.1, 1.1, 6.1) # color bar with zlab margins

# NSCM location
nscmlon <- conv(-6309.7860)
nscmlat <- conv(4414.73)

# HFX viking buoy
hfxviklon <- conv(-6318.40)
hfxviklat <- conv(4420.85)

#deployment/recovery location
drlon <- -63.406418
drlat <- 44.520789

# halifax line stations (HL01 - HL07, HL3.3, HL5.5, HL6.3, HL6.7)
hfxlon <- c(-63.450000, -63.317000, -62.883000, -62.451000, -62.098000, -61.733000, -61.393945, -62.7527, -61.8326, -61.6167, -61.5167)
hfxlat <- c(44.400001, 44.267001, 43.883001, 43.479000, 43.183000, 42.850000, 42.531138, 43.7635, 42.9402, 42.7333, 42.6183)

# piloting waypoints
gllondmm <- c(-6305.5912, -6241.3334, -6153.4760, -6317.2030)
gllatdmm <- c(4403.3124, 4341.1479, 4258.6090, 4421.3538)

gllon <- conv(gllondmm)
gllat <- conv(gllatdmm)

# bonavista line stations (BB01 - BB15)
bblon <- c(-52.967, -52.750, -52.650, -52.400, -52.067, -51.830, -51.542, -51.280, -51.017, -50.533, -50.017, -49.500, -49, -48.472, -47.947)
bblat <- c(48.7300, 48.800, 48.833, 48.917, 49.025, 49.100, 49.190, 49.280, 49.367, 49.517, 49.683, 49.850, 50.000, 50.177, 50.332)

# halifax shipping lane boundaries
load('shippingBoundaries.rda')

# define ftp urls
ftpUrl <- c('ftp://ftp.dfo-mpo.gc.ca/glider',
             'ftp://dfoftp.ocean.dal.ca/pub/dfo/glider',
            'ftp://dfoftp.ocean.dal.ca/pub/dfo/glimpse/')
ftpUrlNames <- c('DFO', 'DAL', 'DAL-GLIMPSE')
names(ftpUrl) <- ftpUrlNames
# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Glider Data"),

  fluidRow(
      column(2, wellPanel(
        selectInput(inputId = 'ftp',
                    label = 'Choose an ftp site',
                    choices = ftpUrl,
                    selected = ftpUrl[3]),
        actionButton(inputId = 'ftpChoice',
                     label = 'Go !'),
        # once ftp button clicked, then everything else
        # pops up
        conditionalPanel(
          condition = "input.ftpChoice != 0",
          uiOutput(outputId = 'Gliders'),
          # selectInput(inputId = 'Glider',
          #            label = 'Choose a glider',
          #            choices = gliderdirnames), #gliderdirnames from downloadData.R
         uiOutput(outputId = 'Missions'),
         actionButton(inputId = 'download',
                      label = 'Download and load data'),

         # conditional panel for plots tab
         conditionalPanel(
           condition = "input.tabs == 'Plots'",
           uiOutput(outputId = 'Vars')
         ),
        #conditional panels for navigation in plots tab
        conditionalPanel(
          condition = "input.Var == 'Navigation' & input.tabs == 'Plots'",
          actionButton("resetNav", "Reset plot")),

          conditionalPanel(
            condition = "input.tabs == 'TS'",
            actionButton("resetTS", "Reset plot")),
  
         conditionalPanel(
          condition="input.Var=='Navigation' & input.tabs == 'Plots'",
          radioButtons(inputId = "NavVar",
                  label = "Variables:",
                  choices = c('Depth' = 'depth',
                              'Altimeter' = 'altimeter',
                              'Alarm'='alarm',
                              'Pitch'='Pitch',
                              'Vertical Speed'='VertSpeed',
                              'Battery Voltage'='BatterieVolt',
                              'Battery Percentage'='BatteriePerc',
                              'Internal Temperature'='Temperature',
                              'Internal Pressure'='int_pres',
                              'Distance'='distkm',
                              'Speed'='speedms',
                              'Heading'='Heading',
                              'Ballast'='BallastPos',
                              'Angular'='AngPos',
                              'Linear'='LinPos',
                              'Roll'='Roll',
                              'Yo Numbers'='profileNumber'),
                  selected = 'Pitch'),
          uiOutput('navScaleBar')),

        #conditional panels for science in plots tab
        conditionalPanel(
          condition = "(input.Var == 'Science' || input.Var == 'Science2') && input.tabs == 'Plots'",
          #condition = "input.Var == 'Science' & input.tabs == 'Plots'",
          actionButton("resetSci", "Reset plot")),
        # only GPCTD
        conditionalPanel(
          condition="input.Var == 'Science' && input.tabs == 'Plots'",
            radioButtons(inputId = "SciVar",
                      label = "Variables:",
                      choices = c('Temperature'='Temp',
                                  'Conductivity'='Cond',
                                  'Salinity'='Sal',
                                  'Density'='Dens',
                                  'Sound Speed' = 'SoundSpeed',
                                  #'Oxygen Frequency'='DOF',
                                  'Oxygen Concentration' = 'OxyConc',
                                  'Oxygen Saturation' = 'OxySat',
                                  'Chlorophyl'='CHL_scaled',
                                  'CDOM'='CDOM_scaled',
                                  'BB_700nm'='BB_scaled'),
                      selected = 'Temp'),
            uiOutput('sciScaleBar')
          ),
        # GPCTD and LEGATO
        conditionalPanel(
          condition="input.Var == 'Science2' && input.tabs == 'Plots'",
          radioButtons(inputId = "SciVar2",
                       label = "Variables:",
                       choices = c('Temperature'='Temp',
                                   'TemperatureLegato' = 'temperatureLegato',
                                   'Conductivity'='Cond',
                                   'ConductivytLegato' = 'conductivityLegato',
                                   'Salinity'='Sal',
                                   'SalinityLegato' = 'salinityLegato',
                                   'Density'='Dens',
                                   'Sound Speed' = 'SoundSpeed',
                                   #'Oxygen Frequency'='DOF',
                                   'Oxygen Concentration' = 'OxyConc',
                                   'Oxygen Saturation' = 'OxySat',
                                   'Chlorophyl'='CHL_scaled',
                                   'CDOM'='CDOM_scaled',
                                   'BB_700nm'='BB_scaled'),
                       selected = 'Temp'),
          uiOutput('sciScaleBar2')
          ),
        # conditionalPanel(
        #   condition = "input.Var == 'Science' || input.Var == 'Science2'",
        #   uiOutput('sciScaleBar')
        # ),
        #conditional panels for porpoise in plots tab
        conditionalPanel(
            condition = "input.Var == 'Porpoise' & input.tabs == 'Plots'",
            actionButton("resetPorp", "Reset plot")),
        
        conditionalPanel(
          condition="input.Var=='Porpoise' & input.tabs == 'Plots'",
          radioButtons(inputId = "porpVar",
                       label = "Variables:",
                       choices = c('Events'='events',
                                   'Status'='status',
                                   'Disk Mounted'='diskMounted',
                                   'Disks Usage'='disksUsage',
                                   'Disks Full' = 'disksFull',
                                   'Sampling Status' = 'samplingStatus',
                                   'Acoustic Recording'='acousticRecording'),
                       selected = 'disksUsage'),
          uiOutput('porpScaleBar'))
         #) # closes download button conditionalPanel
        ) # closes ftp button conditionalPanel
    ) #closes well panel
    ), # closes fluidRow
    # Main panel for displaying outputs ----
    column(10,
      tabsetPanel(id = 'tabs', type = 'tabs',
        tabPanel("Plots",
        #column(10,
               plotOutput("plot1",dblclick="plot_click",brush = brushOpts(id="plot_brush",
                                             direction="x",
                                             resetOnNew = TRUE),
                                            height="310px"),
               plotOutput("plot2", height="310px")),
      tabPanel("Map",
        leafletOutput("map", height = '620px')),
      tabPanel("TS",
                  plotOutput("plotpressure", dblclick="plot_click",
                             brush = brushOpts(id = 'plot_brush',
                                               direction = 'x',
                                               resetOnNew = TRUE),
                             height = '310px'
                             #width = '450px'
                             ),
                  plotOutput("TS", dblclick="plot_click_TS",
                             brush = brushOpts(id = 'plot_brush_TS',
                                               direction = 'xy',
                                               resetOnNew = TRUE),
                             height = '500px',
                             width = '500px'
                             ))
      ) #closes tabset
    ) #closes column
    ) #closes fluidRow
) #closes ui






# Define server
server <- function(input, output) {
  state <- reactiveValues()
  # put observe event when button for ftp choice
  observeEvent(input$ftpChoice, {
    output$Gliders <- renderUI({
      gliders <- getGliderNames(ftpUrl = input$ftp)
      # set names of gliders from gliderdirnames
      glidernames <- unlist(lapply(gliders, function(k) ifelse(k == 'SEA019', 'SEA019 - Mira',
                                                                      ifelse(k == 'SEA021', 'SEA021 - Skye',
                                                                             ifelse(k == 'SEA022', 'SEA022 - Mersey',
                                                                                    ifelse(k == 'SEA024', 'SEA024 - Margaree',
                                                                                           ifelse(k == 'SEA032', 'SEA032 - LaHave', 
                                                                                                  ifelse(k == 'SEA035', 'SEA035 - Eva', k))))))))
      
      names(gliders) <- glidernames
       selectInput(inputId = 'Glider',
                  label = 'Choose a glider',
                  choices = gliders)
    })
    output$Missions <- renderUI({
      missions <- getMissions(ftpUrl = input$ftp, glider = input$Glider)
      selectInput(inputId = 'Mission', label = 'Choose a mission', choices = missions,
                  selected=tail(missions, 1))
  })
  })
  # select input for mission based on selected glider


  # download data and load when actionButton clicked
  # make plots too
  observeEvent(input$download,{
    # download and process data
    downloadData(ftpUrl = input$ftp, datadir = datadir, glider = input$Glider, mission = input$Mission)
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
    # find bad conductivity values and make pressure and salinity NA
    # this will be enough for plotting purposes
    badConduc <- which(PLD$Conduc > 9)
    if(length(badConduc) != 0){
        PLD$Press[badConduc] <- NA
        PLD$Sal[badConduc] <- NA
    }
    
    ## UIOUTPUT for choice of data set
    output$Vars <- renderUI({
      # similar to readSeaExplorerRealTime.R code
      # for CTD/ eco puck set up check for two variables (CHL_count and Temp)
      # for porpoise, check for one variable (diskMounted)
      # add or statement incase ecopuck missing
      if(('CHL_count' %in% names(PLD) | 'Temp' %in% names(PLD)) & !'temperatureLegato' %in% names(PLD)){
        choices <- c('Navigation'='Navigation','Science'='Science')
      }
      if(('CHL_count' %in% names(PLD) | 'Temp' %in% names(PLD)) & 'temperatureLegato' %in% names(PLD)){
        choices <- c('Navigation'='Navigation','Science'='Science2')
      }
      if('diskMounted' %in% names(PLD)){ # we'll have to add another if statement if legato gets put on with porpoise
        choices <- c('Navigation'='Navigation','Porpoise'='Porpoise')
      }
      print(choices)
      selectInput(inputId="Var",
               label="Data Set:",
               choices = choices,
               selected = choices[1])
    })

    profileNumber <- unique(NAV$profileNumber)
    profileTimes <- glon <- glat <- gdeshead <- NULL
    for (pi in seq_along(profileNumber)) {
      ok <- profileNumber[pi] == NAV$profileNumber & NAV$Lon != 0 & NAV$Lat != 0
      profileTimes <- c(profileTimes, NAV$time[ok][1])
      #glon <- c(glon, NAV$Lon[ok][1])
      #glat <- c(glat, NAV$Lat[ok][1])
      glon <- c(glon, tail(NAV$Lon[ok], 1))
      glat <- c(glat, tail(NAV$Lat[ok], 1))
      heading <- NAV$DesiredHeading[ok][1] #- NAV$Declination[ok][1] # what DH calls 'geographical' heading
      gdeshead <- c(gdeshead, heading)
    }
    gdeshead[gdeshead < 0] <- NA
    profileTimes <- numberAsPOSIXct(profileTimes)
    # calculate bearing
    # remove repeat glon glat values
    ok <- diff(glon) != 0 & diff(glat) != 0
    glonb <- glon[ok]
    glatb <- glat[ok]
    bearingTime <- profileTimes[ok]
    if(length(glonb) != 0){
      bearingTime <- numberAsPOSIXct(bearingTime)[1:(length(bearingTime)-1)]
      gbearing <- bearing(lon = glonb, lat = glatb)}
    #dnctd <- data$dnctd
    #upctd <- data$upctd
    
    ## kml file
    #kmlfile <- paste(datadir, input$Glider, input$Mission, paste0(input$Glider,'.', input$Mission,'.trk.kml'), sep = '/')
    kmlfile <- list.files(path = paste(datadir, input$Glider, input$Mission, sep = '/'), pattern = paste0(tolower(input$Glider), '\\.\\w+\\.gps\\.all\\.csv'), full.names = TRUE)
    #kmlfile <- paste(datadir, as.list(list.files(path = datadir, pattern = paste0(tolower(glider), '\\.\\w+\\.gps\\.all\\.csv'))), sep = '')
    {if(file.exists(kmlfile)){
      kmlcoord <- readSeaExplorerKmlGlimpse(datadir = datadir, glider = input$Glider, mission = input$Mission)
      okkml <- !is.na(kmlcoord$lon)
      kmlLon <- kmlcoord$lon[okkml]
      kmlLat <- kmlcoord$lat[okkml]
      if('timestamp' %in% names(kmlcoord)){
         kmlTime <- kmlcoord$time[okkml] 
      } else {
          kmlTime <- rep(NA, length(kmlLon))
      }
      
    } else{
      # 0,0 for plotting purposes
      kmlLon <- 0
      kmlLat <- 0
      kmlTime <- NA
    }}

    ## msn file
    msnfile <- paste(datadir, input$Glider, paste0(input$Glider,input$Mission,'.msn'), sep = '/')
    {if(file.exists(msnfile)){
      msn <- readMsn(file = msnfile)
    }
      else{
        # fake data for plotting purposes
        msn <- data.frame(
          wp = 'fake',
          lon = -63.40650,
          lat = 44.52083,
          rval = 0
        )
      }}
    # profile numbers
    ## profiles <- sort(unique(round(unlist(lapply(c(dnctd,upctd), function(k) k@metadata[['station']])))))
    profiles <- profileNumber
    output$numDncst <- renderUI({
      h5(paste0(length(dnctd),' downcasts detected'))
    })
    output$numUpcst <- renderUI({
      h5(paste0(length(upctd),' upcasts detected'))
    })
    # scaleBar for navigation plots
    output$navScaleBar <- renderUI({
        rng <- switch(input$NavVar,
                      'depth' = c(0,700),
                      'altimeter' = c(0, 60),
                      'alarm' = log2adjusted(c(0.5, 2^20)),
                      'Pitch' = c(-80,50),
                      'VertSpeed' = c(-50, 50),
                      'BatterieVolt' = c(24, 30),
                      'BatteriePerc' = c(0, 100),
                      'Temperature' = c(0,30),
                      'int_pres' = c(6.9e4, 8.1e4),
                      'distkm' = c(0, 700),
                      'speedms' = c(0,2),
                      'Heading' = c(0, 360),
                      'BallastPos' = c(-500, 500),
                      'AngPos' = c(-70, 70),
                      'LinPos' = c(0, 100),
                      'Roll' = c(-20, 30),
                      'profileNumber' = c(0, 1500))
        value <- switch(input$NavVar,
                        'depth' = rev(c(max(NAV$altHit,na.rm = TRUE), 0)),
                        'altimeter' = c(0, 100),
                        'alarm' = c(-1, 20),
                        'Pitch' = c(-80,50),
                        'VertSpeed' = c(-50, 50),
                        'BatterieVolt' = c(24, 30),
                        'BatteriePerc' = c(0, 100),
                        'Temperature' = c(0,26),
                        'int_pres' = c(6.9e4, 8.1e4),
                        'distkm' = c(0, 1000),
                        'speedms' = c(0,1),
                        'Heading' = c(0, 360),
                        'BallastPos' = c(-500, 500),
                        'AngPos' = c(-70, 70),
                        'LinPos' = c(0, 100),
                        'Roll' = c(-20, 30),
                        'profileNumber' = c(0, 1200))
        step <- switch(input$NavVar,
                       'depth' = 5,
                       'altimeter' = 1,
                       'alarm' = 1,
                       'Pitch' = 5,
                       'VertSpeed' = 5,
                       'BatterieVolt' = 0.05,
                       'BatteriePerc' = 1,
                       'Temperature' = 1,
                       'int_pres' = 50,
                       'distkm' = 50,
                       'speedms' = 0.1,
                       'Heading' = 10,
                       'BallastPos' = 25,
                       'AngPos' = 5,
                       'LinPos' = 5,
                       'Roll' = 5,
                       'profileNumber' = 50)
        sliderInput("navLimits", "Choose limits:", min = rng[1], max = rng[2],
                    value = value, step = step, animate = FALSE)
        
    })
    # scaleBar for science plots
    output$sciScaleBar <- renderUI({
      rng <- switch(input$SciVar,
                    'Temp' = c(-2, 22),
                    'temperatureLegato' = c(-2,22),
                    'Sal' = c(29, 35.5),
                    'Cond' = c(0,7),
                    'Dens' = c(20, 28),
                    'SoundSpeed' = c(1425, 1575),
                    'CHL_scaled' = c(-.02,5),
                    'CDOM_scaled' = c(-2,12),
                    'BB_scaled' = c(-0.001, 0.003) * 1000,
                    'DOF' = c(2000, 5000),
                    'OxyConc' = c(0,10),
                    'OxySat' = c(0,120))
      value <- switch(input$SciVar,
                      'Temp' = unname(quantile(PLD$Temp, probs = c(0.01, 0.98), na.rm = TRUE)),
                      'Sal' = unname(quantile(PLD$Sal, probs = c(0.02, 0.98),  na.rm = TRUE)),
                      'Cond' = unname(quantile(PLD$Conduc, probs = c(0.02, 0.98), na.rm = TRUE)),
                      'Dens' = unname(quantile(PLD$SigTheta, probs = c(0.02, 0.98), na.rm = TRUE)),
                      'SoundSpeed' = unname(quantile(PLD$SoundSpeed, probs = c(0.02, 0.98), na.rm = TRUE)),
                      'CHL_scaled' = unname(quantile(PLD$CHL_scaled, probs = c(0.01, 0.99), na.rm = TRUE)),
                      'CDOM_scaled' = unname(quantile(PLD$CDOM_scaled, probs = c(0.01, 0.99), na.rm = TRUE)),
                      'BB_scaled' = unname(quantile(PLD$BB_scaled, probs = c(0.01, 0.99), na.rm = TRUE)) * 1000,
                      'DOF' = unname(quantile(PLD$DOF, probs = c(0.01, 0.97), na.rm = TRUE)),
                      'OxyConc' = unname(quantile(PLD$OxyConc, probs = c(0.01, 0.97), na.rm = TRUE)),
                      'OxySat' = unname(quantile(PLD$OxySat, probs = c(0.01, 0.97), na.rm = TRUE)))
      step <- switch(input$SciVar,
                     'Temp' = 0.5,
                     'Sal' = 0.1,
                     'Cond' = 0.01,
                     'Dens' = 0.1,
                     'SoundSpeed' = 10,
                     'CHL_scaled' = 0.1,
                     'CDOM_scaled' = 0.1,
                     'BB_scaled' = 0.0005 * 100,
                     'DOF' = 100,
                     'OxyConc' = 0.5,
                     'OxySat' = 1)
      # deal with values that vary little during simulation
      if(diff(value) < 5*step){value[2] <- value[2] + 5*step}
      sliderInput("sciLimits", "Choose colorbar limits:", min = rng[1], max = rng[2],
                  value = value, step = step, animate = FALSE)

    })
    output$sciScaleBar2 <- renderUI({
      rng <- switch(input$SciVar2,
                    'Temp' = c(-2, 22),
                    'temperatureLegato' = c(-2,22),
                    'Sal' = c(29, 35.5),
                    'salinityLegato' = c(29, 35.5),
                    'Cond' = c(0,7),
                    'conductivityLegato' = c(0,7) * 10,
                    'Dens' = c(20, 28),
                    'SoundSpeed' = c(1425, 1575),
                    'CHL_scaled' = c(-.02,5),
                    'CDOM_scaled' = c(-2,12),
                    'BB_scaled' = c(-0.001, 0.003) * 1000,
                    'DOF' = c(2000, 5000),
                    'OxyConc' = c(0,10),
                    'OxySat' = c(0,120))
      value <- switch(input$SciVar2,
                      'Temp' = unname(quantile(PLD$Temp, probs = c(0.01, 0.98), na.rm = TRUE)),
                      'temperatureLegato' = unname(quantile(PLD$temperatureLegato, probs = c(0.01, 0.98), na.rm = TRUE)),
                      'Sal' = unname(quantile(PLD$Sal, probs = c(0.02, 0.98),  na.rm = TRUE)),
                      'salinityLegato' = unname(quantile(PLD$salinityLegato, probs = c(0.02, 0.98),  na.rm = TRUE)),
                      'Cond' = unname(quantile(PLD$Conduc, probs = c(0.02, 0.98), na.rm = TRUE)),
                      'conductivityLegato' = unname(quantile(PLD$conductivityLegato, probs = c(0.02, 0.98),  na.rm = TRUE)),
                      'Dens' = unname(quantile(PLD$SigTheta, probs = c(0.02, 0.98), na.rm = TRUE)),
                      'SoundSpeed' = unname(quantile(PLD$SoundSpeed, probs = c(0.02, 0.98), na.rm = TRUE)),
                      'CHL_scaled' = unname(quantile(PLD$CHL_scaled, probs = c(0.01, 0.99), na.rm = TRUE)),
                      'CDOM_scaled' = unname(quantile(PLD$CDOM_scaled, probs = c(0.01, 0.99), na.rm = TRUE)),
                      'BB_scaled' = unname(quantile(PLD$BB_scaled, probs = c(0.01, 0.99), na.rm = TRUE)) * 1000,
                      'DOF' = unname(quantile(PLD$DOF, probs = c(0.01, 0.97), na.rm = TRUE)),
                      'OxyConc' = unname(quantile(PLD$OxyConc, probs = c(0.01, 0.97), na.rm = TRUE)),
                      'OxySat' = unname(quantile(PLD$OxySat, probs = c(0.01, 0.97), na.rm = TRUE)))
      step <- switch(input$SciVar2,
                     'Temp' = 0.5,
                     'temperatureLegato' = 0.5,
                     'Sal' = 0.1,
                     'salinityLegato' = 0.1,
                     'Cond' = 0.01,
                     'conductivityLegato' = 0.01 * 10,
                     'Dens' = 0.1,
                     'SoundSpeed' = 10,
                     'CHL_scaled' = 0.1,
                     'CDOM_scaled' = 0.1,
                     'BB_scaled' = 0.0005 * 100,
                     'DOF' = 100,
                     'OxyConc' = 0.5,
                     'OxySat' = 1)
      # deal with values that vary little during simulation
      if(diff(value) < 5*step){value[2] <- value[2] + 5*step}
      sliderInput("sciLimits2", "Choose colorbar limits:", min = rng[1], max = rng[2],
                  value = value, step = step, animate = FALSE)
      
    })
    # scaleBar for porpoise data
    output$porpScaleBar <- renderUI({
        rng <- switch(input$porpVar,
                      'events' = c(0,1), # not sure on this yet
                      'status' = c(0,1),
                      'diskMounted' = c(0,1),
                      'disksUsage' = c(0,100),
                      'disksFull' = c(0,1),
                      'samplingStatus' = c(0,1),
                      'acousticRecording' = c(0,1))
        value <- switch(input$porpVar,
                        'events' = c(0,1),
                        'status' = c(0,1),
                        'diskMounted' = c(0,1),
                        'disksUsage' = range(PLD$disksUsage[PLD$disksUsage != 9999]),
                        'disksFull' = c(0,1),
                        'samplingStatus' = c(0,1),
                        'acousticRecording' = c(0,1))
        step <- switch(input$porpVar,
                       'events' = 0.5,
                       'status' = 0.5,
                       'diskMounted' = 0.5,
                       'disksUsage' = 0.5,
                       'disksFull' = 0.5,
                       'samplingStatus' = 0.5,
                       'acousticRecording' = 0.5)
        if(input$porpVar == 'disksUsage' & diff(value) < 5*step){value[2] <- value[2] + 2*step}
        sliderInput("porpLimits", "Choose limits:", min = rng[1], max = rng[2],
                    value = value, step = step, animate = FALSE)
    })
    # plot1 - top plot
    output$plot1 <- renderPlot({
      if (is.null(state$xlim)) {
        #par(mar = marcm)
        #par(xaxs='i',yaxs='i')#tight
        oce.plot.ts(NAV$time, NAV$depth,
                    type="n",
                    ylim=c(max(NAV$altHit,na.rm = TRUE), -5),
                    xlim= range(c(NAV$time, PLD$timesci), na.rm = TRUE),
                    ylab='Depth (m)',xlab='Time',
                    mar=marcm)
        points(NAV$time,NAV$altHit,pch=20,cex = 1, col = "red")
        points(NAV$time, NAV$depth, pch=20,cex = 1, col = "dark blue")
        text(profileTimes, -2, as.character(profileNumber), cex=1)
        grid()
      } else {
        par(mar = marcm)
        #par(xaxs='i',yaxs='i')#tight
        oce.plot.ts(NAV$time, NAV$depth,
                    type = "n",
                    ylim=c(max(NAV$altHit,na.rm = TRUE), -5),
                    xlim = state$xlim,
                    ylab = 'Depth (m)',xlab='Time',
                    mar=marcm)
        points(NAV$time,NAV$altHit,pch=20,cex = 1, col = "red")
        points(NAV$time, NAV$depth, pch=20,cex = 1, col = "dark blue")
        text(profileTimes, -2, as.character(profileNumber), cex=1)
        grid()
      }
  })
    # plot2 - bottom plot
    # TO-DO : use switch argument for navigation plots
    #         to make code shorter
    #         see how science plots are created below
    output$plot2 <- renderPlot({
    if (input$Var == 'Navigation') {
        navdata <- switch(input$NavVar,
                          'depth' = NAV$depth,
                          'altimeter' = NAV$alt,
                          'alarm' = log2adjusted(NAV$alarm),
                          'Pitch' = NAV$Pitch,
                          'VertSpeed' = NAV$VertSpeed,
                          'BatterieVolt' = NAV$BatterieVolt,
                          'BatteriePerc' = NAV$BatteriePerc,
                          'speedms' = NAV$speedms,
                          'distkm' = NAV$distkm,
                          'Temperature' = NAV$Temperature,
                          'Heading' = NAV$Heading,
                          'BallastPos' = NAV$BallastPos,
                          'LinPos' = NAV$LinPos,
                          'AngPos' = NAV$AngPos,
                          'profileNumber' = NAV$profileNumber,
                          'int_pres' = NAV$int_pres,
                          'Roll' = NAV$Roll)
        L <- '['
        R <- ']'
        navzlab <- switch(input$NavVar,
                          'depth' = bquote('Depth'*.(L)*m*.(R)),
                          'altimeter' = bquote('Range from bottom'*.(L)*'m'*.(R)),
                          'alarm' = bquote('Alarm'),
                          'Pitch' = bquote('Pitch'*.(L)*'degrees'*.(R)),
                          'VertSpeed' = bquote('Vertical speed'*.(L)*'m/s'*.(R)),
                          'BatterieVolt' = bquote('Battery voltage'*.(L)*'V'*.(R)),
                          'BatteriePerc' = bquote('Battery percentage'*.(L)*'%'*.(R)),
                          'speedms' = bquote('Glider speed'*.(L)*'m/s'*.(R)),
                          'distkm' = bquote('Distance travelled'*.(L)*'km'*.(R)),
                          'Temperature' = bquote('Internal temperature'*.(L)*degree*'C'*.(R)),
                          'Heading' = bquote('Heading'*.(L)*'degrees'*.(R)),
                          'BallastPos' = bquote('Ballast'*.(L)*'ml'*.(R)),
                          'LinPos' = bquote('Linear'*.(L)*'mm'*.(R)),
                          'AngPos' = bquote('Angular'*.(L)*'degrees'*.(R)),
                          'profileNumber' = bquote('Yo number'),
                          'int_pres' = bquote('Internal pressure'*.(L)*'Pa'*.(R)),
                          'Roll' = bquote('Roll'*.(L)*'degrees'*.(R)))
        #plot var as lines or points
        # to save on code, we'll store everything pertaining to plot styling
        # order will be
        # type ('l'/'p'), col, lwd
        navtype <- switch(input$NavVar,
                          'depth' = c('p', 'dark blue', 1),
                          'altimeter' = c('p','red',1),
                          'alarm' = c('l','blue',2),
                          'Pitch' = c('l', 'black',2),
                          'VertSpeed' = c('l','black',2),
                          'BatterieVolt' = c('l','red',2),
                          'BatteriePerc' = c('l', 'red',2),
                          'Temperature' = c('l','red',2),
                          'int_pres' = c('l', 'blue',2),
                          'distkm' = c('p', 'darkgreen',1),
                          'speedms' = c('p', 'darkgreen',1),
                          'Heading' = c('l','red',2),
                          'BallastPos' = c('l', 'red',2),
                          'AngPos' = c('l', 'red',2),
                          'LinPos' = c('l', 'red',2),
                          'Roll' = c('l', 'blue',2),
                          'profileNumber' = c('l','blue',2))
        {if(input$NavVar == 'alarm'){
            yaxt <- 'n'
        } else {
            yaxt <- 's'
        }}
        {if (is.null(state$xlim) & is.null(state$ylim)) {
            par('pch' = 20)
            oce.plot.ts(NAV$time, navdata,
                        xlim = range(c(NAV$time, PLD$timesci), na.rm = TRUE),
                        ylim = if(input$NavVar == 'depth') {rev(input$navLimits)} else {input$navLimits},
                        type = navtype[1], 
                        col = navtype[2],
                        yaxt = yaxt,
                        xlab = '', ylab = navzlab, mar=marcm, lwd = as.numeric(navtype[3]))
            
        }  else {
            par('pch' = 20)
            oce.plot.ts(NAV$time, navdata, 
                        xlim = state$xlim,
                        ylim = if(input$NavVar == 'depth') {rev(input$navLimits)} else {input$navLimits},
                        type = navtype[1], 
                        col = navtype[2],
                        yaxt = yaxt,
                        xlab = '', ylab = navzlab, mar=marcm, lwd = as.numeric(navtype[3]))
        }
        }
        # go through special cases
        if(input$NavVar == 'alarm'){
            ylab <- c(0, 2^(0:20))
            yat <- -1:20
            axis(side = 2, at = yat, labels = ylab)
        }
        if(input$NavVar == 'Pitch'){
            xpoly <- c(NAV$time[1], NAV$time[length(NAV$time)], NAV$time[length(NAV$time)], NAV$time[1])
            ypoly <- c(15, 15, 25, 25)
            polygon(xpoly, ypoly,
                    col = gray(0.8), border = NA)
            polygon(xpoly, rev(ypoly) * -1, 
                    col = gray(0.8), border = NA)
            lines(NAV$time, navdata, lwd = 2, col = navtype[2])
        }
        if(input$NavVar == 'VertSpeed'){
            xpoly <- c(NAV$time[1], NAV$time[length(NAV$time)], NAV$time[length(NAV$time)], NAV$time[1])
            ypoly <- c(13, 13, 17, 17)
            polygon(xpoly, ypoly,
                    col = gray(0.8), border = NA)
            polygon(xpoly, rev(ypoly) * -1, 
                    col = gray(0.8), border = NA)
            lines(NAV$time, navdata, lwd = 2, col = navtype[2])
        }
        if(input$NavVar == 'BatterieVolt'){
            xpoly <- c(NAV$time[1], NAV$time[length(NAV$time)], NAV$time[length(NAV$time)], NAV$time[1])
            ypoly <- c(24, 24, 26, 26)
            polygon(xpoly, ypoly,
                    col = gray(0.8), border = NA)
            polygon(xpoly, rev(ypoly) * -1, 
                    col = gray(0.8), border = NA)
            lines(NAV$time, navdata, lwd = 2, col = navtype[2])
        }
        if(input$NavVar == 'BatteriePerc'){
            xpoly <- c(NAV$time[1], NAV$time[length(NAV$time)], NAV$time[length(NAV$time)], NAV$time[1])
            ypoly <- sx(c(24, 24, 26, 26))
            polygon(xpoly, ypoly,
                    col = gray(0.8), border = NA)
            polygon(xpoly, rev(ypoly) * -1, 
                    col = gray(0.8), border = NA)
            lines(NAV$time, navdata, lwd = 2, col = navtype[2])
        }
        if(input$NavVar == 'Heading'){
            lines(NAV$time, NAV$DesiredHeading,lwd = 2, col = "blue")
            if(exists('gbearing')) lines(bearingTime, gbearing, lwd = 2, col = "darkgreen")
            legend('topleft',
                   lty = 1,
                   col = c('red', 'blue','darkgreen'),
                   legend = c('glider',
                              'desired',
                              'cog'))
        }
        if(input$NavVar == 'BallastPos'){
            lines(NAV$time, NAV$BallastCmd, lwd = 2, col = 'blue')
        }
        if(input$NavVar == 'AngPos'){
            lines(NAV$time, NAV$AngCmd, lwd = 2, col = 'blue')
        }
        if(input$NavVar == 'LinPos'){
            lines(NAV$time, NAV$LinCmd, lwd = 2, col = 'blue')
        }
        
        grid(lwd = 1)
        par(mar=mardef)
                    
    } else if (input$Var %in% c('Science', 'Science2')) {
        # CL's work for science plots
        # get science data, make color map
        var <- ifelse(input$Var == 'Science', input$SciVar, input$SciVar2)
        data <- switch(var,
                       'Temp' = PLD$Temp,
                       'temperatureLegato' = PLD$temperatureLegato,
                       'Sal' = PLD$Sal,
                       'salinityLegato' = PLD$salinityLegato,
                       'Cond' = PLD$Conduc,
                       'conductivityLegato' = PLD$conductivityLegato,
                       'Dens' = PLD$SigTheta,
                       'SoundSpeed' = PLD$SoundSpeed,
                       'CHL_scaled' = PLD$CHL_scaled,
                       'CDOM_scaled' = PLD$CDOM_scaled,
                       'BB_scaled' = PLD$BB_scaled * 1000,
                       'DOF' = PLD$DOF,
                       'OxyConc' = PLD$OxyConc,
                       'OxySat' = PLD$OxySat)
        #use CL's file for resizable label for biological variables ?
        zlab <- switch(var,
                       'Temp' = resizableLabel('T', axis = 'y'),
                       'temperatureLegato' = resizableLabel('T', axis = 'y'),
                       'Sal' = resizableLabel('S', axis = 'y'),
                       'salinityLegato' = resizableLabel('S', axis = 'y'),
                       'Cond' = resizableLabel('conductivity S/m', axis = 'y'),
                       'conductivityLegato' = resizableLabel('conductivity mS/cm', axis = 'y'),
                       'Dens' = resizableLabel('sigmaTheta', axis = 'y'),
                       'SoundSpeed' = bquote('Sound speed ' * '[' * 'm/s' * ']'),
                       'CHL_scaled' = expression(paste('Chlorophyll [', mu, 'g/l]')),
                       'CDOM_scaled' = expression(paste('CDOM [ppb]')),
                       'BB_scaled' = expression(paste('Backscatter [', 10^3, '/ m sr]')),
                       'DOF' = 'Dissolved Oxygen [Hz]',
                       'OxyConc' = 'Oxygen [ml/l]',
                       'OxySat' = 'Oxygen Saturation [%]')
        if(input$Var == 'Science'){cmlim <- input$sciLimits} else {cmlim <- input$sciLimits2}
        cm <- colormap(data, zlim = cmlim)
        ylabp <- resizableLabel('p', axis = 'y')
        #par(xaxs='i',yaxs='i', mar=mardef)
        par(mar=mardef)
        drawPalette(colormap = cm, zlab = zlab)
        # match top panel, so use range of altHits for ylim
        #                  and nav time for xlim
        if (is.null(state$xlim)) {
          oce.plot.ts(PLD$timesci, PLD$Press, type='p',
              ylim = c(max(c(NAV$altHit, PLD$Press),na.rm = TRUE), -5),
              xlim = range(c(NAV$time, PLD$timesci), na.rm = TRUE),
              pch = 20, col = cm$zcol,
              xlab = '', ylab = '', mar=marcm)

        } else {
          okylim <- PLD$timesci > state$xlim[1] & PLD$timesci < state$xlim[2] #limits for science var
          okylimg <- NAV$time > state$xlim[1] & NAV$time < state$xlim[2] #limits for depth from navigation
          oce.plot.ts(PLD$timesci[okylim], PLD$Press[okylim], type='p',
               ylim = rev(range(c(NAV$altHit[okylimg], PLD$Press[okylim]),na.rm = TRUE)),
               xlim = state$xlim,
               pch = 20, col = cm$zcol[okylim],
               xlab = '', ylab = '', mar=marcm)
        }
        grid()
        mtext(ylabp, side = 2, line = 2)
        par(mar=mardef)


      } else if (input$Var == 'Porpoise') { # closes else if sciVar = science 
      porpData <- switch(input$porpVar,
                         'events' = PLD$events,
                         'status' = PLD$status,
                         'diskMounted' = PLD$diskMounted,
                         'disksUsage' = PLD$disksUsage,
                         'disksFull' = PLD$disksFull,
                         'samplingStatus' = PLD$samplingStatus,
                         'acousticRecording' = PLD$acousticRecording)
      varlim <- switch(input$porpVar,
                       'events' = c(0,1), # not sure on this yet
                       'status' = c(0,1),
                       'diskMounted' = c(0,1),
                       'disksUsage' = c(0,100),
                       'disksFull' = c(0,1),
                       'samplingStatus' = c(0,1),
                       'acousticRecording' = c(0,1))
      varlabel <- switch(input$porpVar,
                         'events' = NA, # not sure on this yet
                         'status' = rev(c('On', 'Off')),
                         'diskMounted' = rev(c('Mounted', 'Not Mounted')),
                         'disksUsage' = NA,
                         'disksFull' = rev(c('Full', 'Not Full')),
                         'samplingStatus' = rev(c('Sampling', 'Not Sampling')),
                         'acousticRecording' = rev(c('Recording', 'Not Recording')))
      # I'm into bquote right now
      L <- '['
      R <- ']'
      zlab <- switch(input$porpVar,
                     'events' = bquote('Events'),
                     'status' = bquote('Status'),
                     'diskMounted' = bquote('Disk Mounted'),
                     'disksUsage' = bquote('Disks Usage'*.(L)*'%'*.(R)),
                     'disksFull' = bquote('Disks Full'),
                     'samplingStatus' = bquote('Sampling Status'),
                     'acousticRecording' = bquote('Acoustic Recording'))
      #par(xaxs='i',yaxs='i', mar=mardef)
      par(mar=mardef)

      yaxt <- ifelse(!is.na(varlabel[1]), 'n', 's')
      if (is.null(state$xlim)) {
        oce.plot.ts(PLD$timesci, porpData,
                    ylim = input$porpLimits,
                    xlim = range(c(NAV$time, PLD$timesci), na.rm = TRUE),
                    xlab = '', ylab = zlab, mar=marcm, type = 'n',
                    yaxt = yaxt)
          lines(PLD$timesci, porpData, lwd = 2)
        if(!is.na(varlabel[1])){
          axis(2, at = varlim, label = varlabel)
        }
        
      } else {
        okylim <- PLD$timesci > state$xlim[1] & PLD$timesci < state$xlim[2] #limits for science var
        okylimg <- NAV$time > state$xlim[1] & NAV$time < state$xlim[2] #limits for depth from navigation
        if(input$porpVar == 'disksUsage'){
            ylim <- range(porpData[porpData != 9999][okylim],na.rm = TRUE)
        } else {
            ylim <- varlim
        }
        oce.plot.ts(PLD$timesci, porpData,
                    ylim = input$porpLimits,
                    xlim = state$xlim,
                    xlab = '', ylab = zlab, mar=marcm,
                    yaxt = yaxt)
        lines(PLD$timesci, porpData, lwd = 2)
        if(!is.na(varlabel[1])){
          axis(2, at = varlim, label = varlabel)
        }
      }
      grid()
      par(mar=mardef)
      
      
    } # closes else if sciVar = porpoise
    }) # closes plot2

    # leaflet map plot

    # map groups
    map_allposition <- "Glider positions"
    map_track <- "Glider track"
    #map_lastlocation <- "Last received location"
    map_kml <- "KML track and positions"
    map_piloting <- "Piloting waypoints"
    map_desiredHeading <- "Desired heading"
    map_msn <- "msn file waypoints"
    #map_track_kml <- "Glider Track KML"
    ## okloc <- PLD$Lat > 0
    ## glon <- PLD$Lon[okloc]
    ## glat <- PLD$Lat[okloc]
    #okloc <- NAV$Lat > 0 # commented out 20181015
    #glon <- unique(NAV$Lon[okloc])
    #glat <- unique(NAV$Lat[okloc])
    # remove 0,0 location 20181015
    okloc <- glat > 0
    glon <- glon[okloc]
    glat <- glat[okloc]
    gdeshead <- gdeshead[okloc]
    gdesheadpolar <- compass2polar(theta = gdeshead)
    if(all(is.na(gdesheadpolar))){
      gdesheadpolar <- rep(0, length(gdeshead))
    }
    # get coordinates to show desired heading on map 
    df <- arrowShaftCoordinates(longitude = glon, 
                      latitude = glat, 
                      phi = gdesheadpolar,
                      L = 2)
      map <- leaflet(as.data.frame(cbind(glon, glat)))%>%
        addProviderTiles(providers$Esri.OceanBasemap) %>%
        fitBounds(lng1 = max(glon, na.rm = TRUE) - 0.2,
                  lat1 = min(glat, na.rm = TRUE) + 0.2,
                  lng2 = min(glon, na.rm = TRUE) + 0.2,
                  lat2 = max(glat, na.rm = TRUE) - 0.2) %>%
        # use NOAA graticules
        # not sure if it does much, but it allows to zoom further in
        # no bathy when zoomed less than 500m though.
        addWMSTiles(
          "https://maps.ngdc.noaa.gov/arcgis/services/graticule/MapServer/WMSServer/",
          layers = c("1-degree grid", "5-degree grid"),
          options = WMSTileOptions(format = "image/png8", transparent = TRUE),
          attribution = "NOAA") %>%
        # add extra map features
        addMouseCoordinates(style = 'basic')%>%
        addScaleBar(position = 'topright')%>%
        addMeasure(primaryLengthUnit = "kilometers",
                   secondaryLengthUnit = 'miles',
                   primaryAreaUnit = "hectares",
                   secondaryAreaUnit="acres",
                   position = 'bottomleft') %>%
                # map_piloting
          # shipping lanes
        addPolylines(lng = d1A$lon, lat = d1A$lat,
                    col = 'purple',
                    weight = 3,
                    group = map_piloting)%>%
        addPolylines(lng = d1C1B$lon, lat = d1C1B$lat,
                     col = 'purple',
                     weight = 3,
                     group = map_piloting)%>%
        addPolylines(lng = d1E$lon, lat = d1E$lat,
                     col = 'purple',
                     weight = 3,
                     group = map_piloting)%>%
        addPolylines(lng = d1F$lon, lat = d1F$lat,
                     col = 'purple',
                     weight = 3,
                     group = map_piloting)%>%
          # shipping lane zones
        addPolygons(lng = d1A1Bzone$lon, lat = d1A1Bzone$lat,
                    col = 'pink',
                    stroke = FALSE,
                    fillOpacity = 0.7,
                    group = map_piloting)%>%
        addPolygons(lng = d1B1Czone$lon, lat = d1B1Czone$lat,
                    col = 'pink',
                    stroke = FALSE,
                    fillOpacity = 0.7,
                    group = map_piloting)%>%
        addPolygons(lng = d2C2Dzone$lon, lat = d2C2Dzone$lat,
                    col = 'pink',
                    stroke = FALSE,
                    fillOpacity = 0.7,
                    group = map_piloting)%>%
        addPolygons(lng = d1E1Fzone$lon, lat = d1E1Fzone$lat,
                    col = 'pink',
                    fillOpacity = 0.7,
                    group = map_piloting)%>%
        addPolygons(lng = d3C3Dzone$lon, lat = d3C3Dzone$lat,
                    col = 'pink',
                    fillOpacity = 0.7,
                    group = map_piloting)%>%
          # msn waypoints with rval
        addCircles(lng = msn$lon, lat = msn$lat,
                         radius = msn$rval * 1000,
                         fillOpacity = 0.2,
                         color = 'red',
                         stroke = FALSE,
                         group = map_msn)%>%
          # nscm and viking buoy
        addMarkers(lng = nscmlon, lat = nscmlat,
                   icon = mooringIcon,
                   popup = paste(sep = "<br/>",
                                 "Nova Scotia Current Mooring"),
                   label = paste0("Nova Scotia Current Mooring"),
                   group = map_piloting) %>%
        addMarkers(lng = hfxviklon, lat = hfxviklat,
                   icon = mooringIcon,
                   popup = paste(sep = "<br/>",
                                 "HFX Viking Buoy"),
                   label = paste0("HFX Viking Buoy"),
                   group = map_piloting) %>%
          # deployment/recovery location
        addMarkers(lng = drlon, lat = drlat,
                         #radius = 7, fillOpacity = .4, stroke = F,
                         #color = 'purple',
                         icon = returnIcon,
                         popup = paste(sep = "<br/>",
                                       "Deployment/Recovery Location",
                                       paste0(as.character(round(drlat,4)), ',', as.character(round(drlon,4)))),
                         label = paste0("Deployment/Recovery Location"),
                         group = map_piloting)%>%
          # piloting waypoints
        addMarkers(lng = gllon, lat = gllat,
                         #radius = 7, fillOpacity = .4, stroke = F,
                         #color = 'purple',
                         icon = returnIcon,
                         popup = paste(sep = "<br/>",
                                       paste0('GL', c(1,2,3,4)),
                                       paste0(as.character(round(gllat,4)), ',', as.character(round(gllon,3)))),
                         label = paste0('GL', c(1,2,3,4)),
                         group = map_piloting) %>%
        
        # map_allposition
          # glider positions
        addCircleMarkers(lng = glon, lat = glat,
                         radius = 6, fillOpacity = .6, stroke = F,
                         popup = paste(sep = "<br/>",
                                       "Glider position",
                                       as.character(as.POSIXct(profileTimes, origin = '1970-01-01', tz = 'UTC')),
                                       paste0(as.character(round(glat,4)), ', ', as.character(round(glon,4)))),
                         label = paste0('Glider position: ', as.character(as.POSIXct(profileTimes, origin = '1970-01-01', tz = 'UTC'))),
                         group = map_allposition) %>%
          # desired heading of glider
          # syntax is kinda weird, but leaflet doesn't like for loops
        {
          for(i in unique(df$group)){
            . <- addPolylines(., lng = df$longitude[df$group == i],
                                 lat = df$latitude[df$group == i],
                              group = map_desiredHeading,
                              weight = 2)
              }
            return (.)
        } %>%
        # map_track
          #line track
        addPolylines(lng = glon, lat = glat,
                     weight = 2,
                     group = map_track) %>%
        #map_kml
          # positions from kml
        addCircleMarkers(lng = kmlLon, lat = kmlLat,
                         radius = 4, fillOpacity = .4, stroke = F,
                         color = 'red',
                         popup = paste(sep = "<br/>",
                                       "Glider position kml",
                                       kmlTime,
                                       paste0(as.character(round(kmlLat,4)), ', ', as.character(round(kmlLon,4)))),
                         label = paste0('Glider position kml: ', 1:length(kmlLat)),
                         group = map_kml)%>%
          # line track for kml
        addPolylines(lng = kmlLon, lat = kmlLat,
                     col = 'red',
                     weight = 2,
                     group = map_kml) %>%
          # latest position from kml
        addCircleMarkers(lng = kmlLon[length(kmlLat)], lat = kmlLat[length(kmlLat)],
                         radius = 6, fillOpacity = 1, stroke = F,
                         color = 'green',
                         popup = paste(sep = "<br/>",
                                       "Latest glider position from kml",
                                       kmlTime[length(kmlLat)],
                                       paste0(as.character(round(kmlLat[length(kmlLat)],4)), ', ', as.character(round(kmlLon[length(kmlLat)],4)))),
                         label = paste0('Latest glider position from kml: ', as.character(kmlTime[length(kmlLat)])),
                         group = map_kml)%>%
        # group-less map items
          # halifax line
        addCircleMarkers(lng = hfxlon, lat = hfxlat,
                         radius = 7, fillOpacity = 0.5, stroke = F,
                         color = 'gray48',
                         popup = paste(sep = "<br/>",
                                       #paste0("HL", as.character(1:7)),
                                       c("HL1","HL2","HL3","HL4","HL5","HL6","HL7","HL3.3", "HL5.5", "HL6.3", "HL6.7"),
                                       paste0(as.character(round(hfxlat,4)), ',', as.character(round(hfxlon,3)))),
                        # label = paste0("HL", 1:7))
                          label = c("HL1","HL2","HL3","HL4","HL5","HL6","HL7","HL3.3", "HL5.5", "HL6.3", "HL6.7"))%>%
          # bonavista line
        addCircleMarkers(lng = bblon, lat = bblat,
                         radius = 7, fillOpacity = 0.5, stroke = F,
                         color = 'gray48',
                         popup = paste(sep = "<br/>",
                                       paste0('BB', seq(1,15)),
                                       paste0(as.character(round(bblat, 4)), ',', as.character(round(bblon, 3)))),
                         label = paste0('BB', seq(1,15)))%>%
          # last received / current location
        addCircleMarkers(lng = glon[length(glon)], lat = glat[length(glon)],
                         radius = 6, fillOpacity = 1, stroke = F,
                         popup = paste(sep = "<br/>",
                                       "Lastest location received from nav file",
                                       as.character(profileTimes[length(glon)]),
                                       paste0(as.character(round(glat[length(glon)],4)), ', ', as.character(round(glon[length(glon)],4)))),
                         label = paste0("Last location received from nav file:", as.character(profileTimes[length(glon)])),
                         color = 'green') %>%
                         #group = map_lastlocation) %>%

        # layer control legend
        addLayersControl(overlayGroups = c(map_allposition,
                                           map_track,
                                           #map_lastlocation,
                                           map_kml,
                                           map_piloting,
                                           map_desiredHeading,
                                           map_msn),
                                           #map_track_kml),
                         options = layersControlOptions(collapsed = FALSE, autoZIndex = FALSE),
                         position = 'bottomright') %>%
        hideGroup(group = c(map_allposition,
                            map_track,
                            map_desiredHeading)) %>%
        setView(tail(glon, 1), tail(glat, 1), zoom=11)
    output$map <- renderLeaflet(map) #closes leafletplot

    output$plotpressure <- renderPlot({
      if (is.null(state$xlim)) {
        #par(mar = marcm)
        #par(xaxs='i',yaxs='i')#tight
        oce.plot.ts(NAV$time, NAV$depth,
                    type="n",
                    ylim=c(max(NAV$altHit,na.rm = TRUE), -5),
                    xlim= range(c(NAV$time, PLD$timesci), na.rm = TRUE),
                    ylab='Depth (m)',xlab='Time',
                    mar=marcm)
        points(NAV$time,NAV$altHit,pch=20,cex = 1, col = "red")
        points(NAV$time, NAV$depth, pch=20,cex = 1, col = "dark blue")
        text(profileTimes, -2, as.character(profileNumber), cex=1)
        grid()
      } else {
        par(mar = marcm)
        #par(xaxs='i',yaxs='i')#tight
        oce.plot.ts(NAV$time, NAV$depth,
                    type = "n",
                    ylim=c(max(NAV$altHit,na.rm = TRUE), -5),
                    xlim = state$xlim,
                    ylab = 'Depth (m)',xlab='Time',
                    mar=marcm)
        points(NAV$time,NAV$altHit,pch=20,cex = 1, col = "red")
        points(NAV$time, NAV$depth, pch=20,cex = 1, col = "dark blue")
        text(profileTimes, -2, as.character(profileNumber), cex=1)
        grid()
      }
    })
    
    output$TS  <- renderPlot({
        if (is.null(state$xlim) & is.null(state$Tlim)) {
            plotTS(as.ctd(PLD$Sal, PLD$Temp, PLD$Press), pch=19, col=1)
        } else if (is.null(state$xlim) & !is.null(state$Tlim)) {
            plotTS(as.ctd(PLD$Sal, PLD$Temp, PLD$Press), pch=19, col=1,
                   Tlim=state$Tlim, Slim=state$Slim)
        } else if (!is.null(state$xlim) & is.null(state$Tlim)) {
            II <- state$xlim[1] <= PLD$timesci & PLD$timesci <= state$xlim[2]
            plotTS(as.ctd(PLD$Sal, PLD$Temp, PLD$Press), pch=19, col='lightgrey')
            plotTS(as.ctd(PLD$Sal[II], PLD$Temp[II], PLD$Press[II]), pch=19, col=1,
                   add=TRUE)
        } else {
            II <- state$xlim[1] <= PLD$timesci & PLD$timesci <= state$xlim[2]
            plotTS(as.ctd(PLD$Sal, PLD$Temp, PLD$Press), pch=19, col='lightgrey',
                   Tlim=state$Tlim, Slim=state$Slim)
            plotTS(as.ctd(PLD$Sal[II], PLD$Temp[II], PLD$Press[II]), pch=19, col=1,
                   add=TRUE)
        }
    })
    
    observeEvent(input$dncstp1,{
      state$dnp1 <- input$dncstp1
    })

    observeEvent(input$upcstp1,{
      state$upp1 <- input$upcstp1
    })

    # setting limits for brushed plots
    # top section plot, set limits
    observeEvent(input$plot_brush, {
      state$xlim <- c(input$plot_brush$xmin, input$plot_brush$xmax)
    })
    # TS
    observeEvent(input$plot_brush_TS, {
        state$Tlim <- c(input$plot_brush_TS$ymin, input$plot_brush_TS$ymax)
        state$Slim <- c(input$plot_brush_TS$xmin, input$plot_brush_TS$xmax)
    })
    # reset plots
    # navigation section
    observeEvent(input$resetNav, {
      state$xlim <- range(c(NAV$time, PLD$timesci), na.rm = TRUE)
    })
    observeEvent(input$plot_click, {
      state$xlim <- range(c(NAV$time, PLD$timesci), na.rm = TRUE)
      })
    # science section
    observeEvent(input$resetSci, {
      state$xlim <- range(c(NAV$time, PLD$timesci), na.rm = TRUE)
    })
    # porpoise plots
    observeEvent(input$resetPorp, {
        state$xlim <- range(c(NAV$time, PLD$timesci), na.rm = TRUE)
    })
    observeEvent(input$resetTS, {
        state$xlim <- range(c(NAV$time, PLD$timesci), na.rm = TRUE)
        state$Tlim <- range(PLD$Temp, na.rm = TRUE)
        state$Slim <- range(PLD$Sal, na.rm = TRUE)
    })
    observeEvent(input$plot_click_TS, {
        state$Tlim <- range(PLD$Temp, na.rm = TRUE)
        state$Slim <- range(PLD$Sal, na.rm = TRUE)
    })

  }) #closes download observeEvent


}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
