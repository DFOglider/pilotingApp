rm(list=ls())
library(shiny)
library(oce)
library(ocedata)
library(measurements)
library(leaflet)
library(RCurl)
library(geosphere)
library(XML)
data(ctd) # for initial plotProfile tests, delete later
options(oceEOS='unesco') # prevent error for calculated values using swSigmaTheta, etc 

source('addMouseCoordinates.R') # from mapview package, some had issues with mapview
source('readSeaExplorerRealTime.R') # read in real time seaExplorer data
source('readSeaExplorerKml.R') # gets lon lat from kml file
source('oxygenCalibrationCoefficients.R') # used to convert oxygen from units of Hz to ml/l
source('swSatO2.R') # for use in sbeO2Hz2Sat.R
source('sbeO2Hz2Sat.R') # calculate oxygen from Hz to ml/l from seaBird instrument
source('downloadData.R') # obtain glidernames and missions from ftp and downloads
source('findProfilesSOCIB.R') # finds downcast and upcasts from a yo
data('coastlineWorldFine')

mardef <- c(3.1, 3.1, 1.1, 2.1) # default margins
marcm <- c(3.1, 3.1, 1.1, 6.1) # color bar with zlab margins

#deployment/recovery location
drlon <- -63.406418 
drlat <- 44.520789

# halifax line stations
hfxlon <- c(-63.450000, -63.317000, -62.883000, -62.451000, -62.098000, -61.733000, -61.393945, -62.7527)
hfxlat <- c(44.400001, 44.267001, 43.883001, 43.479000, 43.183000, 42.850000, 42.531138, 43.7635)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Glider Data"),
  
  fluidRow(
    column(2, wellPanel(
         selectInput(inputId = 'Glider', 
                     label = 'Choose a glider', 
                     choices = gliderdirnames), #gliderdirnames from downloadData.R
         uiOutput(outputId = 'Missions'),
         actionButton(inputId = 'download', 
                      label = 'Download and load data'),
         
         # conditional panel for plots tab
         conditionalPanel(
           condition = "input.tabs == 'Plots'",
            selectInput(inputId="Var", 
                     label="Data Set:", 
                     choices=c('Navigation'='Navigation','Science'='Science'), 
                     selected = 'Navigation')
         ),
        #conditional panels for navigation in plots tab
        conditionalPanel(
          condition = "input.Var == 'Navigation' & input.tabs == 'Plots'",
          actionButton("resetNav", "Reset plot")),
        
        

         conditionalPanel(
          condition="input.Var=='Navigation' & input.tabs == 'Plots'", 
          radioButtons(inputId = "NavVar",
                  label = "Variables:",
                  choices = c('Altimeter' = 'altimeter',
                              'Alarm'='alarm',
                              'Pitch'='Pitch',
                              'Vertical Speed'='VertSpeed',
                              'Battery Voltage'='BatterieVolt',
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
                  selected = 'Pitch')),
        
        #conditional panels for science in plots tab
        conditionalPanel(
          condition = "input.Var == 'Science' & input.tabs == 'Plots'",
          actionButton("resetSci", "Reset plot")),
        
        conditionalPanel(
          condition="input.Var=='Science' & input.tabs == 'Plots'", 
            radioButtons(inputId = "SciVar",
                      label = "Variables:",
                      choices = c('Temperature'='Temp',
                                  'Conductivity'='Cond',
                                  'Salinity'='Sal',
                                  'Density'='Dens',
                                  'Oxygen Frequency'='DOF',
                                  'Oxygen Concentration' = 'OxyConc',
                                  'Oxygen Saturation' = 'OxySat',
                                  'Chlorophyl'='CHL_scaled',
                                  'CDOM'='CDOM_scaled',
                                  'BB_700nm'='BB_scaled'),
                      selected = 'Temp'),
            uiOutput('sciScaleBar')),
        #conditionalPanels for profileplots
        conditionalPanel(
          condition = "input.tabs == 'Profiles'",
          # left profile plot data selection
          selectInput(inputId = 'profile1var',
                      label = 'Variable for left Profile:',
                      choices = c('Temperature'='temperature',
                                  'Conductivity'='conductivity',
                                  'Salinity'='salinity',
                                  'Density'='sigmaTheta',
                                  'Oxygen Concentration' = 'oxygenConcentration',
                                  'Oxygen Saturation' = 'oxygenSaturation',
                                  'Chlorophyll'='chlorophyll',
                                  'CDOM'='cdom',
                                  'Backscatter'='backscatter'),
                      selected = 'temperature'),
          # reset left profile plot button
          actionButton('resetp1', 'Reset Profiles'),
          # right profile plot data selection
          selectInput(inputId = 'profile2var',
                      label = 'Variable for right Profile :',
                      choices = c('Temperature'='temperature',
                                  'Conductivity'='conductivity',
                                  'Salinity'='salinity',
                                  'Density'='sigmaTheta',
                                  'Oxygen Concentration' = 'oxygenConcentration',
                                  'Oxygen Saturation' = 'oxygenSaturation',
                                  'Chlorophyll'='chlorophyll',
                                  'CDOM'='cdom',
                                  'Backscatter'='backscatter'),
                      selected = 'salinity'),
          # reset right profile plot button
          # NOTE : not currently needed
          #actionButton('resetp2', 'Reset right Profile'),          
          checkboxInput(inputId = 'dncstp1',
                        label = 'Downcasts',
                        value = TRUE),
          checkboxInput(inputId = 'upcstp1',
                        label = 'Upcasts',
                        value = FALSE),
          uiOutput(outputId = 'numProfiles'),
          strong('Plot Profiles'),
          uiOutput(outputId = 'rng1p1'),
          uiOutput(outputId = 'rng2p1')
          ) #closes conditional panel for profile variable choices.
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
      # Q : Fixed width for profiles or fluid ?
      tabPanel("Profiles",
               fluidRow(
                 column(6,
                  plotOutput("profile1",dblclick="plot_click", 
                             brush = brushOpts(id = 'profile1brush',
                                               direction = 'xy',
                                               resetOnNew = TRUE),
                             height = '620px' 
                             #width = '450px'
                             )),
                 column(6,
                  plotOutput("profile2", 
                             height = '620px'
                             #width = '450px'
                             ))))
      ) #closes tabset
    ) #closes column
    ) #closes fluidRow
) #closes ui


               
           

# Define server 
server <- function(input, output) {
  state <- reactiveValues()
  
  # select input for mission based on selected glider
  output$Missions <- renderUI({
    missions <- getMissions(glider = input$Glider)
    selectInput(inputId = 'Mission', label = 'Choose a mission', choices = missions,
                selected=tail(missions, 1))
  })
  
  # download data and load when actionButton clicked
  # make plots too
  observeEvent(input$download,{
    # download and process data
    downloadData(datadir = datadir, glider = input$Glider, mission = input$Mission)
    data <- readSeaExplorerRealTime(datadir = datadir, glider = input$Glider, mission = input$Mission)
    PLD <- data$PLD
    glider <- data$NAV
    profileNumber <- unique(glider$profileNumber)
    profileTimes <- NULL
    for (pi in seq_along(profileNumber)) {
        profileTimes <- c(profileTimes, glider$time[which(profileNumber[pi] == glider$profileNumber)][1])
    }
    profileTimes <- numberAsPOSIXct(profileTimes)
    dnctd <- data$dnctd
    upctd <- data$upctd
    kmlcoord <- readSeaExplorerKml(datadir = datadir, glider = input$Glider, mission = input$Mission)
    okkml <- !is.na(kmlcoord$lon)
    kmlLon <- kmlcoord$lon[okkml]
    kmlLat <- kmlcoord$lat[okkml]
    # profile numbers    
    profiles <- unlist(lapply(dnctd, function(k) k@metadata[['station']]))    
    output$numProfiles <- renderUI({
      h5(paste0(length(profiles),' profiles detected'))
    })
    output$rng1p1 <- renderUI({
      selectInput(inputId = 'profileRng1p1',
                  label = '',
                  choices = profiles,
                  selected = profiles[1],
                  width = '40%')
    })
    output$rng2p1 <- renderUI({
      selectInput(inputId = 'profileRng2p1',
                  label = 'to',
                  choices = profiles[profiles >= as.numeric(input$profileRng1p1)],
                  width = '40%',
                  selected = profiles[length(profiles >= as.numeric(input$profileRng1p1))])
    })
    # scaleBar for science plots
    output$sciScaleBar <- renderUI({
      rng <- switch(input$SciVar,
                    'Temp' = c(-2, 22),
                    'Sal' = c(29, 35.5),
                    'Cond' = c(0,7),
                    'Dens' = c(20, 28),
                    'CHL_scaled' = c(-.02,5),
                    'CDOM_scaled' = c(-2,12),
                    'BB_scaled' = c(-0.005, 0.005),
                    'DOF' = c(2000, 5000),
                    'OxyConc' = c(0,10),
                    'OxySat' = c(0,120))
      value <- switch(input$SciVar,
                      'Temp' = unname(quantile(PLD$Temp, probs = c(0.01, 0.99), na.rm = TRUE)),
                      'Sal' = unname(quantile(PLD$Sal, probs = c(0.02, 0.99),  na.rm = TRUE)),
                      'Cond' = unname(quantile(PLD$Conduc, probs = c(0.02, 0.99), na.rm = TRUE)),
                      'Dens' = unname(quantile(PLD$SigTheta, probs = c(0.02, 0.99), na.rm = TRUE)),
                      'CHL_scaled' = unname(quantile(PLD$CHL_scaled, probs = c(0.01, 0.99), na.rm = TRUE)),
                      'CDOM_scaled' = unname(quantile(PLD$CDOM_scaled, probs = c(0.01, 0.99), na.rm = TRUE)),
                      'BB_scaled' = unname(quantile(PLD$BB_scaled, probs = c(0.01, 0.99), na.rm = TRUE)),
                      'DOF' = unname(quantile(PLD$DOF, probs = c(0.01, 0.97), na.rm = TRUE)),
                      'OxyConc' = unname(quantile(PLD$OxyConc, probs = c(0.01, 0.97), na.rm = TRUE)),
                      'OxySat' = unname(quantile(PLD$OxySat, probs = c(0.01, 0.97), na.rm = TRUE)))
      step <- switch(input$SciVar,
                     'Temp' = 0.5,
                     'Sal' = 0.1,
                     'Cond' = 0.01,
                     'Dens' = 0.1,
                     'CHL_scaled' = 0.1,
                     'CDOM_scaled' = 0.1,
                     'BB_scaled' = 0.0005,
                     'DOF' = 100,
                     'OxyConc' = 0.5,
                     'OxySat' = 1)
      # deal with values that vary little during simulation
      if(diff(value) < 5*step){value[2] <- value[2] + 5*step} 
      sliderInput("sciLimits", "Choose colorbar limits:", min = rng[1], max = rng[2],
                  value = value, step = step, animate = FALSE)  
      
    })
    # plot1 - top plot
    output$plot1 <- renderPlot({
      if (is.null(state$xlim)) {
        #par(mar = marcm)
        #par(xaxs='i',yaxs='i')#tight
        oce.plot.ts(glider$time, glider$depth, 
                    type="n", 
                    ylim=c(max(glider$altHit,na.rm = TRUE), -5), 
                    xlim=(range(glider$time, na.rm = TRUE)), 
                    ylab='Depth (m)',xlab='Time', 
                    mar=marcm)
        points(glider$time,glider$altHit,pch=20,cex = 1, col = "red")
        points(glider$time, glider$depth, pch=20,cex = 1, col = "dark blue")
        text(profileTimes, -2, as.character(profileNumber), cex=1) 
        grid()
      } else {
        par(mar = marcm)
        #par(xaxs='i',yaxs='i')#tight
        oce.plot.ts(glider$time, glider$depth, 
                    type = "n",
                    ylim=c(max(glider$altHit,na.rm = TRUE), -5), 
                    xlim = state$xlim, 
                    ylab = 'Depth (m)',xlab='Time', 
                    mar=marcm)
        points(glider$time,glider$altHit,pch=20,cex = 1, col = "red")
        points(glider$time, glider$depth, pch=20,cex = 1, col = "dark blue")
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
      if(input$NavVar == 'altimeter'){
        if (is.null(state$xlim)) {
          #par(mar = marcm)
          #par(xaxs='i',yaxs='i')#tight
          oce.plot.ts(glider$time, glider$depth, 
               type = "n", 
               ylim = range(glider$alt,na.rm = TRUE), 
               xlim = range(glider$time, na.rm = TRUE), 
               ylab = 'range (m)', 
               xlab = 'Time',
               mar=marcm)
          points(glider$time,glider$alt,pch=20,cex = 1, col = "red")
          ## points(glider$time, glider$depth, pch=20,cex = 1, col = "dark blue")
          grid()
        } else {
          okylim <- glider$time > state$xlim[1] & glider$time < state$xlim[2]
          #par(mar = marcm)
          #par(xaxs='i', yaxs='i')#tight
          oce.plot.ts(glider$time, glider$depth, 
               type = "n", 
               ylim = range(glider$alt[okylim],na.rm = TRUE) + c(-0.5, 0.5 ), 
               xlim = state$xlim, 
               ylab = 'range (m)', 
               xlab = 'Time',
               mar=marcm)
          points(glider$time, glider$alt, pch=20,cex = 1, col = "red")
          ## points(glider$time, glider$depth, pch=20,cex = 1, col = "dark blue")
          grid()
        }
      } else if (input$NavVar=='Pitch') {
        if (is.null(state$xlim)) {
           #par(mar=marcm)
           #par(xaxs='i',yaxs='i')#tight
           oce.plot.ts(glider$time, glider[[input$NavVar]],
           ylim=c(-80,40),
           xlim=(range(glider$time, na.rm = TRUE)),
           xlab='Time',ylab='',type='n', 
           mar=marcm)
           polygon(c(glider$time,rev(glider$time)),c(rep(15,length(glider$time)),rep(25,length(glider$time))),col=gray(0.8),border=NA)  
           polygon(c(glider$time,rev(glider$time)),c(rep(-15,length(glider$time)),rep(-25,length(glider$time))),col=gray(0.8),border=NA) 
           lines(glider$time, glider[[input$NavVar]],lwd = 2, col = "black")
           grid()
        } else {
          #par(mar = marcm)
          #par(xaxs='i',yaxs='i')#tight
          oce.plot.ts(glider$time, glider[[input$NavVar]],
                      ylim=c(-80,40),
                      xlim=state$xlim,
                      xlab='Time',ylab='',type='n', 
                      mar=marcm)
          polygon(c(glider$time,rev(glider$time)),c(rep(15,length(glider$time)),rep(25,length(glider$time))),col=gray(0.8),border=NA)  
          polygon(c(glider$time,rev(glider$time)),c(rep(-15,length(glider$time)),rep(-25,length(glider$time))),col=gray(0.8),border=NA) 
          lines(glider$time, glider[[input$NavVar]],lwd = 2, col = "black")
          grid()
        }
      } else if (input$NavVar=='VertSpeed') {
        if (is.null(state$xlim)) {
        #par(xaxs='i',yaxs='i')#tight
        #par(mar = marcm)
        oce.plot.ts(glider$time, glider[[input$NavVar]],
             ylim=c(-30,30),
             xlim=(range(glider$time, na.rm = TRUE)),
             xlab='Time',ylab='',type='n',
             mar=marcm)
        polygon(c(glider$time,rev(glider$time)),c(rep(-13,length(glider$time)),rep(-17,length(glider$time))),col=gray(0.8),border=NA)  
        polygon(c(glider$time,rev(glider$time)),c(rep(13,length(glider$time)),rep(17,length(glider$time))),col=gray(0.8),border=NA) 
        lines(glider$time, glider[[input$NavVar]],lwd = 2, col = "black")
        grid()
        } else {
          #par(xaxs='i',yaxs='i')#tight
          #par(mar = marcm)
          oce.plot.ts(glider$time, glider[[input$NavVar]],
               ylim=c(-30,30),
               xlim=state$xlim,
               xlab='Time',ylab='',type='n',
               mar=marcm)
          polygon(c(glider$time,rev(glider$time)),c(rep(-13,length(glider$time)),rep(-17,length(glider$time))),col=gray(0.8),border=NA)  
          polygon(c(glider$time,rev(glider$time)),c(rep(13,length(glider$time)),rep(17,length(glider$time))),col=gray(0.8),border=NA) 
          lines(glider$time, glider[[input$NavVar]],lwd = 2, col = "black")
          grid()
        }
        
      } else if (input$NavVar=='BatterieVolt') {
        if (is.null(state$xlim)) {
         #par(mar = marcm)
         #par(xaxs='i',yaxs='i')#tight
        oce.plot.ts(glider$time, glider[[input$NavVar]],
             ylim=c(24,30),
             xlim=(range(glider$time, na.rm = TRUE)),
             xlab='Time',ylab='',type='n',
             mar=marcm)
        polygon(c(glider$time,rev(glider$time)),c(rep(24,length(glider$time)),rep(26,length(glider$time))),col=gray(0.8),border=NA)  
        lines(glider$time, glider[[input$NavVar]],lwd = 2, col = "red")
        grid()
        } else {
          #par(xaxs='i',yaxs='i')#tight
          #par(mar = marcm)
          oce.plot.ts(glider$time, glider[[input$NavVar]],
               ylim=c(24,30),
               xlim=state$xlim,
               xlab='Time',ylab='',type='n',
               mar=marcm)
          polygon(c(glider$time,rev(glider$time)),c(rep(24,length(glider$time)),rep(26,length(glider$time))),col=gray(0.8),border=NA)  
          lines(glider$time, glider[[input$NavVar]],lwd = 2, col = "red")
          grid()
        }
        
      } else if (input$NavVar=='speedms') {
        if (is.null(state$xlim)) {
          #par(mar = marcm)
          #par(xaxs='i',yaxs='i')#tight
          oce.plot.ts(glider$time, glider[[input$NavVar]],
               ylim=c(0,max(glider[[input$NavVar]],na.rm = TRUE)),
               xlim=(range(glider$time, na.rm = TRUE)),
               xlab='Time',ylab='(m/s)',
               mar=marcm)
          points(glider$time, glider[[input$NavVar]], pch=19,cex = 1, col = "dark green")
          grid()
        } else {
          #par(mar = marcm)
          #par(xaxs='i',yaxs='i')#tight
          oce.plot.ts(glider$time, glider[[input$NavVar]],
               ylim=c(0,max(glider[[input$NavVar]],na.rm = TRUE)),
               xlim=state$xlim,
               xlab='Time',ylab='(m/s)',
               mar=marcm)
          points(glider$time, glider[[input$NavVar]], pch=19,cex = 1, col = "dark green")
          grid()
        }
        
      } else if (input$NavVar=='distkm') {
        if (is.null(state$xlim)) {
          #par(mar = marcm)
          #par(xaxs='i',yaxs='i')#tight
          oce.plot.ts(glider$time, glider[[input$NavVar]],
               ylim=c(0,max(glider[[input$NavVar]],na.rm = TRUE)),
               xlim=(range(glider$time, na.rm = TRUE)),
               xlab='Time',ylab='(km)',
               mar=marcm)
          points(glider$time, glider[[input$NavVar]], pch=19,cex = 1, col = "dark green")
          grid()
        } else {
          #par(mar = marcm)
          #par(xaxs='i',yaxs='i')#tight
          oce.plot.ts(glider$time, glider[[input$NavVar]],
               ylim=c(0,max(glider[[input$NavVar]],na.rm = TRUE)),
               xlim=state$xlim,
               xlab='Time',ylab='(km)',
               mar=marcm)
          points(glider$time, glider[[input$NavVar]], pch=19,cex = 1, col = "dark green")
          grid()
        }
        
      } else if (input$NavVar=='Temperature') {
        if (is.null(state$xlim)) {
          #par(mar = marcm)
          #par(xaxs='i',yaxs='i')#tight
          oce.plot.ts(glider$time, glider[[input$NavVar]],
               ylim=c(0,max(glider[[input$NavVar]])+3),
               xlim=(range(glider$time, na.rm = TRUE)),
               xlab='Time',ylab='',type='n',
               mar=marcm)
          lines(glider$time, glider[[input$NavVar]],lwd = 2, col = "red")
          grid()
        } else {
          #par(mar = marcm)
          #par(xaxs='i',yaxs='i')#tight
          oce.plot.ts(glider$time, glider[[input$NavVar]],
               ylim=c(0,max(glider[[input$NavVar]])+3),
               xlim=state$xlim,
               xlab='Time',ylab='',type='n',
               mar=marcm)
           lines(glider$time, glider[[input$NavVar]],lwd = 2, col = "red")
          grid()
        }
        
      } else if (input$NavVar=='Heading') {
        if (is.null(state$xlim)) {
        #par(xaxs='i',yaxs='i')#tight
        #par(mar = marcm)
        oce.plot.ts(glider$time, glider[[input$NavVar]],
             xlim=(range(glider$time, na.rm = TRUE)),
             xlab='Time',ylab='',type='n',
             mar=marcm)
        lines(glider$time, glider[[input$NavVar]],lwd = 2, col = "red")
        lines(glider$time, glider$DesiredHeading,lwd = 2, col = "blue")
        grid()
        } else {
          #par(mar = marcm)
          #par(xaxs='i',yaxs='i')#tight
          oce.plot.ts(glider$time, glider[[input$NavVar]],
               xlim=state$xlim,
               xlab='Time',ylab='',type='n',
               mar=marcm)
          lines(glider$time, glider[[input$NavVar]],lwd = 2, col = "red")
          lines(glider$time, glider$DesiredHeading,lwd = 2, col = "blue")
          grid()
        }
        
      } else if (input$NavVar=='BallastPos') {
        if (is.null(state$xlim)) {
        #par(xaxs='i',yaxs='i')#tight
        #par(mar = marcm)
        oce.plot.ts(glider$time, glider[[input$NavVar]],
             xlim=(range(glider$time, na.rm = TRUE)),
             xlab='Time',ylab='',type='n',
             mar=marcm)
        lines(glider$time, glider[[input$NavVar]],lwd = 3, col = "red")
        lines(glider$time, glider$BallastCmd,lwd = 2, col = "blue")
        grid()
        } else {
          #par(xaxs='i',yaxs='i')#tight
          #par(mar = marcm)
          oce.plot.ts(glider$time, glider[[input$NavVar]],
               xlim=state$xlim,
               xlab='Time',ylab='',type='n',
               mar=marcm)
          lines(glider$time, glider[[input$NavVar]],lwd = 3, col = "red")
          lines(glider$time, glider$BallastCmd,lwd = 2, col = "blue")
          grid()
        }
        
      } else if (input$NavVar=='LinPos') {
        if (is.null(state$xlim)) {
        #par(xaxs='i',yaxs='i')#tight
        #par(mar = marcm)
        oce.plot.ts(glider$time, glider[[input$NavVar]],
             xlim=(range(glider$time, na.rm = TRUE)),
             xlab='Time',ylab='',type='n',
             mar=marcm)
        lines(glider$time, glider[[input$NavVar]],lwd = 3, col = "red")
        lines(glider$time, glider$LinCmd,lwd = 2, col = "blue")
        grid()
        } else {
          #par(xaxs='i',yaxs='i')#tight
          #par(mar = marcm)
          oce.plot.ts(glider$time, glider[[input$NavVar]],
               xlim=state$xlim,
               xlab='Time',ylab='',type='n',
               mar=marcm)
          lines(glider$time, glider[[input$NavVar]],lwd = 3, col = "red")
          lines(glider$time, glider$LinCmd,lwd = 2, col = "blue")
          grid()
        } 
        
      } else if (input$NavVar=='AngPos') {
        if (is.null(state$xlim)) {
        #par(mar = marcm)
        #par(xaxs='i',yaxs='i')#tight
        oce.plot.ts(glider$time, glider[[input$NavVar]],
             xlim=(range(glider$time, na.rm = TRUE)),
             xlab='Time',ylab='',type='n',
             mar=marcm)
        lines(glider$time, glider[[input$NavVar]],lwd = 3, col = "red")
        lines(glider$time, glider$AngCmd,lwd = 2, col = "blue")
        grid()
        } else {
          #par(xaxs='i',yaxs='i')#tight
          #par(mar = marcm)
          oce.plot.ts(glider$time, glider[[input$NavVar]],
               xlim=state$xlim,
               xlab='Time',ylab='',type='n',
               mar=marcm)
          lines(glider$time, glider[[input$NavVar]],lwd = 3, col = "red")
          lines(glider$time, glider$AngCmd,lwd = 2, col = "blue")
          grid()
        } 
        
      } else { #profileNumber
        if (is.null(state$xlim)) {  
      #par(xaxs='i',yaxs='i')#tight
      #par(mar = marcm)
      oce.plot.ts(glider$time, glider[[input$NavVar]],
           xlim=(range(glider$time, na.rm = TRUE)),
           xlab='Time',ylab='',type="n",
           mar=marcm)
      lines(glider$time, glider[[input$NavVar]],col='blue',lwd=2)
      grid()
        } else {
          okylim <- glider$time > state$xlim[1] & glider$time < state$xlim[2]
          #par(xaxs='i',yaxs='i')#tight
          #par(mar = marcm)
          oce.plot.ts(glider$time, glider[[input$NavVar]],
               xlim=state$xlim,
               ylim = range(glider[[input$NavVar]][okylim], na.rm = TRUE),
               xlab='Time',ylab='',type="n",
               mar=marcm)
          lines(glider$time, glider[[input$NavVar]],col='blue',lwd=2)
          grid()
        }   
      }
    } else if (input$Var == 'Science') {
        # CL's work for science plots
        # get science data, make color map
        data <- switch(input$SciVar,
                       'Temp' = PLD$Temp,
                       'Sal' = PLD$Sal,
                       'Cond' = PLD$Conduc,
                       'Dens' = PLD$SigTheta,
                       'CHL_scaled' = PLD$CHL_scaled,
                       'CDOM_scaled' = PLD$CDOM_scaled,
                       'BB_scaled' = PLD$BB_scaled,
                       'DOF' = PLD$DOF,
                       'OxyConc' = PLD$OxyConc,
                       'OxySat' = PLD$OxySat)
        #use CL's file for resizable label for biological variables ?
        zlab <- switch(input$SciVar,
                       'Temp' = resizableLabel('T', axis = 'y'),
                       'Sal' = resizableLabel('S', axis = 'y'),
                       'Cond' = resizableLabel('conductivity S/m', axis = 'y'),
                       'Dens' = resizableLabel('sigmaTheta', axis = 'y'),
                       'CHL_scaled' = 'Chlorophyll',
                       'CDOM_scaled' = 'CDOM',
                       'BB_scaled' = 'Backscatter',
                       'DOF' = 'Dissolved Oxygen [Hz]',
                       'OxyConc' = resizableLabel('oxygen mL/L', axis = 'y'),
                       'OxySat' = 'Oxygen Saturation [%]')
        cm <- colormap(data, zlim = input$sciLimits)
        ylabp <- resizableLabel('p', axis = 'y')
        #par(xaxs='i',yaxs='i', mar=mardef)
        par(mar=mardef)
        drawPalette(colormap = cm, zlab = zlab)
        # match top panel, so use range of altHits for ylim
        #                  and nav time for xlim
        if (is.null(state$xlim)) {
          oce.plot.ts(PLD$timesci, PLD$Press, type='p',
              ylim = rev(range(glider$altHit,na.rm = TRUE)),
              xlim = (range(glider$time, na.rm = TRUE)),
              pch = 20, col = cm$zcol,
              xlab = '', ylab = '', mar=marcm)

        } else {
          okylim <- PLD$timesci > state$xlim[1] & PLD$timesci < state$xlim[2] #limits for science var
          okylimg <- glider$time > state$xlim[1] & glider$time < state$xlim[2] #limits for depth from navigation
          oce.plot.ts(PLD$timesci[okylim], PLD$Press[okylim], type='p',
               ylim = rev(range(glider$depth[okylimg],na.rm = TRUE)),
               xlim = state$xlim,
               pch = 20, col = cm$zcol[okylim],
               xlab = '', ylab = '', mar=marcm)
        }
        grid()
        mtext(ylabp, side = 2, line = 2)
        par(mar=mardef)
      
      
      } # closes else if sciVar = science  
    }) # closes plot2
    
    # leaflet map plot
    
    # map groups
    map_allposition <- "All Positions"
    map_track <- "Glider Track"
    map_lastlocation <- "Last received location"
    map_kml <- "Positions from KML"
    okloc <- PLD$Lat > 0
    glon <- PLD$Lon[okloc]
    glat <- PLD$Lat[okloc]
    

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
        #line track
        addPolylines(lng = glon, lat = glat, 
                     weight = 2,
                     group = map_track) %>%
        # deployment/recovery location
        addCircleMarkers(lng = drlon, lat = drlat,
                         radius = 5, fillOpacity = .4, stroke = F,
                         color = 'black',
                         popup = paste(sep = "<br/>",
                                       "Deployment/Recovery Location",
                                       paste0(as.character(round(drlat,4)), ',', as.character(round(drlon,4)))),
                         label = paste0("Deployment/Recovery Location"))%>%
        # halifax line
        addCircleMarkers(lng = hfxlon, lat = hfxlat,
                         radius = 5, fillOpacity = .4, stroke = F, 
                         color = 'black',
                         popup = paste(sep = "<br/>",
                                       #paste0("HL", as.character(1:7)),
                                       c("HL1","HL2","HL3","HL4","HL5","HL6","HL7","HL3.3"),
                                       paste0(as.character(round(hfxlat,4)), ',', as.character(round(hfxlon,3)))),
                        # label = paste0("HL", 1:7))
                          label = c("HL1","HL2","HL3","HL4","HL5","HL6","HL7","HL3.3"))%>%
        # glider positions
        addCircleMarkers(lng = glon, lat = glat, 
                         radius = 4, fillOpacity = .2, stroke = F,
                         popup = paste(sep = "<br/>",
                                       "Glider position",
                                       as.character(PLD$timesci[okloc]),
                                       paste0(as.character(round(glat,4)), ', ', as.character(round(glon,4)))),
                         label = paste0('Glider position: ', as.character(PLD$timesci[okloc])),
                         group = map_allposition)%>%
        # positions from kml
        addCircleMarkers(lng = kmlLon, lat = kmlLat, 
                         radius = 5, fillOpacity = .4, stroke = F,
                         color = 'red',
                         popup = paste(sep = "<br/>",
                                       "Glider position kml",
                                       #as.character(PLD$timesci[okloc]),
                                       paste0(as.character(round(kmlLat,4)), ', ', as.character(round(kmlLon,4)))),
                         label = paste0('Glider position kml: ', 1:length(kmlLat)),
                         group = map_kml)%>%
        # last received / current location
        addCircleMarkers(lng = glon[length(glon)], lat = glat[length(glon)],
                         radius = 6, fillOpacity = 1, stroke = F,
                         popup = paste(sep = "<br/>",
                                       "Lastest location received from nav file",
                                       as.character(PLD$timesci[okloc][length(glon)]),
                                       paste0(as.character(round(glat[length(glon)],4)), ', ', as.character(round(glon[length(glon)],4)))),
                         label = paste0("Last location received from nav file:", as.character(PLD$timesci[okloc][length(glon)])),
                         color = 'green',
                         group = map_lastlocation) %>%
        # latest position from kml
        addCircleMarkers(lng = kmlLon[length(kmlLat)], lat = kmlLat[length(kmlLat)], 
                         radius = 6, fillOpacity = 1, stroke = F,
                         color = 'green',
                         popup = paste(sep = "<br/>",
                                       "Latest glider position from kml",
                                       #as.character(PLD$timesci[okloc]),
                                       paste0(as.character(round(kmlLat[length(kmlLat)],4)), ', ', as.character(round(kmlLon[length(kmlLat)],4)))),
                         label = paste0('Latest glider position from kml: ', length(kmlLat)),
                         group = map_kml)%>%
        # layer control legend
        addLayersControl(overlayGroups = c(map_allposition,
                                           map_track,
                                           map_lastlocation,
                                           map_kml),
                         options = layersControlOptions(collapsed = FALSE), 
                         position = 'bottomright') %>%
        setView(tail(glon, 1), tail(glat, 1), zoom=11)
    output$map <- renderLeaflet(map) #closes leafletplot
    
    output$profile1 <- renderPlot({
      # can't use oce plotProfile due to its restrictions on
      # providing limits for variables, i.e, cannot supply
      # xlim and Tlim when the xtype is temperature
      # not a problem though when xtype is not one of the 
      # default variables make plot look like plotProfile
      
      # use same margins, etc as plotProfile
      mgp <- getOption('oceMgp')
      mar <- c(1, mgp[1]+1.5, mgp[1]+1.5, mgp[1])
      axisNameLoc <- mgp[1]
      par(mgp = mgp, mar = mar)
      ylab <- resizableLabel(item = 'p', axis = 'y')
      xlab <- switch(input$profile1var,
                     'temperature' = resizableLabel('T', axis = 'x'),
                     'salinity' = resizableLabel('S', axis = 'x'),
                     'conductivity' = resizableLabel('conductivity S/m', axis = 'x'),
                     'sigmaTheta' = resizableLabel('sigmaTheta', axis = 'x'),
                     'chlorophyll' = 'Chlorophyll',
                     'cdom' = 'CDOM',
                     'backscatter' = 'Backscatter',
                     'oxygenConcentration' = resizableLabel('oxygen mL/L', axis = 'y'),
                     'oxygenSaturation' = 'Oxygen Saturation [%]')
      ylim <- rev(range(unlist(lapply(dnctd, function(k) k[['pressure']]))))
      ylim <- if(is.null(state$ylimp1)) ylim else state$ylimp1
      xlim <- range(unlist(lapply(dnctd, function(k) k[[input$profile1var]])), na.rm=TRUE)
      xlim <- if(is.null(state$xlimp1)) xlim else state$xlimp1
      
      plot(dnctd[[1]][[input$profile1var]], dnctd[[1]][['pressure']],
           xlab = '',
           ylab = ylab,
           type = 'b',
           xaxs = 'r',
           yaxs = 'r',
           xlim = xlim,
           ylim = ylim, 
           axes = FALSE,
           col = 'white')
      axis(3)
      mtext(xlab, side = 3, line = axisNameLoc)
      axis(2)
      box()
      grid()
      okprofiles <- profiles >= as.numeric(input$profileRng1p1) & profiles <= as.numeric(input$profileRng2p1) 
      if(state$dnp1 == TRUE){
        dnctdp <- dnctd[okprofiles]
        for(i in 1:length(dnctdp)){
          lines(dnctdp[[i]][[input$profile1var]], dnctdp[[i]][['pressure']],
                type = 'b')
        }
      }
      if(state$upp1 == TRUE){
        upctdp <- upctd[okprofiles]
        for(i in 1:length(upctdp)){
          lines(upctdp[[i]][[input$profile1var]], upctdp[[i]][['pressure']],
                type = 'b', col = 'red')
        }
      }
    })
    
    output$profile2 <- renderPlot({
      # nearly identical code to profile1
      # but takes the chosen variable
      # will also change ylim based on brush
      # from profile1
      mgp <- getOption('oceMgp')
      mar <- c(1, mgp[1]+1.5, mgp[1]+1.5, mgp[1])
      axisNameLoc <- mgp[1]
      par(mgp = mgp, mar = mar)
      ylab <- resizableLabel(item = 'p', axis = 'y')
      xlab <- switch(input$profile2var,
                     'temperature' = resizableLabel('T', axis = 'x'),
                     'salinity' = resizableLabel('S', axis = 'x'),
                     'conductivity' = resizableLabel('conductivity S/m', axis = 'x'),
                     'sigmaTheta' = resizableLabel('sigmaTheta', axis = 'x'),
                     'chlorophyll' = 'Chlorophyll',
                     'cdom' = 'CDOM',
                     'backscatter' = 'Backscatter',
                     'oxygenConcentration' = resizableLabel('oxygen mL/L', axis = 'y'),
                     'oxygenSaturation' = 'Oxygen Saturation [%]')
      ylim <- rev(range(unlist(lapply(dnctd, function(k) k[['pressure']]))))
      ylim <- if(is.null(state$ylimp1)) ylim else state$ylimp1
      xlim <- range(unlist(lapply(dnctd, function(k) k[[input$profile2var]])), na.rm=TRUE)
      #xlim <- if(is.null(state$xlimp1)) xlim else state$xlimp1
      
      plot(dnctd[[1]][[input$profile2var]], dnctd[[1]][['pressure']],
           xlab = '',
           ylab = ylab,
           type = 'b',
           xaxs = 'r',
           yaxs = 'r',
           xlim = xlim,
           ylim = ylim, 
           axes = FALSE,
           col = 'white')
      axis(3)
      mtext(xlab, side = 3, line = axisNameLoc)
      axis(2)
      box()
      grid()
      okprofiles <- profiles >= as.numeric(input$profileRng1p1) & profiles <= as.numeric(input$profileRng2p1) 
      if(state$dnp1 == TRUE){
        dnctdp <- dnctd[okprofiles]
        for(i in 1:length(dnctdp)){
          lines(dnctdp[[i]][[input$profile2var]], dnctdp[[i]][['pressure']],
                type = 'b')
        }
      }
      if(state$upp1 == TRUE){
        upctdp <- upctd[okprofiles]
        for(i in 1:length(upctdp)){
          lines(upctdp[[i]][[input$profile2var]], upctdp[[i]][['pressure']],
                type = 'b', col = 'red')
        }
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
    # reset plots 
    # navigation section
    observeEvent(input$resetNav, {
      state$xlim <- range(glider$time,na.rm = TRUE)
    })
    observeEvent(input$plot_click, {
      state$xlim <- range(glider$time,na.rm = TRUE)
      })
    # science section
    observeEvent(input$resetSci, {
      state$xlim <- range(glider$time,na.rm = TRUE)
    })
    
    # profile1 plot
    observeEvent(input$profile1brush,{
      state$xlimp1 <- c(input$profile1brush$xmin, input$profile1brush$xmax)
      state$ylimp1 <- c(input$profile1brush$ymax, input$profile1brush$ymin)
    })
    # reset profile plots
    observeEvent(input$resetp1,{
      state$ylimp1 <- NULL
      state$xlimp1 <- NULL
      #state$ylimp1 <- rev(range(unlist(lapply(dnctd, function(k) k[['pressure']]))))
      #state$xlimp1 <- range(unlist(lapply(dnctd, function(k) k[[input$profile1var]])), na.rm=TRUE)
    })
    observeEvent(input$plot_click, {
      state$ylimp1 <- NULL
      state$xlimp1 <- NULL
    })
    
  }) #closes download observeEvent


}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
