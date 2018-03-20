rm(list=ls())
library(shiny)
library(oce)
library(ocedata)
library(measurements)
library(leaflet)
library(RCurl)
library(geosphere)
library(XML)
options(oceEOS='unesco') # prevent error for calculated values using swSigmaTheta, etc 

source('readSeaExplorerRealTime.R') # read in real time seaExplorer data
source('readSeaExplorerKml.R') # gets lon lat from kml file
source('oxygenCalibrationCoefficients.R') # used to convert oxygen from units of Hz to ml/l
source('swSatO2.R') # for use in sbeO2Hz2Sat.R
source('sbeO2Hz2Sat.R') # calculate oxygen from Hz to ml/l from seaBird instrument
source('downloadData.R') # obtain glidernames and missions from ftp and downloads
data('coastlineWorldFine')

mardef <- c(5.1, 4.1, 4.1, 2.1) # default margins
marcm <- c(5.1, 4.1, 4.1, 6.1) # color bar with zlab margins

#deployment/recovery location
drlon <- -63.406418 
drlat <- 44.520789

# halifax line stations
hfxlon <- c(-63.450000, -63.317000, -62.883000, -62.451000, -62.098000, -61.733000, -61.393945)
hfxlat <- c(44.400001, 44.267001, 43.883001, 43.479000, 43.183000, 42.850000, 42.531138)

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
         selectInput(inputId="Var", 
                     label="Data Set:", 
                     choices=c('Navigation'='Navigation','Science'='Science'), 
                     selected = 'Navigation'),
        
        conditionalPanel(
          condition="input.Var=='Navigation'", 
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
           
        conditionalPanel(
          condition="input.Var=='Science'", 
            radioButtons(inputId = "SciVar",
                      label = "Variables:",
                      choices = c('Temperature'='Temp',
                                  'Conductivity'='Cond',
                                  'Salinity'='Sal',
                                  'Density'='Dens',
                                  'Oxygen Frequency'='DOF',
                                  'Oxygen Concentration' = 'OxySat',
                                  'Chlorophyl'='CHL_scaled',
                                  'CDOM'='CDOM_scaled',
                                  'BB_700nm'='BB_scaled'),
                      selected = 'Temp'),
            uiOutput('sciScaleBar')),
        
        conditionalPanel(
          condition = "input.Var == 'Navigation'",
          actionButton("resetNav", "Reset plot")),
      
        conditionalPanel(
          condition = "input.Var == 'Science'",
          actionButton("resetSci", "Reset plot"))
             )),
    
    # Main panel for displaying outputs ----
    column(10,
      tabsetPanel(type = 'tabs',
        tabPanel("Plots",
        #column(10, 
               plotOutput("plot1",brush = brushOpts(id="plot_brush",
                                             direction="x",
                                             resetOnNew = TRUE),
                                            height="310px"),
               plotOutput("plot2", height="310px")
      ),
      tabPanel("Map",
        leafletOutput("map", height = '620px'))
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
  
  # CL enter output for sciScaleBar based on input$SciVar
  #  variable names 
  # c('Map'='Map','Map close up'='Mapcloseup','Temperature'='Temp','Conductivity'='Cond',
  #   'Salinity'='Sal','Density'='Dens','Dissolved Oxygen'='DOF','Chlorophyl'='CHL_scaled',
  #   'CDOM'='CDOM_scaled','BB_700nm'='BB_scaled'),
  # output$sciScaleBar <- renderUI({
  #   rng <- switch(input$SciVar,
  #                 'Temp' = c(-2, 22),
  #                 'Sal' = c(29, 35.5),
  #                 'Cond' = c(0,7),
  #                 'Dens' = c(20, 28),
  #                 'CHL_scaled' = c(-.02,5),
  #                 'CDOM_scaled' = c(-12,12),
  #                 'BB_scaled' = c(-0.005, 0.005),
  #                 'DOF' = c(2000, 5000),
  #                 'OxySat' = c(0,10))
  #   value <- switch(input$SciVar,
  #                   'Temp' = range(PLD$Temp, na.rm = TRUE),
  #                   'Sal' = range(PLD$Sal, na.rm = TRUE),
  #                   'Cond' = range(PLD$Conduc, na.rm = TRUE),
  #                   'Dens' = range(PLD$SigTheta, na.rm = TRUE),
  #                   'CHL_scaled' = range(PLD$CHL_scaled, na.rm = TRUE),
  #                   'CDOM_scaled' = range(PLD$CDOM_scaled, na.rm = TRUE),
  #                   'BB_scaled' = range(PLD$BB_scaled, na.rm = TRUE),
  #                   'DOF' = range(PLD$DOF, na.rm = TRUE),
  #                   'OxySat' = range(PLD$OxySat, na.rm = TRUE))
  #   step <- switch(input$SciVar,
  #                  'Temp' = 0.5,
  #                  'Sal' = 0.1,
  #                  'Cond' = 0.01,
  #                  'Dens' = 0.1,
  #                  'CHL_scaled' = 1,
  #                  'BB_scaled' = 0.0005,
  #                  'DOF' = 100,
  #                  'OxySat' = 0.5)
  #   sliderInput("sciLimits", "Choose colorbar limits:", min = rng[1], max = rng[2],
  #               value = value, step = step, animate = FALSE)  
  #   
  # })
  
  # download data and load when actionButton clicked
  # make plots too
  observeEvent(input$download,{
    # download and process data
    downloadData(datadir = datadir, glider = input$Glider, mission = input$Mission)
    data <- readSeaExplorerRealTime(datadir = datadir, glider = input$Glider, mission = input$Mission)
    PLD <- data$PLD
    glider <- data$NAV
    kmlcoord <- readSeaExplorerKml(datadir = datadir, glider = input$Glider, mission = input$Mission)
    okkml <- !is.na(kmlcoord$lon)
    kmlLon <- kmlcoord$lon[okkml]
    kmlLat <- kmlcoord$lat[okkml]  
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
                    'OxySat' = c(0,10))
      value <- switch(input$SciVar,
                      'Temp' = unname(quantile(PLD$Temp, probs = c(0.01, 0.99), na.rm = TRUE)),
                      'Sal' = unname(quantile(PLD$Sal, probs = c(0.02, 0.99),  na.rm = TRUE)),
                      'Cond' = unname(quantile(PLD$Conduc, probs = c(0.02, 0.99), na.rm = TRUE)),
                      'Dens' = unname(quantile(PLD$SigTheta, probs = c(0.02, 0.99), na.rm = TRUE)),
                      'CHL_scaled' = unname(quantile(PLD$CHL_scaled, probs = c(0.01, 0.99), na.rm = TRUE)),
                      'CDOM_scaled' = unname(quantile(PLD$CDOM_scaled, probs = c(0.01, 0.99), na.rm = TRUE)),
                      'BB_scaled' = unname(quantile(PLD$BB_scaled, probs = c(0.01, 0.99), na.rm = TRUE)),
                      'DOF' = unname(quantile(PLD$DOF, probs = c(0.01, 0.97), na.rm = TRUE)),
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
                     'OxySat' = 0.5)
      sliderInput("sciLimits", "Choose colorbar limits:", min = rng[1], max = rng[2],
                  value = value, step = step, animate = FALSE)  
      
    })
    # plot1 - top plot
    output$plot1 <- renderPlot({
      if (is.null(state$xlim)) {
        par(mar = marcm)
        par(xaxs='i',yaxs='i')#tight
        plot(glider$time, glider$depth,type="n",ylim=rev(range(glider$altHit,na.rm = TRUE)),xlim=(range(glider$time, na.rm = TRUE)),ylab='Depth (m)',xlab='Time')
        points(glider$time,glider$altHit,pch=20,cex = 1, col = "red")
        points(glider$time, glider$depth, pch=20,cex = 1, col = "dark blue")
        grid()
      } else {
        par(mar = marcm)
        par(xaxs='i',yaxs='i')#tight
        plot(glider$time, glider$depth,type="n",ylim=rev(range(glider$altHit,na.rm = TRUE)),xlim=state$xlim,ylab='Depth (m)',xlab='Time')
        points(glider$time,glider$altHit,pch=20,cex = 1, col = "red")
        points(glider$time, glider$depth, pch=20,cex = 1, col = "dark blue")
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
          par(mar = marcm)
          par(xaxs='i',yaxs='i')#tight
          plot(glider$time, glider$depth, 
               type = "n", 
               ylim = rev(range(glider$altHit,na.rm = TRUE)), 
               xlim = range(glider$time, na.rm = TRUE), 
               ylab = 'Depth (m)', 
               xlab = 'Time')
          points(glider$time,glider$altHit,pch=20,cex = 1, col = "red")
          points(glider$time, glider$depth, pch=20,cex = 1, col = "dark blue")
          grid()
        } else {
          okylim <- glider$time > state$xlim[1] & glider$time < state$xlim[2]
          par(mar = marcm)
          par(xaxs='i', yaxs='i')#tight
          plot(glider$time, glider$depth, 
               type = "n", 
               ylim = rev(range(glider$altHit[okylim],na.rm = TRUE) + c(-0.5, 0.5 )), 
               xlim = state$xlim, 
               ylab = 'Depth (m)', 
               xlab = 'Time')
          points(glider$time, glider$altHit, pch=20,cex = 1, col = "red")
          points(glider$time, glider$depth, pch=20,cex = 1, col = "dark blue")
          grid()
        }
      } else if (input$NavVar=='Pitch') {
        if (is.null(state$xlim)) {
          par(mar=marcm)
           par(xaxs='i',yaxs='i')#tight
           plot(glider$time, glider[[input$NavVar]],
           ylim=c(-80,40),
           xlim=(range(glider$time, na.rm = TRUE)),
           xlab='Time',ylab='',type='n')
           polygon(c(glider$time,rev(glider$time)),c(rep(15,length(glider$time)),rep(25,length(glider$time))),col=gray(0.8),border=NA)  
           polygon(c(glider$time,rev(glider$time)),c(rep(-15,length(glider$time)),rep(-25,length(glider$time))),col=gray(0.8),border=NA) 
           lines(glider$time, glider[[input$NavVar]],lwd = 2, col = "black")
           grid()
        } else {
          par(mar = marcm)
          par(xaxs='i',yaxs='i')#tight
          plot(glider$time, glider[[input$NavVar]],
          ylim=c(-80,40),
          xlim=state$xlim,
          xlab='Time',ylab='',type='n')
          polygon(c(glider$time,rev(glider$time)),c(rep(15,length(glider$time)),rep(25,length(glider$time))),col=gray(0.8),border=NA)  
          polygon(c(glider$time,rev(glider$time)),c(rep(-15,length(glider$time)),rep(-25,length(glider$time))),col=gray(0.8),border=NA) 
          lines(glider$time, glider[[input$NavVar]],lwd = 2, col = "black")
          grid()
        }
      } else if (input$NavVar=='VertSpeed') {
        if (is.null(state$xlim)) {
        par(xaxs='i',yaxs='i')#tight
        par(mar = marcm)
        plot(glider$time, glider[[input$NavVar]],
             ylim=c(-30,30),
             xlim=(range(glider$time, na.rm = TRUE)),
             xlab='Time',ylab='',type='n')
        polygon(c(glider$time,rev(glider$time)),c(rep(-13,length(glider$time)),rep(-17,length(glider$time))),col=gray(0.8),border=NA)  
        polygon(c(glider$time,rev(glider$time)),c(rep(13,length(glider$time)),rep(17,length(glider$time))),col=gray(0.8),border=NA) 
        lines(glider$time, glider[[input$NavVar]],lwd = 2, col = "black")
        grid()
        } else {
          par(xaxs='i',yaxs='i')#tight
          par(mar = marcm)
          plot(glider$time, glider[[input$NavVar]],
               ylim=c(-30,30),
               xlim=state$xlim,
               xlab='Time',ylab='',type='n')
          polygon(c(glider$time,rev(glider$time)),c(rep(-13,length(glider$time)),rep(-17,length(glider$time))),col=gray(0.8),border=NA)  
          polygon(c(glider$time,rev(glider$time)),c(rep(13,length(glider$time)),rep(17,length(glider$time))),col=gray(0.8),border=NA) 
          lines(glider$time, glider[[input$NavVar]],lwd = 2, col = "black")
          grid()
        }
        
      } else if (input$NavVar=='BatterieVolt') {
        if (is.null(state$xlim)) {
          par(mar = marcm)
         par(xaxs='i',yaxs='i')#tight
        plot(glider$time, glider[[input$NavVar]],
             ylim=c(24,30),
             xlim=(range(glider$time, na.rm = TRUE)),
             xlab='Time',ylab='',type='n')
        polygon(c(glider$time,rev(glider$time)),c(rep(24,length(glider$time)),rep(26,length(glider$time))),col=gray(0.8),border=NA)  
        lines(glider$time, glider[[input$NavVar]],lwd = 2, col = "red")
        grid()
        } else {
          par(xaxs='i',yaxs='i')#tight
          par(mar = marcm)
          plot(glider$time, glider[[input$NavVar]],
               ylim=c(24,30),
               xlim=state$xlim,
               xlab='Time',ylab='',type='n')
          polygon(c(glider$time,rev(glider$time)),c(rep(24,length(glider$time)),rep(26,length(glider$time))),col=gray(0.8),border=NA)  
          lines(glider$time, glider[[input$NavVar]],lwd = 2, col = "red")
          grid()
        }
        
      } else if (input$NavVar=='speedms') {
        if (is.null(state$xlim)) {
          par(mar = marcm)
          par(xaxs='i',yaxs='i')#tight
          plot(glider$time, glider[[input$NavVar]],
               ylim=c(0,max(glider[[input$NavVar]],na.rm = TRUE)),
               xlim=(range(glider$time, na.rm = TRUE)),
               xlab='Time',ylab='(m/s)')
          points(glider$time, glider[[input$NavVar]], pch=19,cex = 1, col = "dark green")
          grid()
        } else {
          par(mar = marcm)
          par(xaxs='i',yaxs='i')#tight
          plot(glider$time, glider[[input$NavVar]],
               ylim=c(0,max(glider[[input$NavVar]],na.rm = TRUE)),
               xlim=state$xlim,
               xlab='Time',ylab='(m/s)')
          points(glider$time, glider[[input$NavVar]], pch=19,cex = 1, col = "dark green")
          grid()
        }
        
      } else if (input$NavVar=='distkm') {
        if (is.null(state$xlim)) {
          par(mar = marcm)
          par(xaxs='i',yaxs='i')#tight
          plot(glider$time, glider[[input$NavVar]],
               ylim=c(0,max(glider[[input$NavVar]],na.rm = TRUE)),
               xlim=(range(glider$time, na.rm = TRUE)),
               xlab='Time',ylab='(km)')
          points(glider$time, glider[[input$NavVar]], pch=19,cex = 1, col = "dark green")
          grid()
        } else {
          par(mar = marcm)
          par(xaxs='i',yaxs='i')#tight
          plot(glider$time, glider[[input$NavVar]],
               ylim=c(0,max(glider[[input$NavVar]],na.rm = TRUE)),
               xlim=state$xlim,
               xlab='Time',ylab='(km)')
          points(glider$time, glider[[input$NavVar]], pch=19,cex = 1, col = "dark green")
          grid()
        }
        
      } else if (input$NavVar=='Temperature') {
        if (is.null(state$xlim)) {
          par(mar = marcm)
          par(xaxs='i',yaxs='i')#tight
          plot(glider$time, glider[[input$NavVar]],
               ylim=c(0,max(glider[[input$NavVar]])+3),
               xlim=(range(glider$time, na.rm = TRUE)),
               xlab='Time',ylab='',type='n')
          lines(glider$time, glider[[input$NavVar]],lwd = 2, col = "red")
          grid()
        } else {
          par(mar = marcm)
          par(xaxs='i',yaxs='i')#tight
          plot(glider$time, glider[[input$NavVar]],
               ylim=c(0,max(glider[[input$NavVar]])+3),
               xlim=state$xlim,
               xlab='Time',ylab='',type='n')
           lines(glider$time, glider[[input$NavVar]],lwd = 2, col = "red")
          grid()
        }
        
      } else if (input$NavVar=='Heading') {
        if (is.null(state$xlim)) {
        par(xaxs='i',yaxs='i')#tight
        par(mar = marcm)
        plot(glider$time, glider[[input$NavVar]],
             xlim=(range(glider$time, na.rm = TRUE)),
             xlab='Time',ylab='',type='n')
        lines(glider$time, glider[[input$NavVar]],lwd = 2, col = "red")
        lines(glider$time, glider$DesiredHeading,lwd = 2, col = "blue")
        grid()
        } else {
          par(mar = marcm)
          par(xaxs='i',yaxs='i')#tight
          plot(glider$time, glider[[input$NavVar]],
               xlim=state$xlim,
               xlab='Time',ylab='',type='n')
          lines(glider$time, glider[[input$NavVar]],lwd = 2, col = "red")
          lines(glider$time, glider$DesiredHeading,lwd = 2, col = "blue")
          grid()
        }
        
      } else if (input$NavVar=='BallastPos') {
        if (is.null(state$xlim)) {
        par(xaxs='i',yaxs='i')#tight
        par(mar = marcm)
        plot(glider$time, glider[[input$NavVar]],
             xlim=(range(glider$time, na.rm = TRUE)),
             xlab='Time',ylab='',type='n')
        lines(glider$time, glider[[input$NavVar]],lwd = 3, col = "red")
        lines(glider$time, glider$BallastCmd,lwd = 2, col = "blue")
        grid()
        } else {
          par(xaxs='i',yaxs='i')#tight
          par(mar = marcm)
          plot(glider$time, glider[[input$NavVar]],
               xlim=state$xlim,
               xlab='Time',ylab='',type='n')
          lines(glider$time, glider[[input$NavVar]],lwd = 3, col = "red")
          lines(glider$time, glider$BallastCmd,lwd = 2, col = "blue")
          grid()
        }
        
      } else if (input$NavVar=='LinPos') {
        if (is.null(state$xlim)) {
        par(xaxs='i',yaxs='i')#tight
        par(mar = marcm)
        plot(glider$time, glider[[input$NavVar]],
             xlim=(range(glider$time, na.rm = TRUE)),
             xlab='Time',ylab='',type='n')
        lines(glider$time, glider[[input$NavVar]],lwd = 3, col = "red")
        lines(glider$time, glider$LinCmd,lwd = 2, col = "blue")
        grid()
        } else {
          par(xaxs='i',yaxs='i')#tight
          par(mar = marcm)
          plot(glider$time, glider[[input$NavVar]],
               xlim=state$xlim,
               xlab='Time',ylab='',type='n')
          lines(glider$time, glider[[input$NavVar]],lwd = 3, col = "red")
          lines(glider$time, glider$LinCmd,lwd = 2, col = "blue")
          grid()
        } 
        
      } else if (input$NavVar=='AngPos') {
        if (is.null(state$xlim)) {
        par(mar = marcm)
        par(xaxs='i',yaxs='i')#tight
        plot(glider$time, glider[[input$NavVar]],
             xlim=(range(glider$time, na.rm = TRUE)),
             xlab='Time',ylab='',type='n')
        lines(glider$time, glider[[input$NavVar]],lwd = 3, col = "red")
        lines(glider$time, glider$AngCmd,lwd = 2, col = "blue")
        grid()
        } else {
          par(xaxs='i',yaxs='i')#tight
          par(mar = marcm)
          plot(glider$time, glider[[input$NavVar]],
               xlim=state$xlim,
               xlab='Time',ylab='',type='n')
          lines(glider$time, glider[[input$NavVar]],lwd = 3, col = "red")
          lines(glider$time, glider$AngCmd,lwd = 2, col = "blue")
          grid()
        } 
        
      } else { #profileNumber
        if (is.null(state$xlim)) {  
      par(xaxs='i',yaxs='i')#tight
      par(mar = marcm)
      plot(glider$time, glider[[input$NavVar]],
           xlim=(range(glider$time, na.rm = TRUE)),
           xlab='Time',ylab='',type="n")
      lines(glider$time, glider[[input$NavVar]],col='blue',lwd=2)
      grid()
        } else {
          okylim <- glider$time > state$xlim[1] & glider$time < state$xlim[2]
          par(xaxs='i',yaxs='i')#tight
          par(mar = marcm)
          plot(glider$time, glider[[input$NavVar]],
               xlim=state$xlim,
               ylim = range(glider[[input$NavVar]][okylim], na.rm = TRUE),
               xlab='Time',ylab='',type="n")
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
                       'DOF' = 'Dissolved Oxygen [Hz',
                       'OxySat' = resizableLabel('oxygen mL/L', axis = 'y'))
        cm <- colormap(data, zlim = input$sciLimits)
        ylabp <- resizableLabel('p', axis = 'y')
        par(xaxs='i',yaxs='i', mar=mardef)
        drawPalette(colormap = cm, zlab = zlab)
        # match top panel, so use range of altHits for ylim
        #                  and nav time for xlim
        if (is.null(state$xlim)) {
          plot(PLD$timesci, PLD$Press,
              ylim = rev(range(glider$altHit,na.rm = TRUE)),
              xlim = (range(glider$time, na.rm = TRUE)),
              pch = 20, col = cm$zcol,
              xlab = '', ylab = '')

        } else {
          okylim <- glider$time > state$xlim[1] & glider$time < state$xlim[2]
          plot(PLD$timesci, PLD$Press,
               ylim = rev(range(glider$depth[okylim],na.rm = TRUE)),
               xlim=state$xlim,
               pch = 20, col = cm$zcol,
               xlab = '', ylab = '')
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
    
    output$map <- renderLeaflet({
      leaflet(as.data.frame(cbind(glon, glat)))%>%
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
                                       paste0(as.character(round(drlat,3)), ',', as.character(round(drlon,3)))),
                         label = paste0("Deployment/Recovery Location"))%>%
        # halifax line
        addCircleMarkers(lng = hfxlon, lat = hfxlat,
                         radius = 5, fillOpacity = .4, stroke = F, 
                         color = 'black',
                         popup = paste(sep = "<br/>",
                                       paste0("HL", as.character(1:7)),
                                       paste0(as.character(round(hfxlat,3)), ',', as.character(round(hfxlon,3)))),
                         label = paste0("HL", 1:7)) %>%
        # glider positions
        addCircleMarkers(lng = glon, lat = glat, 
                         radius = 4, fillOpacity = .2, stroke = F,
                         popup = paste(sep = "<br/>",
                                       "Glider position",
                                       as.character(PLD$timesci[okloc]),
                                       paste0(as.character(round(glat,3)), ', ', as.character(round(glon,3)))),
                         label = paste0('Glider position: ', as.character(PLD$timesci[okloc])),
                         group = map_allposition)%>%
        # positions from kml
        addCircleMarkers(lng = kmlLon, lat = kmlLat, 
                         radius = 5, fillOpacity = .4, stroke = F,
                         color = 'red',
                         popup = paste(sep = "<br/>",
                                       "Glider position kml",
                                       #as.character(PLD$timesci[okloc]),
                                       paste0(as.character(round(kmlLat,3)), ', ', as.character(round(kmlLon,3)))),
                         label = paste0('Glider position kml: ', 1:length(kmlLat)),
                         group = map_kml)%>%
        # last received / current location
        addCircleMarkers(lng = glon[length(glon)], lat = glat[length(glon)],
                         radius = 4, fillOpacity = 1, stroke = F,
                         popup = paste(sep = "<br/>",
                                       "Last location received",
                                       as.character(PLD$timesci[okloc][length(glon)]),
                                       paste0(as.character(round(glat[length(glon)],3)), ', ', as.character(round(glon[length(glon)],3)))),
                         label = paste0("Last location received:", as.character(PLD$timesci[okloc][length(glon)])),
                         color = 'green',
                         group = map_lastlocation) %>%
        # layer control legend
        addLayersControl(overlayGroups = c(map_allposition,
                                           map_track,
                                           map_lastlocation,
                                           map_kml),
                         options = layersControlOptions(collapsed = FALSE), 
                         position = 'bottomright')
    }) #closes leafletplot
    
    # brush plots
    observeEvent(input$plot_brush, {
     
      #df <- data.frame(x=glider$time, x=glider[[input$NavVar]])
      #state$brushed <- brushedPoints(df, input$plot_brush, "x", "y", allRows=TRUE)$selected_
      
      state$xlim <- c(input$plot_brush$xmin, input$plot_brush$xmax)
      
    })

    # reset plots 
    observeEvent(input$resetNav, {
      state$xlim <- range(glider$time,na.rm = TRUE)
    })
    observeEvent(input$resetSci, {
      state$xlim <- range(glider$time,na.rm = TRUE)
    })
    
    
  }) #closes download observeEvent


}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
