rm(list=ls())
library(shiny)
library(measurements)
library(oce)
library(ocedata)
data('coastlineWorldFine')
#load("R:/Shared/Gliders/SEA0019/Data/M29/currentMission.RData")

mardef <- c(5.1, 4.1, 4.1, 2.1)
marcm <- c(5.1, 4.1, 4.1, 6.1)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Glider Data"),
  
  fluidRow(
    column(2, wellPanel(
         selectInput(inputId="Glider", label="Glider:", choices=c('SEA019'='SEA019','SEA021'='SEA021','SEA022'='SEA022','SEA024'='SEA024'), selected = 'Navigation'),
         numericInput(inputId="Mission",label="Mission Number:",value='29', min = '10', max = NA),
         actionButton("LoadData", "Load Data"),
         selectInput(inputId="Var", label="Data Set:", choices=c('Navigation'='Navigation','Science'='Science'), selected = 'Navigation'),
        
        conditionalPanel(
          condition="input.Var=='Navigation'", 
          radioButtons(inputId = "NavVar",
                  label = "Variables:",
                  choices = c('Alarm'='alarm',
                              'Pitch'='Pitch',
                              'Vertical Speed'='VertSpeed',
                              'Battery Voltage'='BatterieVolt',
                              'Internal Pressure'='int_pres',
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
                      choices = c('Map'='Map',
                                  'Map close up'='Mapcloseup',
                                  'Temperature'='Temp',
                                  'Conductivity'='Cond',
                                  'Salinity'='Sal',
                                  'Density'='Dens',
                                  'Dissolved Oxygen'='DOF',
                                  'Oxygen Saturation' = 'OxySat',
                                  'Chlorophyl'='CHL_scaled',
                                  'CDOM'='CDOM_scaled',
                                  'BB_700nm'='BB_scaled'),
                      selected = 'Map'),
            uiOutput('sciScaleBar')),
      #  conditionalPanel(
      #    condition = "input.Var == 'Navigation'",
      #    "Click/Drag top panel to zoom."),
        conditionalPanel(
          condition = "input.Var == 'Navigation'",
          actionButton("resetNav", "Reset plot")),
      
        conditionalPanel(
          condition = "input.Var == 'Science'",
          actionButton("resetSci", "Reset plot"))
             )),
    
    # Main panel for displaying outputs ----
    column(10,
        plotOutput("plot1",brush = brushOpts(id="plot_brush",
                                                    direction="x",
                                                    resetOnNew = TRUE),
                                  height="310px")),
    column(10,
        plotOutput("plot2", height="310px"))
        )
)


               
           

# Define server 
server <- function(input, output) {
  state <- reactiveValues()
  # Loading the data
  #local({
  load("R:/Shared/Gliders/SEA019/Data/M29/currentMission.RData")
  
  #print(paste("R:/Shared/Gliders/",input$Glider,"/Data/M",input$Mission,"/currentMission.RData",sep=""))
  #load(paste("R:/Shared/Gliders/`,input$Glider,`/Data/M`,input$Mission,`/currentMission.RData",sep=""))
    #ls()
  #})
  
  # CL enter output for sciScaleBar based on input$SciVar
  #  variable names 
  # c('Map'='Map','Map close up'='Mapcloseup','Temperature'='Temp','Conductivity'='Cond',
  #   'Salinity'='Sal','Density'='Dens','Dissolved Oxygen'='DOF','Chlorophyl'='CHL_scaled',
  #   'CDOM'='CDOM_scaled','BB_700nm'='BB_scaled'),
  output$sciScaleBar <- renderUI({
    rng <- switch(input$SciVar,
                  'Temp' = c(-5,45),
                  'Sal' = c(15, 45),
                  'Cond' = c(0,7),
                  'Dens' = c(0, 35),
                  'CHL_scaled' = c(-5,5),
                  'CDOM_scaled' = c(-12,12),
                  'BB_scaled' = c(-0.5, 0.5),
                  'DOF' = c(2000, 5000),
                  'OxySat' = c(0,10))
    value <- switch(input$SciVar,
                    'Temp' = range(PLD$Temp, na.rm = TRUE),
                    'Sal' = range(PLD$Sal, na.rm = TRUE),
                    'Cond' = range(PLD$Conduc, na.rm = TRUE),
                    'Dens' = range(PLD$SigTheta, na.rm = TRUE),
                    'CHL_scaled' = range(PLD$CHL_scaled, na.rm = TRUE),
                    'CDOM_scaled' = range(PLD$CDOM_scaled, na.rm = TRUE),
                    'BB_scaled' = range(PLD$BB_scaled, na.rm = TRUE),
                    'DOF' = range(PLD$DOF, na.rm = TRUE),
                    'OxySat' = range(PLD$OxySat, na.rm = TRUE))
    sliderInput("sciLimits", "Choose colorbar limits:", min = rng[1], max = rng[2],
                value = value, animate = FALSE)  
    
  })
  
  output$plot1 <- renderPlot({
 # if (input$Var == 'Navigation') {
      if (is.null(state$xlim)) {
        par(mar = marcm)
        par(xaxs='i',yaxs='i')#tight
        plot(glider$time, glider$depth,type="n",ylim=rev(range(glider$altHit,na.rm = TRUE)),xlim=(range(glider$time, na.rm = TRUE)),ylab='Depth (m)',xlab='Time')
        points(glider$time,glider$altHit,pch=19,cex = 1, col = "red")
        points(glider$time, glider$depth, pch=19,cex = 1, col = "dark blue")
        grid()
      } else {
        par(mar = marcm)
        par(xaxs='i',yaxs='i')#tight
        plot(glider$time, glider$depth,type="n",ylim=rev(range(glider$altHit,na.rm = TRUE)),xlim=state$xlim,ylab='Depth (m)',xlab='Time')
        points(glider$time,glider$altHit,pch=19,cex = 1, col = "red")
        points(glider$time, glider$depth, pch=19,cex = 1, col = "dark blue")
        grid()
      }
    
 # } else if (input$Var == 'Science') {
  
 # }
  })
    output$plot2 <- renderPlot({
    if (input$Var == 'Navigation') {
      if (input$NavVar=='Pitch') {
        if (is.null(state$brushed)) {
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
        if (is.null(state$brushed)) {
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
        if (is.null(state$brushed)) {
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
        
      } else if (input$NavVar=='Heading') {
        if (is.null(state$brushed)) {
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
        if (is.null(state$brushed)) {
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
        if (is.null(state$brushed)) {
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
        if (is.null(state$brushed)) {
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
        
      } else {
        if (is.null(state$brushed)) {  
      par(xaxs='i',yaxs='i')#tight
      par(mar = marcm)
      plot(glider$time, glider[[input$NavVar]],
           xlim=(range(glider$time, na.rm = TRUE)),
           xlab='Time',ylab='',type="n")
      lines(glider$time, glider[[input$NavVar]],col='blue',lwd=2)
      grid()
        } else {
          par(xaxs='i',yaxs='i')#tight
          par(mar = marcm)
          plot(glider$time, glider[[input$NavVar]],
               xlim=state$xlim,
               xlab='Time',ylab='',type="n")
          lines(glider$time, glider[[input$NavVar]],col='blue',lwd=2)
          grid()
        }   
      }
    } else if (input$Var == 'Science') {
      Lontmp <-  sub("\\$", "", sub('(.{3})', '\\1 ', PLD$Lon))
      Lon <- as.numeric(conv_unit(Lontmp,from = 'deg_dec_min', to = 'dec_deg'))
      Lattmp <-  sub("\\$", "", sub('(.{2})', '\\1 ', PLD$Lat))
      Lat <- as.numeric(conv_unit(Lattmp,from = 'deg_dec_min', to = 'dec_deg'))
      
      if (input$SciVar=='Map') {
      data(coastlineWorldFine)
      lonlim <- c(mean(Lon,na.rm = TRUE)-3, mean(Lon,na.rm = TRUE)+3)
      latlim <- c(mean(Lat,na.rm = TRUE)-1, mean(Lat,na.rm = TRUE)+1)
      par(mar = marcm)
      mapPlot(coastlineWorldFine,  projection='+proj=wintri +lon_0=-68',
              longitudelim=lonlim, latitudelim=latlim) 
      mapLines(coastlineWorldFine)
      mapLines(Lon,Lat,type='l',col='red',lwd=3)
      mapPoints(Lon[length(Lon)],Lat[length(Lat)],pch=19,cex = 1, col = "dark blue")
      
      } else if (input$SciVar=='Mapcloseup') {
      data(coastlineWorldFine)
      lonlim <- c(Lon[length(Lon)]-0.25, Lon[length(Lon)]+0.25)
      latlim <- c(Lat[length(Lat)]-0.10, Lat[length(Lat)]+0.10)
      #par(new=TRUE,fig=c(0.05,0.5,0.55,0.80))
      par(mar = marcm)
      mapPlot(coastlineWorldFine,  projection='+proj=wintri +lon_0=-68',
              longitudelim=lonlim, latitudelim=latlim) 
      mapLines(coastlineWorldFine)
      mapLines(Lon,Lat,type='l',col='red',lwd=3)
      mapPoints(Lon[length(Lon)],Lat[length(Lat)],pch=19,cex = 1, col = "dark blue")
      } else if(input$SciVar != 'Map' & input$SciVar != 'Mapcloseup'){
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
        if (is.null(state$brushed)) {
        plot(PLD$timesci, PLD$Press,
             ylim = rev(range(glider$altHit,na.rm = TRUE)),
             xlim = (range(glider$time, na.rm = TRUE)),
             pch = 20, col = cm$zcol,
             xlab = '', ylab = '')

        } else {
          plot(PLD$timesci, PLD$Press,
               ylim = rev(range(glider$altHit,na.rm = TRUE)),
               xlim=state$xlim,
               pch = 20, col = cm$zcol,
               xlab = '', ylab = '')
        }
        grid()
        mtext(ylabp, side = 2, line = 2)
        par(mar=mardef)
      }
      
    }  
    })

    observeEvent(input$plot_brush, {
     
      df <- data.frame(x=glider$time, x=glider[[input$NavVar]])
      state$brushed <- brushedPoints(df, input$plot_brush, "x", "y", allRows=TRUE)$selected_
      
      state$xlim <- c(input$plot_brush$xmin, input$plot_brush$xmax)
      
    })

    observeEvent(input$resetNav, {
      state$xlim <- range(glider$time,na.rm = TRUE)
    })
    observeEvent(input$resetSci, {
      state$xlim <- range(glider$time,na.rm = TRUE)
    })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server, options = list("display.mode" = "showcase")) #see code when app is ran
#shinyApp(ui = ui, server = server)
