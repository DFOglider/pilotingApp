rm(list=ls())
library(shiny)
#load("R:/Shared/Gliders/SEA0019/Data/M29/currentMission.RData")

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
                  choices = c('Alarm'='alarm','Pitch'='Pitch','Vertical Speed'='VertSpeed','Battery Voltage'='BatterieVolt','Internal Pressure'='int_pres','Heading'='Heading','Ballast'='BallastPos','Angular'='AngPos','Linear'='LinPos','Roll'='Roll','Yo Numbers'='profileNumber'),
                  selected = 'Pitch')),
           
        conditionalPanel(
          condition="input.Var=='Science'", 
            radioButtons(inputId = "SciVar",
                      label = "Variables:",
                      choices = c('Map'='Map','Map close up'='Mapcloseup','Temperature'='Temp','Conductivity'='Cond','Salinity'='Sal','Density'='Dens','Dissolved Oxygen'='DOF','Chlorophyl'='CHL_scaled','CDOM'='CDOM_scaled','BB_700nm'='BB_scaled'),
                      selected = 'Map')),
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
  load("R:/Shared/Gliders/SEA0019/Data/M29/currentMission.RData")
  #print(paste("R:/Shared/Gliders/",input$Glider,"/Data/M",input$Mission,"/currentMission.RData",sep=""))
  #load(paste("R:/Shared/Gliders/`,input$Glider,`/Data/M`,input$Mission,`/currentMission.RData",sep=""))
    #ls()
  #})
  output$plot1 <- renderPlot({
 # if (input$Var == 'Navigation') {
    
      if (is.null(state$xlim)) {
        par(xaxs='i',yaxs='i')#tight
        plot(glider$time, glider$depth,type="n",ylim=rev(range(glider$altHit,na.rm = TRUE)),xlim=(range(glider$time, na.rm = TRUE)),ylab='Depth (m)',xlab='Time')
        points(glider$time,glider$altHit,pch=19,cex = 1, col = "red")
        points(glider$time, glider$depth, pch=19,cex = 1, col = "dark blue")
        grid()
      } else {
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
        plot(glider$time, glider[[input$NavVar]],
             xlim=(range(glider$time, na.rm = TRUE)),
             xlab='Time',ylab='',type='n')
        lines(glider$time, glider[[input$NavVar]],lwd = 2, col = "red")
        lines(glider$time, glider$DesiredHeading,lwd = 2, col = "blue")
        grid()
        } else {
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
        plot(glider$time, glider[[input$NavVar]],
             xlim=(range(glider$time, na.rm = TRUE)),
             xlab='Time',ylab='',type='n')
        lines(glider$time, glider[[input$NavVar]],lwd = 3, col = "red")
        lines(glider$time, glider$BallastCmd,lwd = 2, col = "blue")
        grid()
        } else {
          par(xaxs='i',yaxs='i')#tight
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
        plot(glider$time, glider[[input$NavVar]],
             xlim=(range(glider$time, na.rm = TRUE)),
             xlab='Time',ylab='',type='n')
        lines(glider$time, glider[[input$NavVar]],lwd = 3, col = "red")
        lines(glider$time, glider$LinCmd,lwd = 2, col = "blue")
        grid()
        } else {
          par(xaxs='i',yaxs='i')#tight
          plot(glider$time, glider[[input$NavVar]],
               xlim=state$xlim,
               xlab='Time',ylab='',type='n')
          lines(glider$time, glider[[input$NavVar]],lwd = 3, col = "red")
          lines(glider$time, glider$LinCmd,lwd = 2, col = "blue")
          grid()
        } 
        
      } else if (input$NavVar=='AngPos') {
        if (is.null(state$brushed)) {
        par(xaxs='i',yaxs='i')#tight
        plot(glider$time, glider[[input$NavVar]],
             xlim=(range(glider$time, na.rm = TRUE)),
             xlab='Time',ylab='',type='n')
        lines(glider$time, glider[[input$NavVar]],lwd = 3, col = "red")
        lines(glider$time, glider$AngCmd,lwd = 2, col = "blue")
        grid()
        } else {
          par(xaxs='i',yaxs='i')#tight
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
      plot(glider$time, glider[[input$NavVar]],
           xlim=(range(glider$time, na.rm = TRUE)),
           xlab='Time',ylab='',type="n")
      lines(glider$time, glider[[input$NavVar]],col='blue',lwd=2)
      grid()
        } else {
          par(xaxs='i',yaxs='i')#tight
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
      mapPlot(coastlineWorldFine,  projection='+proj=wintri +lon_0=-68',
              longitudelim=lonlim, latitudelim=latlim) 
      mapLines(coastlineWorldFine)
      mapLines(Lon,Lat,type='l',col='red',lwd=3)
      mapPoints(Lon[length(Lon)],Lat[length(Lat)],pch=19,cex = 1, col = "dark blue")
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
shinyApp(ui = ui, server = server)
