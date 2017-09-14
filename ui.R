#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("BSAM Visualizer - hDCRWS model"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      selectInput("track_id", "Select Track",
                  choices = track, 
                  selected=track[1])
      ,
      checkboxInput("towers", "Display Towers", value=FALSE),
      
      sliderInput("radius", "Select point radius",
                  min=1, max=15, value=7,
                  sep=""),
      sliderInput("maxdensity", "Select maximum density per cell",
                  min=1, max=15, value=5,
                  sep=""),
      
      
      selectInput("timespan", "Select Timespan (only single timespan for now)",
                  list("Single time" = "sing.t"), selected="sing.t"),
      ## if single year is selected, select year. if multiple years are selected, choose range.
      conditionalPanel(
        condition="input.timespan == 'sing.t'",
        ## Initializing a single slider
        sliderInput("track_time", "Select Time (% along the track)",
                    min=0.01, max=1, value=0.01,
                    sep="")
      )
      ,
      conditionalPanel(
        ## Initializes a multi-year slider (range)
        condition="input.timespan == 'mult.ts'",
        ## Slider starts from 2010-2012
        sliderInput("track_range", "Select the portion of the track to show",
                    min=0.01, max=1, value=c(0.01, 0.10),
                    sep="")
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      #leafletOutput("leafmap1", width=750, height=500)
 #     textOutput("track"),
 #     textOutput("steps"),
      #title for single timepoint
      conditionalPanel(
        condition="input.timespan == 'sing.t'",
        htmlOutput("singletitle")
      ),
      #title for single timepoint
      conditionalPanel(
        condition="input.timespan == 'mult.ts'",
        htmlOutput("multititle")
      ),
     # tableOutput("tab"),
      #tableOutput("tow"),
      
      leafletOutput("leafmap1"),
     plotlyOutput("traceplot")
     #, dataTableOutput("revitab"),
     #dataTableOutput("modtab")
      )
    )
  ))
