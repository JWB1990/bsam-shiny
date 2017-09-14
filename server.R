#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
densmod_r<- reactive({
    ## Output reactive dataframe
    densmod
  })



  
#track_r <- reactive({track})
stepids <- reactive({
  #first the filterering argument contains the track's timesteps
  #then filter by selected timespan
  if(input$timespan == "sing.t"){
    stepids <- as.numeric(mod[mod$id == input$track_id,]$stepid)
    
  stepids <- stepids[ceiling(length(stepids)*input$track_time)]
  
 }
 else if(input$timespan == "mult.ts"){
   stepids <- as.numeric(mod[mod$id == input$track_id,]$stepid)

   start=floor(length(stepids)*input$track_range[1])
   stepids <- stepids[start:ceiling(length(stepids)*input$track_range[2])]
 }
  stepids
})

###########################Labels for dates

begin <- reactive({
  #first the filterering argument contains the track's timesteps
  begin <- mod[min(stepids()),]$date
  begin
})
end <- reactive({
  end <- tail(mod[max(stepids()),],1)$date
  end
})

output$singletitle <- renderUI({
  HTML(paste("<b>", 
             "This is the animal's predicted location at", begin(), sep=" ",
             "</b>")
  )
})

output$multititle <- renderUI({
  HTML(paste("<b>", 
             "This is the animal's track segment between", begin(),"and", end(), sep=" ",
             "</b>")
  )
})



#output$tab <- renderTable(head(map_df()))
#output$tow <- renderTable(head(towers))

#output$track <- renderText(input$track_id)
#output$steps <- renderText(stepids())
output$begin <- renderText(begin())
output$end <- renderText(end())

  
#track id is chosen, makesetep selector


  map_df<-reactive({
    densmod_r <- densmod_r()
    map_df <- densmod_r  %>%
      #select(1,3,4,6, sel_col, 22,74,75) %>%
      filter(timestep %in% stepids())

    #colnames(map_df)[5]<-"var"
    #colnames(map_df)[6]<-"TSE"
    map_df$dens[map_df$dens>input$maxdensity] <- input$maxdensity
    map_df
  })

  #create maps in leaflet

  #opacity (circle rim opacity)
  op=0.5
  #fill opacity (color of circles)
  fop=0.7


  output$leafmap1<-renderLeaflet({

    leaflet()%>%
      setView(lng= -71.75, lat= 42, zoom=6) %>%
      addProviderTiles("Stamen.TonerLite") 
  })

  observeEvent(input$towers, {
    proxy=leafletProxy("leafmap1")  %>%
      addCircleMarkers(data=towers,
        lng=~lon, lat=~lat,
        weight = 5, opacity = 0.5,radius=2,
        fill = TRUE, fillColor = c("blue"), fillOpacity = 0.5,
        popup=~site,
        group='towerlocs')
    
    if(input$towers) proxy %>% showGroup('towerlocs')
       else proxy  %>% hideGroup('towerlocs')
  })


  #opacity (circle rim opacity)
  op=0.0001
  #fill opacity (color of circles)
  fop=0.3

radius_r <- reactive({input$radius})

observe({


    map_df<-map_df()
    radius <- radius_r()

    pal <- colorNumeric(
      palette = c("green", "red"),
      domain = c(0,input$maxdensity))

    ## Map Creation
    proxy=leafletProxy("leafmap1") %>% clearGroup('model')  %>%
      addCircleMarkers(group='model',
                       data=map_df,
        lng = ~lon, lat = ~lat,
        fillColor=~pal(dens),
        radius=radius,
        stroke=F,  opacity=op,
        fillOpacity=fop) 
    
    if(input$towers) proxy %>% showGroup('towerlocs')
    else proxy  %>% hideGroup('towerlocs')
    
})

  #})

  

#Either plot all points and use equal color
#or plot densities at coarser grain



########################
#traceplot

modout <- reactive({
  subset(mod, mod$id==input$track_id)
  
})
revi <- reactive({
  subset(birdraw, birdraw$id==input$track_id)
  
})


output$traceplot <- renderPlotly({
  oo=modout()
  melt.modout<-melt(oo, measure=c("lon", "lat"))
  
  #plot raw data on traceplot, add variable to make aes match
  
    rawbird=revi()
    rawbird$value<-rep(c(mean(rawbird$lon), mean(rawbird$lat)), length.out=nrow(rawbird))
    rawbird$variable<-rep(c("lon", "lat"), length.out=nrow(rawbird))
    

    i1=ggplot(data=melt.modout, aes(x=date, y=value))+geom_point(
      aes(col=b)
    )+
      scale_colour_gradient2(low="red",mid="white", high="blue", midpoint = 1.5)+
      geom_line()+
      facet_grid(variable~id, scales = "free_y")+
      geom_ribbon(data=subset(melt.modout, melt.modout$variable=="lon"), aes(ymin=lon.025, ymax=lon.975), alpha=0.4)+
      geom_ribbon(data=subset(melt.modout, melt.modout$variable=="lat"), aes(ymin=lat.025, ymax=lat.975), alpha=0.4)
      #+geom_rug(data=rawbird, aes(x=date,y=value), sides="b")

  
  ggplotly(i1)
})

output$event <- renderPrint({
  d <- event_data("plotly_hover")
  if (is.null(d)) "Hover on a point!" else d
})



#output$revitab <- renderDataTable({head(revi())})
#output$modtab <- renderDataTable({head(modout())})
  
})

