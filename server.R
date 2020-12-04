#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(viridis)

# Define server logic 
shinyServer(function(input, output) {
  
  set.seed(122)
  firstpage <- rnorm(500) #This is ensure to display the dialog box only once
  
  
  
  ################Tab-1###########################
  #This function will create the pop-up dialog box
  observeEvent(once = TRUE, ignoreNULL = FALSE, ignoreInit = FALSE, eventExpr = firstpage, { 
    showModal(modalDialog(
      easyClose = T,
      title=HTML('<br>'),
      size = 'l',
      tags$head(tags$style('body,  { font-family: "Open Sans"; background-color: black;}')),
      HTML('<h1><strong><center>Which is the cost-effective neighbourhood of NewYork for AirBnB ?</center></strong></h1>')
    ))
  })
  
  ################Tab-2###########################
  mapdf <- reactive({
    updated_listing=listing %>%
      filter(  room_type %in% input$select_room & 
                 price >= input$slider_price[1] &
                 price <= input$slider_price[2] &
                 number_of_reviews >= input$slider_review[1] &
                 number_of_reviews <= input$slider_review[2] &
                 review_scores_location  >= input$slider_rating[1] &
                 review_scores_location <= input$slider_rating[2]) 
    NeighReviews <- updated_listing %>% group_by(neighbourhood_cleansed) %>% summarise(avg_loc_review = round(mean(review_scores_location, na.rm = TRUE),1))
    colnames(NeighReviews) <- c("neighbourhood","avg_review")
    
    NeighPriveAvg <- updated_listing %>% group_by(neighbourhood_cleansed) %>% summarise(avg_price = round(mean(price, na.rm = TRUE),0))
    colnames(NeighPriveAvg) <- c("neighbourhood","avg_price")
    temp <- merge(NeighPriveAvg, NeighReviews, by="neighbourhood")
    
    airbnb_neighbourhoods <- sp::merge(neighGeoJson ,temp, by="neighbourhood",all.=FALSE)
    airbnb_neighbourhoods
    
  })
  
  #This leaflet is draw only once
  output$mymap <- renderLeaflet({
      pal <- colorBin(review.palatte,domain = airbnb_neigh$avg_review, bin=review.bins)
      leaflet(airbnb_neigh) %>%
        setView(lng = -74.00, lat = 40.71, zoom = 10.5)%>%
        addProviderTiles("CartoDB.Positron")%>%
        addPolygons(stroke = FALSE, 
                    smoothFactor = 0.3, 
                    fillOpacity = .9 ,fillColor = ~pal(avg_review),
                    label = ~paste(neighbourhood,',',avg_review))%>%
        addLegend(pal = pal, values = ~avg_review, opacity = 1.0, title = "Avg Location Review",layerId = "foo")
      
  })
  
  # observe an event to update the leaflet 
  observe({ #require a trigger to call the observe function
    if(input$MapType=='Avg Price')
    {
      #leaflet map for avg price
      #pal <- colorBin(price.palatte, bin=price.bins)
      pal <- colorBin(price.palatte,  domain = mapdf()$avg_price, pretty = FALSE,bin=price.bins)
      proxy <- leafletProxy("mymap",data = mapdf()) %>%
        clearShapes()%>%
        clearControls()%>%
        addPolygons(stroke = FALSE, 
                    smoothFactor = 0.3, 
                    fillOpacity = .8 ,fillColor = ~pal(avg_price),
                    label = ~paste(neighbourhood,',',avg_price))%>%
        addLegend(pal = pal, values =~avg_price, opacity = 1.0, title = "Avg Price",layerId = "foo")
    }
    else
    {
      #leaflet map for avg review
      pal <- colorBin(review.palatte,domain = mapdf()$avg_review, bin=review.bins)
      proxy <- leafletProxy("mymap",data = mapdf()) %>%
        clearShapes()%>%
        clearControls%>%
        addPolygons(stroke = FALSE, 
                    smoothFactor = 0.3, 
                    fillOpacity = .9 ,fillColor = ~pal(avg_review),
                    label = ~paste(neighbourhood,',',avg_review))%>%
        addLegend(pal = pal, values = ~avg_review, opacity = 1.0, title = "Avg Location Review",layerId = "foo")
      
      
    }
  })
  
  #Change the text below the map based on user selection
  output$ui <- renderUI({
    if(input$MapType=='Avg Review')
    {
      HTML('<p style="font-size: 16px;color: #000000;font-weight: 700;font-style: italic;font-variant: normal;">
                       <strong> Manhattan and nearby area has the high location review compare to other boroughs
                       mostly as it hosts iconic attractions like the Empire State Building, Statue of 
                      Liberty and Central Park. Check out how avg price varies or apply filter to explore more.</strong></left></p>')
    }
    else{
      HTML('<p style="font-size: 16px;color: #000000;font-weight: 700;font-style: italic;font-variant: normal;">
                       <strong> Manhattan is the most expensive borough which is quite reasonable since 
                       it’s the tourist hub. Brooklyn is also expensive sinces its also a popular 
                      hub for multiple attraction.Check out how avg location rating varies or apply filter to explore more</strong></left></p>')
    }
  })
  
  ################Tab-3###########################
  #Update the data for the bubble chart 
  price_data <- reactive({
    listing %>% 
      # mutate(n=1) %>% 
      filter(room_type==input$roomtypetab3,neighbourhood_group_cleansed==input$boroughtab3) %>% 
      group_by(neighbourhood_cleansed,neighbourhood_group_cleansed) %>% 
      summarise(
        avg_price=round(mean(price,na.rm = TRUE),0),
        avg_reviews=round(mean(review_scores_location,na.rm = TRUE),0),
        num_listings=n()) %>%
      filter(avg_price<=input$pricerangetab3[2] & avg_price>=input$pricerangetab3[1]) %>%
      # prepare text for tooltip
      mutate(text = 
               paste(neighbourhood_cleansed,", ", neighbourhood_group_cleansed, 
                     "\nAverage Price: ", avg_price, 
                     "\nAverage  Reviews: ", avg_reviews, 
                     "\nTotal Listings: ", num_listings, sep=""))%>%  
      arrange(desc(avg_price))
  })
  
  #Bubble plot, best neighbour tab
  output$leBubblePlot <- renderPlotly({
    p<-ggplot(price_data()[1:input$numoptionstab3,], aes(x=avg_price, y=avg_reviews, size = num_listings, color = neighbourhood_cleansed,text=text)) +
      geom_point(alpha=0.7) +
      scale_size(range = c(5, 15)) +
      scale_color_viridis(discrete=TRUE, guide=FALSE) +
      theme_bw() +
      xlab("Average Price") + 
      ylab("Average Review") +
      ylim(NA, 10)+
      ggtitle("Most budgeted Neighborhoods") +
      theme(legend.title=element_blank(),legend.position="topright",plot.title = element_text(hjust = 0.5))
    # turn ggplot interactive with plotly
    ggplotly(p,tooltip="text")
  })
  
  
  ################Tab-4###########################
  # Data for left selected neighbourhood in the compare tab
  comparedata1 <- reactive({
    listing %>% 
      filter(neighbourhood_cleansed==input$SelectNeighborhood1) 
  })
  
  # Data for right selected neighbourhood in the compare tab
  comparedata2<- reactive({
    listing %>% 
      filter(neighbourhood_cleansed==input$SelectNeighborhood2) 
  })
  
  #1st plot on compare tab
  output$comparePlot1 <- renderPlot({
    ggplot(
      comparedata1() %>% mutate(n=1) %>% 
        group_by(room_type) %>% 
        summarise(count=sum(n)) %>% mutate(percent=round(count/sum(count)*100,0.0))
      , aes(x=reorder(room_type,desc(count)),y=count)) + 
      geom_bar(stat="identity", fill = 'light blue') +  
      geom_text(aes(label=paste0(percent, "%\n(", count, ")"))) +
      theme_bw() +
      ylab("Number of listing") +
      xlab("") +
      ggtitle("Frequency of listing Type")
  })
  
  
  #2nt plot on compare tab
  output$comparePlot2 <- renderPlot({
    ggplot(
      comparedata2() %>% mutate(n=1) %>% 
        group_by(room_type) %>% 
        summarise(count=sum(n)) %>% mutate(percent=round(count/sum(count)*100,0.0))
      , aes(x=reorder(room_type,desc(count)),y=count)) + 
      geom_bar(stat="identity", fill = 'light blue') +  
      geom_text(aes(label=paste0(percent, "%\n(", count, ")"))) +
      theme_bw() +
      ylab("Number of listing") +
      xlab("") +
      ggtitle("Frequency of listing Type")
  })
  
  #3rd plot on compare tab
  output$comparePlot3 <- renderPlot({
    ggplot(
      comparedata1(), aes(x=room_type,y=price,fill=room_type)) +
      geom_violin() +
      theme_bw() +
      ylab("Price") +
      xlab("") +
      ylim(NA, 500)+
      ggtitle("Price/night for different listing Type")
  })
  
  
  #4th plot on the compare tab
  output$comparePlot4 <- renderPlot({
    ggplot(
      comparedata2(), aes(x=room_type,y=price,fill=room_type)) +
      geom_violin() +
      theme_bw() +
      ylab("Price") +
      xlab("") +
      ylim(NA, 500)+
      ggtitle("Price/night for different listing Type")
  })
  
  
  
  
})
