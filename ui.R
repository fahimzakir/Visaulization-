#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Include all the libraries
library(shiny)
library(shinythemes)
library(leaflet)
require(shinyjs)
library(plotly)
library(shinydashboard)


# Define UI for application 
shinyUI(
        navbarPage(title = "", selected ="Home", 
        # Application title
        theme = shinytheme("flatly"), #theme for the shiny 
        useShinyjs(), 
        includeScript(path = "validation.js"), #script to add validation in the 2nd tab 
        
        #Below function will implement all the UI for the first Tab of the app
        tabPanel("Home",
                 #Below function will create the CSS for the Dialog box
                 tags$head(tags$style(HTML('
                        .modal.in .modal-dialog {
                        filter: alpha(opacity=10); opacity: 0.8;border-color: 
                        height:150%; width:100%; position:absolute;
                        padding:5px;top:30%;left:0%;
                                }
                                                    
                        .modal-content {
                        filter: alpha(opacity=10); 
                        opacity: 0.9;
                        background-color: black;
                        height:150%;width:100%;
                                }
                        .modal-body {
                        filter: alpha(opacity=10); opacity: 1;
                        float: none; margin: 0;
                        height:150%; width:100%; font-size: 42px;
                        color: #fff; font-weight: 300;
                        text-shadow: none;font-opacity: 1;
                                        }
                                                    
                                        '))),
                 
                 br(),
                 
                 #This panel for add the introduction  
                 absolutePanel( 
                     fixed = TRUE,draggable = F, 
                      top = 70, left = 480, 
                     right = "auto", bottom = "auto",
                     width = 900, height = "auto",
                     style = "opacity: .7 ; z-index: 1000",
                     HTML('<h3><strong><left>Are you ready to travel?</left></strong></h3>'),
                     HTML('<h2><strong><left style="font-size:0.8em";>This app uses Airbnb dataset provided by InsideAirbnb.com to help New York city travellers to decide which neighbourhood they can choose based on the exploration I have done</strong> </left></h2>'),
                     br(),
                     HTML('<p id="maptext"; style="line-height:1.6em; font-size:1.1em; text-align:left"><strong> Below are some guide to use this app. </strong>
                          <ul>
                        <li>Map Tab: Here you will find out how neighbourhoods compare in terms of there avg price and avg review. </li>
                        <li>Best Neighbourhood Tab: Here find the most cost-effective neighbourhood.</li>
                        <li>Compare Tab: Compare any two neighbourhood side by side.</li>
                        <ul>
                          </left></p>')
                        ),
                 
                 #This panel is used to add the static information of the dataset
                 absolutePanel( 
                     fixed = TRUE, draggable = F,top = 400, left = 480, right = "auto", bottom = "auto",
                     width = 1000, height = "auto",style = "opacity: 1 ; z-index: 1000",
                     fluidRow(valueBox("Number of listing", 50378, icon = icon("hotel",class = NULL, lib = "font-awesome")),
                              valueBox("Total Income", 8284030, icon = icon("dollar-sign",class = NULL, lib = "font-awesome")),
                              valueBox("Number of Hosts", 37758, icon = icon("user",class = NULL, lib = "font-awesome"))
                              )
                 ),
                 
                 #This panel is show the Data Source 
                 absolutePanel( 
                     fixed = TRUE, draggable = F,top = 600, left = 480, right = "auto", bottom = "auto",
                     width = 1000, height = "auto",style = "opacity: .7 ; z-index: 1000",
                     HTML('<p style="font-size:1em"><strong>Data sources:</strong></p>'),
                     HTML('<p style="line-height:1.7em">http://insideairbnb.com</p>')
                 ),
                 
                 #This panel is to show the reference
                 absolutePanel( 
                     fixed = TRUE,draggable = F, top = 750, left = 480, 
                     right = "auto", bottom = 20,width = 1000, height = "auto",
                     style = "opacity: .7 ; z-index: 1000",
                     HTML('<p style="font-size:1em; text-align:left"><right><strong>References:</strong></p>'),
                     HTML('<p style="text-align:left">https://www.timeout.com/newyork/attractions/top-attractions-in-manhattan</p>'),
                     HTML('<p style="text-align:left">https://en.wikipedia.org/wiki/Boroughs_of_New_York_City</p>')
                 ),
                 
                 # This is leftmost panel on the first tab which gives the title of the app
                 absolutePanel( fixed = TRUE,draggable = F, top = 70, left = 50, 
                     right = "auto",  bottom = "auto",width = 400, height = "auto",
                     style = "opacity: .8 ; z-index: 1000",
                     HTML('<p style="font-size:5em; line-height: 1.2em"><strong>New York<br>AirBnB<br>Data Analayisis</strong></p>'),
                     HTML('<div style="width:340px;height:10px;background-color:black;opacity:0.8"></div>')
                 )
        ),
        
        #Below Tab contains UI for the 2nd map tab
        tabPanel("Map",
                
                 #leaflet outout
                leafletOutput(outputId = "mymap", width = "100%", height =800),
                #leafletOutput("mymap"),
                
                #Panel to control the inputs to the maps
                absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE, draggable = F, 
                              top = 200, left = 20, right = "auto" , bottom = 40,
                              width = 250, height = "auto",
                              
                              radioButtons("MapType","Please select", choices = c("Avg Review","Avg Price")),
                              
                              checkboxGroupInput(inputId = "select_room", label = h4("Room Type"), 
                                                 choices = room_type, selected = room_type),
                              sliderInput(inputId = "slider_price", label = h4("Listing Price"), min = 30, max = 1000, step = 50,
                                          pre = "$", sep = ",", value = c(30, 1000)),
                              sliderInput(inputId = "slider_rating", label = h4("Avg_Rating"), min = 7, max = 10, step = 0.2,
                                          value = c(7, 10)),
                              sliderInput(inputId = "slider_review", label = h4("Number of Reviews"), min = 0, max = 400, step = 25,
                                          value = c(10, 350))
                            ),
                
                #This function implements the bottom panel of the map page
                absolutePanel( 
                    fixed = TRUE,draggable = T, top = "auto", left = "auto", right = 40, bottom = 20,
                    width = 800, height = 100,style = "opacity: .7 ; z-index: 1000",
                    HTML('<h3><strong><left style="color: #000000;">Map insight</left></strong></h3>'),
                    uiOutput("ui") # this section dynamically added based on user selection
                )),
        
            # Below tab has the UI for the 3rd tab
            tabPanel("Best Neighbourhood",
                    #plots
                    fluidRow(
                        column(4,"Please select your preference using the option below.",
                               HTML('<br></br>'),
                selectizeInput(inputId ="boroughtab3",label = "Borough",choices = unique (listing$neighbourhood_group_cleansed)),
                selectizeInput(inputId ="roomtypetab3",label = "Room Type",choices = unique (listing$room_type)),
                sliderInput("numoptionstab3","Number of Options:",min = 2,max = 10,value = 5),
                sliderInput("pricerangetab3","Avg Price Range:",min = 30,max = 500,value = c(75,300))
                
                   
            ),
            column(8,plotlyOutput("leBubblePlot"))
            
            
            ),
                HTML('<br></br>'),
                HTML('<br></br>'),
                "Note: Top 5 (or as specified) number of budgeted neighborhoods
                          displays those with the lowest avg price that
                          satisfy the user input criteria. The size of bubble reflect the number of listing in that neighbourhood. ",
                        HTML('
                         <br></br>
                         <br></br> 
                       ')),
        
        #Below panel has UI for fourth tab of the app
        tabPanel('Compare',
                fluidRow(
                    HTML('<p style="line-height:1.6em; font-size:1.1em; text-align:left"><strong> Lets compare any two neighbourhoods </strong></left></p>'),
                    column(6,
                           selectizeInput(inputId ="SelectNeighborhood1",
                                          label = "Neighborhood",
                                          choices = unique (listing$neighbourhood_cleansed)),
                           plotOutput("comparePlot1"),
                           plotOutput("comparePlot3")
                    ),
                    column(6,
                           
                           selectizeInput(inputId ="SelectNeighborhood2",
                                          label = "Neighborhood",
                                          choices = unique (listing$neighbourhood_cleansed)),
                           plotOutput("comparePlot2"),
                           plotOutput("comparePlot4")
                    )
                )
                
        )
    

    )
)
