#Import libraries
library(shiny)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(dplyr)
library(lubridate)
#library(av)
library(leaflet)
library(RCurl)
library(htmltools)



# Import data
global <- read.csv("data/merged_final.csv")


country_list <- sort(unique(global$country))
#min_date<-  month(min(as.Date(global$date)))
#max_date<-  month(max(as.Date(global$date)))
min_date<-  toString(min(as.Date(global$date)))
max_date<-  toString(max(as.Date(global$date)))



#Fluid Page
ui <- fluidPage(
  theme=shinytheme('sandstone'),
  #titlePanel("COVID during 2020"),
  headerPanel(
    fluidRow(
      column(
        width=12,
        h2(tags$b("COVID during 2020")),
        style = "background-color:#aabbdd;",align="left"
        
      ),
      column(
        width=12,
        h4(tags$b("->by: Shree Bhattarai,  (Go fullscreen from a Desktop for best experience)")),
        style = "background-color:#aabbdd",align="left"
        
      )
    )
  ),
  #Add sidebar layout with sidebarpanel and mainpanel
  
  #Sidebar Layout

    #Adding SidebarPanel
    
      #Set tabs
      tabsetPanel(
        #Tab 1
        tabPanel(title=h3(tags$b("General Overview")),
                 sidebarLayout(
                   sidebarPanel(
                     tags$h2("World Data"),
                     hr(),
                     h4("Table Controls"),
                     sliderInput("max_count","Max # of countries",
                                 min=5,max=20,value=10,step=1),
                     radioButtons("sorting","Top countries based on", c("Cases"="Cases",
                                                                        "Deaths"="Deaths",
                                                                        "Deaths_per_Cases"="Deaths_per_Cases",
                                                                        "Popn_in_Millions"="Popn_in_Millions")),
                     hr(),
                    
                     h4("Time Series Plot Controls"),
                     selectInput("countries","Select a Country",country_list,selected="Japan"),
                     hr(),
                     h4(tags$b("Data Sources:")),
                     h6(tags$li("https://covidtracking.com/data/download")),
                     h6(tags$li("https://raw.githubusercontent.com/albertyw/avenews/master/old/data/average-latitude-longitude-countries.csv")),
                     h6(tags$li("https://ourworldindata.org/coronavirus-source-data")),
                     h6(tags$li("https://covid19.who.int/info/")),
                     ),
                   
                   
                   mainPanel(
                     fluidRow(
                       
                       column(width=6,h3(tags$b("Coronavirus")),
                         style = "background-color:#eeaa88;",align="center"),
                       
                       column(width=5,h3(tags$b(textOutput("title1_text"))),
                         style = "background-color:#eeaa88",align="center")
                     ),
                    
                     fluidRow(
                       
                       column(width=3,img(src='coronavirus.jpg',height=350,width=510)),
                       column(width=5,tableOutput("world_Count_total"),offset=2)
                       ),
                     
                     fluidRow(
                       column(width=11,h3(tags$b(textOutput("title2_text"))),
                              style = "background-color:#eeaa88",align="center")
                     ),
                     
                     fluidRow(
                       column(width=6,plotOutput("time_series2")),
                       column(width=5,plotOutput("time_series1"))
                     )
                     
                   )
                   
                   
                 ),
                 ),
        
        
        
        
        #Tab 2
        tabPanel(title=h3(tags$b("Time Evolution of COVID-19")),
                 mainPanel(
                   fluidRow(
                     
                     column(width=12,h3(tags$b("How COVID-19 spread over time?")),
                            style = "background-color:#eeaa88;",align="center")
                   ),
                   
                   fluidRow(
                     
                     column(width=12,h5(tags$b("(You may click at any point along the Timeline to view data for a given Date)")),
                            style = "background-color:#bbaa88;",align="center")
                   ),
                   fluidRow(
                     
                     column(width=12,
                                sliderInput("timeline", h3(tags$b("Timeline")),
                                            min=as.Date(min_date,"%Y-%m-%d"),
                                            max=as.Date(max_date,"%Y-%m-%d"),
                                            value=as.Date(min_date,"%Y-%m-%d"),
                                            timeFormat="%Y-%m-%d",
                                            animate = animationOptions(interval =1000,
                                                                       loop = FALSE,
                                                                       playButton=h3(tags$b("Click here to Play")),
                                                                       pauseButton=h3(tags$b("Click here to Pause"))),
                                            step = 15,ticks=TRUE,
                                            width="100%"))
                     ), #fluidRow
                   
                   fluidRow(
                     column(width=2,radioButtons("sorting_Time",h4(tags$b("Bar Plot based on daily number by")), c("Cases"="cases",
                                                                                       "Deaths"='deaths'),
                                                 inline=TRUE,
                     ),style = "background-color:#bbaa88;",align="center"
                     ),
                     column(width=10,plotOutput("hist_timeline"))                                                                 
                     
                   )
                   
                 )#mainPanel closes
                 
                 
                 ),#tabPanel closes
        
        tabPanel(title=h3(tags$b("COVID Map")),
                 mainPanel(
                   fluidRow(
                     
                     column(width=12,h3(tags$b("Map View of overall COVID-19 numbers during 2020")),
                            style = "background-color:#eeaa88;",align="center")
                   ),
                   
                   
                   fluidRow(
                     column(width=12,selectInput("country",h4(tags$b("Click on the white bar to select one or more countries. Hover on the marker to view their data. Click on the Marker to view name of the country.")),
                                                choices=country_list,
                                                multiple=TRUE,width=800
                                                
                     ),style = "background-color:#bbaa88;",align="center"
                     ),
                     column(width=10,leafletOutput("leafletmap", height = "600px", width = "1245px"))                                                                 
                     
                   )
                   
                 )
                 
          
        ),
      )
      
    )
    
  
  
  



# Server setup
server <- function(input,output) {
  
  
  output$world_Count_total <- renderTable({
      v <- global %>% group_by(country) %>%
      summarise(Cases=sum(new_cases),Deaths=sum(new_deaths),Popn_in_Millions=mean(population/1000000)) %>%
      mutate("Deaths_per_Cases"=Deaths/Cases) %>% arrange(desc(switch(input$sorting, Cases=Cases, 
                           Deaths=Deaths,
                           Deaths_per_Cases=Deaths_per_Cases,
                           Popn_in_Millions=Popn_in_Millions)))
      head(v,input$max_count)
  })
    
  output$time_series1 <- renderPlot({
    v2 <- global %>% filter(grepl(input$countries,country))
    ymax <- max(v2$cases)
    
    scaleRight <- max(v2$deaths)/ymax
    ggplot(v2)+geom_line(aes(x=as.Date(date),y=cases),size=1)+
    geom_line(aes(x=as.Date(date),y=deaths/scaleRight),size=1,color='red')+scale_x_date(date_breaks='1 months', date_labels='%B')+
      scale_y_continuous(name="Total Cases", sec.axis = sec_axis(~.*scaleRight,name="Total Deaths"))+theme_bw()+
      theme(axis.text.x=element_text(angle=90,vjust=0.5, size=14),
            axis.text = element_text(color='black',size=14),
            axis.title=element_text(size=14,face='bold'),
            axis.title.x=element_blank(),
            plot.title=element_text(size=16,face='bold'),
            axis.title.y = element_text(color='black',size=14),
            axis.title.y.right = element_text(color ='red', size=14))
                                       })
  
  
  output$time_series2 <- renderPlot({
    v2 <- global %>% filter(grepl(input$countries,country))
    ymax <- max(v2$new_cases)
    
    scaleRight <- max(v2$new_deaths)/ymax
    ggplot(v2)+geom_line(aes(x=as.Date(date),y=new_cases),size=0.7)+
      geom_line(aes(x=as.Date(date),y=new_deaths/scaleRight),size=0.7,color='red')+scale_x_date(date_breaks='1 months', date_labels='%B')+
      scale_y_continuous(name="Daily New Cases", sec.axis = sec_axis(~.*scaleRight,name="Daily New Deaths"))+ theme_bw()+
      theme(axis.text.x=element_text(angle=90,vjust=0.5, size=14),
            axis.text = element_text(color='black',size=14),
            axis.title=element_text(size=14,face='bold'),
            axis.title.x=element_blank(),
            plot.title=element_text(size=16,face='bold'),
            axis.title.y = element_text(color='black',size=14),
            axis.title.y.right = element_text(color ='red', size=14))
  })
  
  output$title1_text <- renderText({
    paste("Countries sorted by highest number of",input$sorting,sep=" ")
  })
  
  
  output$title2_text <- renderText({
    paste("Daily and Total Cases and Deaths in",input$countries,sep=" ")
  })
  
  

## Contents for Animated Views
  # Bar plot by country 
  output$hist_timeline <- renderPlot({
  v2 <- global %>% mutate(per_mil_popn = !!as.symbol(input$sorting_Time)/population*1000000) %>%
    filter(date==input$timeline)%>% top_n(10,per_mil_popn)
  
  ggplot(head(v2,10), aes(reorder(country,per_mil_popn),per_mil_popn,country,fill=country))+
    geom_bar(stat='identity')+coord_flip()+theme_bw()+ 
    ggtitle(paste('Date: ',input$timeline, "(Daily change displayed in biweekly interval)",sep=' '))+
    theme(legend.position="none",
          axis.title.y=element_blank(),
          axis.text = element_text(color='black',size=20),
          axis.title = element_text(color='black',size=20),
          plot.title=element_text(color='black',size=20))+
    labs(y=paste(toupper(input$sorting_Time),"per Million Population", " "))+
    geom_text(aes(label=ceiling(per_mil_popn)), size=10,hjust=1)
  })
  
  # Map
  df_groups <- reactive({
    
   global %>% filter(country %in% input$country)
  })
  
  output$leafletmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.NatGeoWorldMap") %>%
      fitBounds(min(global$Long), min(global$Lat),
                max(global$Long), max(global$Lat))
  })
  
  observe({
    df <- df_groups()
    labels <- df$deaths
    labels2 <- df$cases
    if(nrow(df) == 0){
      leafletProxy("leafletmap", data = df) %>%
        clearMarkers()
    }
    else{
      leafletProxy("leafletmap",data=df) %>%
        clearMarkers() %>%
        addProviderTiles("Esri.NatGeoWorldMap") %>%
        addMarkers(lng =~Long, lat = ~Lat,
                   popup = ~country,
                   label = paste("Total Deaths:",labels,";", "Total Cases:", labels2
                                 ),
                   labelOptions = labelOptions(noHide = F, offset=c(0,-12)))
    }
  })
  
  
  
  
  }


#Shiny App 
shinyApp(ui=ui, server=server)

