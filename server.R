# Author: Bobae Kang (Bobae.Kang@illinois.gov)
# Created: 10/31/2017
# Last revised: 11/7/2017
# Script title: server.R
#---------------------------------------------------------------------------
# Script description:
# The current script is to define server logic for the Shiny application
# for ISP data.
#---------------------------------------------------------------------------
# Table of contents:
# * PREPARE FOR THE SESSION
# * DEFINE SERVER LOGIC
#---------------------------------------------------------------------------


# PREPARE FOR THE SESSIONS #
#---------------------------------------------------------------------------
options(shiny.sanitize.errors = FALSE)


# import packages
library(shiny)
library(DT)
library(rgdal)
library(leaflet)
library(highcharter)
library(dplyr)
library(tidyr)


# import data
load("data/mydata.rda")
load("data/mymap.rda")


# DEFINE SERVER LOGIC #
#---------------------------------------------------------------------------
server <- function (input, output, session) {


  # data output-------------------------------------------------------------
  data_output <- mydata %>%
    select(1:4, violent = violentCrime, property = propertyCrime, population)
  
  
  # reset filtering
  observeEvent(input$reset,{
    
    updateRadioButtons(
      session,
      inputId = 'crimeCat',
      selected = 'All'
    )
    
    updateSelectInput(
      session,
      inputId = 'region',
      choices = c("All", sort(unique(as.character(data_output$region)))),
      selected = 'All'
    )
    
    updateSelectInput(
      session,
      inputId = 'countyType',
      choices = c("All", sort(unique(as.character(data_output$countyType)))),
      selected = 'All'
    )
    
    updateSelectInput(
      session,
      inputId = 'county',
      choices = c("All", sort(unique(as.character(data_output$county)))),
      selected = 'All'
    )
  })
  
  
  # filtering data----------------------------------------------------------
  observeEvent(input$region,{

    if (input$region == 'All') {
      updateSelectInput(
        session,
        inputId = 'countyType',
        choices = c("All", sort(unique(as.character(data_output$region)))),
        selected = 'All'
      )
      
      updateSelectInput(
        session,
        inputId = 'countyType',
        choices = c("All", sort(unique(as.character(data_output$countyType)))),
        selected = 'All'
       )
      
      updateSelectInput(
        session,
        inputId = 'county',
        choices = c("All", sort(unique(as.character(data_output$county)))),
        selected = 'All'
      )
    } else {
      data_update <- mydata %>%
        filter(region == input$region)
      
      updateSelectInput(
        session,
        inputId = 'countyType',
        label = 'Select county type',
        choices = c("All", sort(unique(as.character(data_update$countyType))))
      )
      
      updateSelectInput(
        session,
        inputId = 'county',
        label = 'Select county',
        choices = c("All", sort(unique(as.character(data_update$county))))
      )
    }
  })
  
  observeEvent(input$countyType,{
    
    if (input$countyType == 'All') {
      updateSelectInput(
        session,
        inputId = 'county',
        selected = 'All'
      )
    } else {
      data_update <- mydata %>%
        filter(countyType == input$countyType)
      
      updateSelectInput(
        session,
        inputId = 'county',
        label = 'Select county',
        choices = c("All", sort(unique(as.character(data_update$county))))
      )
      
      if (input$region!='All') {
        data_update <- data_update %>%
          filter(region==input$region)
        
        updateSelectInput(
          session,
          inputId='county',
          label='Select county',
          choices=c("All", sort(unique(as.character(data_update$county))))
        )
      }
    }
  })
  
 
  # data to download--------------------------------------------------------
  dataDownload <- reactive({
    data_download <- mydata
    if (input$region != "All") {
      data_download <- filter(data_download, region == input$region)
    }
    if (input$countyType != "All") {
      data_download <- filter(data_download, countyType == input$countyType)
    }
    if (input$county != "All") {
      data_download <- filter(data_download, county == input$county)
    }
    na.omit(data_download)
  })
  

  # download data-----------------------------------------------------------
  output$downloadData <- downloadHandler(
    filename = function () {
      paste("data-filtered-", Sys.Date(), ".csv", sep="")
    },
    content = function (file) {
      write.csv(dataDownload(), file, row.names=FALSE)
    }
  )

    
  # Text on the top---------------------------------------------------------
  output$current <- renderText({
    
    text_output <- 'Illinois'
    
    if(input$region != "All"){
      text_output <- paste(input$region, 'region')
      if(input$countyType != "All"){
        text_output <- paste(text_output, input$countyType, sep=', ')
      }
      if(input$region == "Cook"){
        text_output = paste(input$region, 'county')
      }
    } else if(input$countyType != "All"){
      text_output <- input$countyType
      if(input$county != "All") {
        text_output <- input$county
      }
    }
    
    if(input$county != "All"){
      text_output <- paste(input$county, 'county')
    }
    
    paste('Crime Data Profile:', text_output, sep=' ')
  })
  
  
  # Value Boxes ------------------------------------------------------------
  output$kpi_box_1 <- renderValueBox({
    
    data_output <- data_output %>% filter(year==2015)
    
    if(input$region != "All"){
      data_output <- data_output[data_output$region == input$region,]
    }
    if(input$countyType != "All"){
      data_output <- data_output[data_output$countyType == input$countyType,]
    }
    if(input$county != "All"){
      data_output <- data_output[data_output$county == input$county,]
    }

    value <- sum(data_output$violent, data_output$property, na.rm=TRUE)
    
    if(input$crimeCat == 'Violent'){
      value <- value - sum(data_output$property, na.rm=TRUE)
    } else if(input$crimeCat == 'Property'){
      value <- value - sum(data_output$violent, na.rm=TRUE)
    } 
    
    if(value > 10000){
      value <- paste(round(value/1000), 'K', sep='')
    }
    
    valueBox(
      value=value,
      subtitle='Offenses in 2015',
      icon=icon('exclamation-triangle'),
      color='maroon')
  })
  
  output$kpi_box_2 <- renderValueBox({
    
    if(input$region != "All"){
      data_output <- data_output[data_output$region == input$region,]
    }
    if(input$countyType != "All"){
      data_output <- data_output[data_output$countyType == input$countyType,]
    }
    if(input$county != "All"){
      data_output <- data_output[data_output$county == input$county,]
    }
    
    data_2015 <- data_output[data_output$year==2015,]
    crime_2015 <- sum(data_2015$violent, data_2015$property,
                      na.rm=TRUE)
    pop_2015 <- sum(data_2015$population, na.rm=TRUE)
    
    if(input$crimeCat == 'Violent'){
      crime_2015 <- sum(data_2015$violent, na.rm=TRUE)
    } else if(input$crimeCat == 'Property'){
      crime_2015 <- sum(data_2015$property, na.rm=TRUE)
    }
    
    value = crime_2015/pop_2015*100000
    
    valueBox(
      value=paste(round(value, 2), sep=''),
      subtitle='Crime rate in 2015 (per 100K)',
      icon=icon('bar-chart'),
      color='orange')
  })
  
  output$kpi_box_3 <- renderValueBox({
    
    if(input$region != "All"){
      data_output <- data_output[data_output$region == input$region,]
    }
    if(input$countyType != "All"){
      data_output <- data_output[data_output$countyType == input$countyType,]
    }
    if(input$county != "All"){
      data_output <- data_output[data_output$county == input$county,]
    }
    
    data_2015 <- data_output[data_output$year==2015,]
    data_2014 <- data_output[data_output$year==2014,]
    
    value_2015 <- sum(data_2015$violent, data_2015$property,
                      na.rm=TRUE)
    
    value_2014 <- sum(data_2014$violent, data_2014$property,
                      na.rm=TRUE)
    
    
    if(input$crimeCat == 'Violent'){
      value_2015 <- sum(data_2015$violent, na.rm=TRUE)
      value_2014 <- sum(data_2014$violent, na.rm=TRUE)
    } else if(input$crimeCat == 'Property'){
      value_2015 <- sum(data_2015$property, na.rm=TRUE)
      value_2014 <- sum(data_2014$property, na.rm=TRUE)
    }
  
    value <- round((value_2015-value_2014)/value_2014*100,2)
      
    valueBox(
      value=value,
      subtitle='Percent Change, 2014-15',
      icon=icon('percent'),
      color='aqua')
  })
  
  output$kpi_box_4 <- renderValueBox({
    
    if(input$region != "All"){
      data_output <- data_output[data_output$region == input$region,]
    }
    if(input$countyType != "All"){
      data_output <- data_output[data_output$countyType == input$countyType,]
    }
    if(input$county != "All"){
      data_output <- data_output[data_output$county == input$county,]
    }
    
    data_2015 <- data_output[data_output$year==2015,]
    data_2011 <- data_output[data_output$year==2011,]
    
    value_2015 <- sum(data_2015$violent, data_2015$property,
                      na.rm=TRUE)
    
    value_2011 <- sum(data_2011$violent, data_2011$property,
                      na.rm=TRUE)
    
    
    if(input$crimeCat == 'Violent'){
      value_2015 <- sum(data_2015$violent, na.rm=TRUE)
      value_2011 <- sum(data_2011$violent, na.rm=TRUE)
    } else if(input$crimeCat == 'Property'){
      value_2015 <- sum(data_2015$property, na.rm=TRUE)
      value_2011 <- sum(data_2011$property, na.rm=TRUE)
    }
    
    value <- round((value_2015-value_2011)/value_2011*100,2)
    
    valueBox(
      value=value,
      subtitle='Percent Change, 2011-15',
      icon=icon('percent'),
      color='blue')
  })
  

  # historical trend line chart---------------------------------------------
  output$lineChart_title <- renderText({
    
    text_output <- 'Historical Trend'
    
    if(input$crimeCat != "All"){
      text_output <- paste(text_output, input$crimeCat, sep=': ')
    } 
    text_output
  })
  
  output$lineChart <- renderHighchart({
    
    if(input$region != "All"){
      data_output <- data_output[data_output$region == input$region,]
    }
    if(input$countyType != "All"){
      data_output <- data_output[data_output$countyType == input$countyType,]
    }
    if(input$county != "All"){
      data_output <- data_output[data_output$county == input$county,]
    }
    
    data_plot <- data_output %>%
      group_by(year) %>%
      summarise(Violent = sum(violent, na.rm=TRUE),
                Property = sum(property, na.rm=TRUE)) %>%
      gather(Violent, Property, key='crimeCat', value='count')
    
    if(input$crimeCat != "All"){
      plot <- data_plot %>%
        filter(crimeCat == input$crimeCat) %>%
        hchart("line", hcaes(x=year, y=count))
    } else {
      plot <- data_plot %>%
        hchart("line", hcaes(x=year, y=count, group=crimeCat))
    }
    
    plot %>%
      hc_yAxis(min=0) %>%
      hc_add_theme(hc_theme_sandsignika())
  })

  
  # crime categories pie chart----------------------------------------------
  output$pieChart_title <- renderText({
    
    text_output <- 'Crime Categories (2015)'
    
    if(input$crimeCat != "All"){
      text_output <- paste(text_output, input$crimeCat, sep=': ')
    } 
    text_output
  })
  
  
  output$pieChart <- renderHighchart({
    
    if(input$region != "All"){
      data_output <- data_output[data_output$region == input$region,]
    }
    if(input$countyType != "All"){
      data_output <- data_output[data_output$countyType == input$countyType,]
    }
    if(input$county != "All"){
      data_output <- data_output[data_output$county == input$county,]
    }
    
    data_plot <- mydata %>%
      filter(year==2015) %>%
      summarise(Murder     = sum(murder, na.rm=TRUE),
                Rape       = sum(rape, na.rm=TRUE),
                Robbery    = sum(robbery, na.rm=TRUE,na.rm=TRUE),
                Assult     = sum(aggAssult,na.rm=TRUE),
                Burglary   = sum(burglary,na.rm=TRUE),
                LarcenyTft = sum(larcenyTft, na.rm=TRUE),
                MVTft      = sum(MVTft, na.rm=TRUE),
                Arson      = sum(arson, na.rm=TRUE)) %>%
      gather(Murder:Arson, key='crimeCat', value='count')
    
    if(input$crimeCat == "Violent"){
      plot <- data_plot %>%
        filter(crimeCat %in% c('Murder', 'Rape', 'Robbery', 'Assult')) %>%
        hchart("pie", hcaes(x=crimeCat, y=count))
    } else if(input$crimeCat == "Property"){
      plot <- data_plot %>%
        filter(crimeCat %in% c('Burglary', 'LarcenyTft', 'MVTft', 'Arson')) %>%
        hchart("pie", hcaes(x=crimeCat, y=count), name='count')
    } else {
      plot <- data_plot %>%
        hchart("pie", hcaes(x=crimeCat, y=count), name='count')
    }
    
    plot %>%
      hc_add_theme(hc_theme_sandsignika())
  })

  
  # selected area map-------------------------------------------------------
  output$map <- renderLeaflet({

    map <- leaflet() %>%
      setView(lng=-89.3,lat=39.8,zoom=6)

    map_selected <- mymap

    if(input$region != "All"){
      if(input$region == 'Cook'){
        map_selected <- map_selected[map_selected$CNTYNAM_LO == 'Cook',]
      } else {
        map_selected <- map_selected[map_selected$REGION == input$region,]
      }
    }
    if(input$countyType != "All"){
      if(input$countyType == 'Cook'){
        map_selected <- map_selected[map_selected$CNTYNAM_LO == 'Cook',]
      } else {
        map_selected <- map_selected[map_selected$COUNTYTYPE == input$countyType,]
      }
    }
    if(input$county != "All"){
      map_selected <- map_selected[map_selected$CNTYNAM_LO == input$county,]
    }

    map <- map %>%
      addPolygons(data=mymap,
                  weight=0.6,
                  color='black',
                  fillOpacity=0.2,
                  fillColor='grey',
                  label=~as.character(CNTYNAM_LO)) %>%
      addPolygons(data=map_selected,
                  weight=0.6,
                  color='black',
                  fillOpacity=0.8,
                  fillColor='#466c8c',
                  label=~as.character(CNTYNAM_LO))
  })

  # output$map <- renderPlot({
  # 
  #   map_selected <- mymap
  # 
  #   if(input$region != "All"){
  #     if(input$region == 'Cook'){
  #       map_selected <- map_selected[map_selected$CNTYNAM_LO == 'Cook',]
  #     } else {
  #       map_selected <- map_selected[map_selected$REGION == input$region,]
  #     }
  #   }
  #   if(input$countyType != "All"){
  #     if(input$countyType == 'Cook'){
  #       map_selected <- map_selected[map_selected$CNTYNAM_LO == 'Cook',]
  #     } else {
  #       map_selected <- map_selected[map_selected$COUNTYTYPE == input$countyType,]
  #     }
  #   }
  #   if(input$county != "All"){
  #     map_selected <- map_selected[map_selected$CNTYNAM_LO == input$county,]
  #   }
  #   
  #   qtm(shp=mymap,
  #       title='',
  #       bg.color='lightgrey',
  #       borders='grey',
  #       fill='lightgrey',
  #       fill.alpha=.3,
  #       frame=FALSE) + 
  #     qtm(shp=map_selected,
  #         borders='grey',
  #         fill='steelblue',
  #         fill.alpha=.8,
  #         frame=FALSE)
  #   
  # })

  # data table--------------------------------------------------------------
  output$dataTable <- renderDataTable({

    if(input$region != "All"){
      data_output <- data_output[data_output$region == input$region,]
    }
    if(input$countyType != "All"){
      data_output <- data_output[data_output$countyType == input$countyType,]
    }
    if(input$county != "All"){
      data_output <- data_output[data_output$county == input$county,]
    }
    
    data_output[!is.na(data_output$county),]
  },
  rownames=FALSE
  )
  
}