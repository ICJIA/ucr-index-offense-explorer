# Author: Bobae Kang (Bobae.Kang@illinois.gov)
# Created: 2017-10-31
# Last revised: 2018-04-16
# Script title: server.R
#---------------------------------------------------------------------------
# Script description:
# The current script is to define server logic for the Shiny application
# for ISP data.
# (develop) v2.0: Without shinydashboard
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
library(tidyr)
library(dplyr)


# import data
load("data/data.rda")
# load("data/mydata.rda")
# load("data/mymap.rda")


# DEFINE SERVER LOGIC #
#---------------------------------------------------------------------------
server <- function (input, output, session) {


  # data output-------------------------------------------------------------
  data_output <- mydata %>%
    select(1:5, violent = violent_crime, property = property_crime)
  
  
  # reset filtering
  observeEvent(input$reset,{
    
    updateRadioButtons(
      session,
      inputId  = "crimeCat",
      selected = "All"
    )
    
    updateSelectInput(
      session,
      inputId  = "region",
      choices  = c("All", sort(unique(as.character(data_output$region)))),
      selected = "All"
    )
    
    updateSelectInput(
      session,
      inputId  = "type",
      choices  = c("All", sort(unique(as.character(data_output$type)))),
      selected = "All"
    )
    
    updateSelectInput(
      session,
      inputId  = "county",
      choices  = c("All", sort(unique(as.character(data_output$county)))),
      selected = "All"
    )
  })
  
  
  # filtering data----------------------------------------------------------
  observeEvent(input$region,{

    if (input$region == "All") {
      updateSelectInput(
        session,
        inputId  = "region",
        choices  = c("All", sort(unique(as.character(data_output$region)))),
        selected = "All"
      )
      
      updateSelectInput(
        session,
        inputId  = "type",
        choices  = c("All", sort(unique(as.character(data_output$type)))),
        selected = "All"
       )
      
      updateSelectInput(
        session,
        inputId  = "county",
        choices  = c("All", sort(unique(as.character(data_output$county)))),
        selected = "All"
      )
    } else {
      data_update <- mydata %>%
        filter(region == input$region)
      
      updateSelectInput(
        session,
        inputId = "type",
        label   = "Select county type",
        choices = c("All", sort(unique(as.character(data_update$type))))
      )
      
      updateSelectInput(
        session,
        inputId = "county",
        label   = "Select county",
        choices = c("All", sort(unique(as.character(data_update$county))))
      )
    }
  })
  
  observeEvent(input$type,{
    
    if (input$type == "All") {
      updateSelectInput(
        session,
        inputId  = "county",
        selected = "All"
      )
    } else {
      data_update <- mydata %>%
        filter(type == input$type)
      
      updateSelectInput(
        session,
        inputId = "county",
        label   = "Select county",
        choices = c("All", sort(unique(as.character(data_update$county))))
      )
      
      if (input$region != "All") {
        data_update <- data_update %>%
          filter(region == input$region)
        
        updateSelectInput(
          session,
          inputId = "county",
          label   = "Select county",
          choices = c("All", sort(unique(as.character(data_update$county))))
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
    if (input$type != "All") {
      data_download <- filter(data_download, type == input$type)
    }
    if (input$county != "All") {
      data_download <- filter(data_download, county == input$county)
    }
    na.omit(data_download)
  })
  

  # download data-----------------------------------------------------------
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-filtered-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dataDownload(), file, row.names = FALSE)
    }
  )

    
  # Text on the top---------------------------------------------------------
  output$current <- renderText({
    
    text_output <- "Illinois"
    
    if (input$region != "All") {
      text_output <- paste(input$region, "region")
      if (input$type != "All") {
        text_output <- paste(text_output, input$type, sep = ", ")
      }
      if (input$region == "Cook") {
        text_output = paste(input$region, "county")
      }
    } else if (input$type != "All") {
      text_output <- input$type
      if (input$county != "All") {
        text_output <- input$county
      }
    }

    if(input$county != "All"){
      text_output <- paste(input$county, "county")
    }
    
    paste("Crime Data Profile:", text_output, sep=" ")
  })
  
  
  # Value Boxes ------------------------------------------------------------
  output$kpi_box_1 <- renderUI({
    
    data_output <- data_output %>% filter(year == max(data_output$year))
    
    if (input$region != "All") {
      data_output <- data_output[data_output$region == input$region, ]
    }
    if (input$type != "All") {
      data_output <- data_output[data_output$type == input$type, ]
    }
    if (input$county != "All") {
      data_output <- data_output[data_output$county == input$county, ]
    }

    value <- sum(data_output$violent, data_output$property, na.rm = TRUE)
    
    if (input$crimeCat == "Violent") {
      value <- value - sum(data_output$property, na.rm = TRUE)
    } else if (input$crimeCat == "Property") {
      value <- value - sum(data_output$violent, na.rm = TRUE)
    } 
    
    if (value > 10000) {
      value <- paste(round(value / 1000), "K", sep = "")
    }
    
    tagList(
      tags$h1(value),
      tags$p(
        span(icon("exclamation-triangle"), style="color:red;"),
        paste0("Offenses in ", max(data_output$year))
      )
    )
  })
  
  output$kpi_box_2 <- renderUI({
    
    if (input$region != "All") {
      data_output <- data_output[data_output$region == input$region, ]
    }
    if (input$type != "All") {
      data_output <- data_output[data_output$type == input$type, ]
    }
    if (input$county != "All") {
      data_output <- data_output[data_output$county == input$county, ]
    }
    
    data_max <- data_output[data_output$year == max(data_output$year), ]
    crime_max <- sum(data_max$violent, data_max$property, na.rm = TRUE)
    pop_max <- sum(data_max$population, na.rm = TRUE)
    
    if (input$crimeCat == "Violent") {
      crime_max <- sum(data_max$violent, na.rm = TRUE)
    } else if (input$crimeCat == "Property") {
      crime_max <- sum(data_max$property, na.rm = TRUE)
    }
    
    value <- crime_max / pop_max * 100000
    
    tagList(
      tags$h1(round(value, 2)),
      tags$p(
        span(icon("bar-chart"), style="color:yellow;"),
        paste0("Crime rate in ", max(data_output$year), " (per 100K)")
      )
    )
  })
  
  output$kpi_box_3 <- renderUI({
    
    if (input$region != "All") {
      data_output <- data_output[data_output$region == input$region, ]
    }
    if (input$type != "All") {
      data_output <- data_output[data_output$type == input$type, ]
    }
    if (input$county != "All") {
      data_output <- data_output[data_output$county == input$county, ]
    }
    
    data_max <- data_output[data_output$year == max(data_output$year), ]
    data_pre <- data_output[data_output$year == max(data_output$year) - 1, ]
    
    value_max <- sum(data_max$violent, data_max$property, na.rm = TRUE)
    value_pre <- sum(data_pre$violent, data_pre$property, na.rm = TRUE)
    
    
    if (input$crimeCat == "Violent") {
      value_max <- sum(data_max$violent, na.rm = TRUE)
      value_pre <- sum(data_pre$violent, na.rm = TRUE)
    } else if (input$crimeCat == "Property") {
      value_max <- sum(data_max$property, na.rm = TRUE)
      value_pre <- sum(data_pre$property, na.rm = TRUE)
    }
  
    value <- round((value_max - value_pre) / value_pre * 100, 2)
      
    tagList(
      tags$h1(value),
      tags$p(
        span(icon("percent"), style="color:#337ab7;"),
        paste0("Percent Change, ", max(data_output$year) - 1, "-", substr(max(data_output$year), 3, 4))
      )
    )
  })
  
  output$kpi_box_4 <- renderUI({
    
    if (input$region != "All") {
      data_output <- data_output[data_output$region == input$region, ]
    }
    if (input$type != "All") {
      data_output <- data_output[data_output$type == input$type, ]
    }
    if (input$county != "All") {
      data_output <- data_output[data_output$county == input$county, ]
    }
    
    data_max <- data_output[data_output$year == max(data_output$year), ]
    data_min <- data_output[data_output$year == min(data_output$year), ]
    
    value_max <- sum(data_max$violent, data_max$property, na.rm = TRUE)
    value_min <- sum(data_min$violent, data_min$property, na.rm = TRUE)
    
    
    if(input$crimeCat == "Violent"){
      value_max <- sum(data_max$violent, na.rm = TRUE)
      value_min <- sum(data_min$violent, na.rm = TRUE)
    } else if(input$crimeCat == "Property"){
      value_max <- sum(data_max$property, na.rm = TRUE)
      value_min <- sum(data_min$property, na.rm = TRUE)
    }
    
    value <- round((value_max - value_min) / value_min * 100, 2)
    
    tagList(
      tags$h1(value),
      tags$p(
        span(icon("percent"), style="color:#337ab7;"),
        paste0("Percent Change, ", min(data_output$year), "-", substr(max(data_output$year), 3, 4))
      )
    )
  })
  

  # historical trend line chart---------------------------------------------
  output$lineChart_title <- renderText({
    
    text_output <- "Historical Trend"
    
    if(input$crimeCat != "All"){
      text_output <- paste(text_output, input$crimeCat, sep=": ")
    } 
    text_output
  })
  
  output$lineChart <- renderHighchart({
    
    if(input$region != "All"){
      data_output <- data_output[data_output$region == input$region,]
    }
    if(input$type != "All"){
      data_output <- data_output[data_output$type == input$type,]
    }
    if(input$county != "All"){
      data_output <- data_output[data_output$county == input$county,]
    }
    
    data_plot <- data_output %>%
      group_by(year) %>%
      summarise(
        Violent = sum(violent, na.rm = TRUE),
        Property = sum(property, na.rm = TRUE)
      ) %>%
      gather(Violent, Property, key = "crimeCat", value = "count")
    
    if (input$crimeCat != "All") {
      plot <- hchart(filter(data_plot, crimeCat == input$crimeCat), "line", hcaes(x = year, y = count))
    } else {
      plot <- hchart(data_plot, "line", hcaes(x = year, y = count, group = crimeCat))
    }
    
    plot %>%
      hc_yAxis(min = 0) %>%
      hc_add_theme(hc_theme_sandsignika())
  })

  
  # crime categories pie chart----------------------------------------------
  output$pieChart_title <- renderText({
    
    text_output <- "Crime Categories (2015)"
    
    if(input$crimeCat != "All"){
      text_output <- paste(text_output, input$crimeCat, sep = ": ")
    } 
    text_output
  })
  
  
  output$pieChart <- renderHighchart({
    
    if (input$region != "All") {
      data_output <- data_output[data_output$region == input$region,]
    }
    if (input$type != "All") {
      data_output <- data_output[data_output$type == input$type,]
    }
    if (input$county != "All") {
      data_output <- data_output[data_output$county == input$county,]
    }
    
    data_plot <- mydata %>%
      filter(year == 2015) %>%
      summarise(
        Murder     = sum(murder, na.rm = TRUE),
        Rape       = sum(rape, na.rm = TRUE),
        Robbery    = sum(robbery, na.rm = TRUE),
        Assult     = sum(assault, na.rm = TRUE),
        Burglary   = sum(burglary, na.rm = TRUE),
        LarcenyTft = sum(larcenytft, na.rm = TRUE),
        MVTft      = sum(mvtft, na.rm = TRUE),
        Arson      = sum(arson, na.rm = TRUE)
      ) %>%
      gather(Murder:Arson, key="crimeCat", value="count")
    
    if (input$crimeCat == "Violent") {
      plot <- data_plot %>%
        filter(crimeCat %in% c("Murder", "Rape", "Robbery", "Assult")) %>%
        hchart("pie", hcaes(x=crimeCat, y=count))
    } else if (input$crimeCat == "Property") {
      plot <- data_plot %>%
        filter(crimeCat %in% c("Burglary", "LarcenyTft", "MVTft", "Arson")) %>%
        hchart("pie", hcaes(x=crimeCat, y=count), name="count")
    } else {
      plot <- data_plot %>%
        hchart("pie", hcaes(x=crimeCat, y=count), name="count")
    }
    
    plot %>%
      hc_add_theme(hc_theme_sandsignika())
  })

  
  # selected area map-------------------------------------------------------
  output$map <- renderLeaflet({

    if (input$crimeCat == "Property") {
      my_attr <- data_output %>%
        group_by(name = county) %>%
        summarise(data = mean(property, na.rm = TRUE))
    } else if (input$crimeCat == "Violent") {
      my_attr <- data_output %>%
        group_by(name = county) %>%
        summarise(data = mean(violent, na.rm = TRUE))
    } else {
      my_attr <- data_output %>%
        group_by(name = county) %>%
        summarise(data = mean(sum(violent, property, na.rm = TRUE), na.rm = TRUE))
    }
      
    map_selected <- mymap
    map_selected@data <- map_selected@data %>%
      left_join(my_attr)
    
    map_selected2 <- map_selected

    if (input$region != "All") {
      if (input$region == "Cook") {
        map_selected2 <- map_selected[map_selected$name == "Cook", ]
      } else {
        map_selected2 <- map_selected[map_selected$region == input$region, ]
      }
    }
    if (input$type != "All") {
      if (input$type == "Cook") {
        map_selected2 <- map_selected[map_selected$name == "Cook", ]
      } else {
        map_selected2 <- map_selected[map_selected$type == input$type, ]
      }
    }
    if (input$county != "All") {
      map_selected2 <- map_selected[map_selected$name == input$county, ]
    }
    
    fill_color <- colorQuantile("YlOrRd", map_selected$data)
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -89.3, lat = 39.8, zoom = 6) %>%
      addPolygons(
        data = mymap,
        weight = 0.6,
        color = "black",
        fillOpacity = 0.2,
        fillColor = "lightgrey"
      ) %>%
      addPolygons(
        data = map_selected2,
        weight = 0.6,
        color = "black",
        fillOpacity = 0.8,
        fillColor = ~fill_color(data),
        label = ~paste0(as.character(name), ": ", round(data, 2)),
        labelOptions = labelOptions(
          offset = c(0, -40),
          direction = "top",
          textsize = "12px",
          style = list(
            "background" = "white"
            )
        ),
        highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE)
      )
      # addMarkers(label = ~as.character(name))
  })

  # data table--------------------------------------------------------------
  output$dataTable <- renderDataTable({

    if(input$region != "All"){
      data_output <- data_output[data_output$region == input$region, ]
    }
    if(input$type != "All"){
      data_output <- data_output[data_output$type == input$type, ]
    }
    if(input$county != "All"){
      data_output <- data_output[data_output$county == input$county, ]
    }
    
    data_output[!is.na(data_output$county), ]
  },
  rownames = FALSE
  )
  
}