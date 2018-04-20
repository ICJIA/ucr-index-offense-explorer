# Author: Bobae Kang (Bobae.Kang@illinois.gov)
# Created: 2017-10-31
# Last revised: 2018-04-20
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
library(shinyjs)
library(DT)
library(rgdal)
library(leaflet)
library(highcharter)
library(tidyr)
library(dplyr)


# import data
load("data/data.rda")


# define a function to use
apply_rate <- function(count, population) {
  round(count / population * 100000, 2)
}


# DEFINE SERVER LOGIC #
#---------------------------------------------------------------------------
server <- function (input, output, session) {
  
  # toggle sidebar
  observeEvent(input$toggleSidebar, {
    toggleCssClass("main", "col-sm-9")
    js$toggleSidebar()
  })


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

    updateSliderInput(
      session,
      inputId = "range",
      value = c(min(as.integer(mydata$year)), max(as.integer(mydata$year)))
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
  observeEvent(input$region, {

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
  
  observeEvent(input$type, {
    
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
    data_download <- mydata %>%
      filter(year %in% seq(input$range[1], input$range[2]))
    
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
    
    data_output <- data_output %>%
      filter(year == input$range[2])
    
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
        span(icon("bar-chart"), style="color:yellow;"),
        paste0("Offenses in ", input$range[2]),
        style="font-size:1.1em;"
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
    
    data_max <- data_output[data_output$year == input$range[2], ]
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
        paste0("Crime rate in ", input$range[2]),
        style="font-size:1.1em;"
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
    
    data_max <- data_output[data_output$year == input$range[2], ]
    data_pre <- data_output[data_output$year == input$range[2] - 1, ]
    
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
        paste0("Change, ", input$range[2] - 1, "-", substr(input$range[2], 3, 4)),
        style="font-size:1.1em;"
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
    
    data_max <- data_output[data_output$year == input$range[2], ]
    data_min <- data_output[data_output$year == input$range[1], ]
    
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
        paste0("Change, ", input$range[1], "-", substr(input$range[2], 3, 4)),
        style="font-size:1.1em;"
      )
    )
  })
  

  # historical trend line chart---------------------------------------------
  output$line_title <- renderText({
    
    text_output <- "Historical Trend"
    
    if(input$crimeCat != "All"){
      text_output <- paste(text_output, input$crimeCat, sep=": ")
    } 
    text_output
  })
  
  output$line <- renderHighchart({
    
    data_output <- data_output %>%
      filter(year %in% seq(input$range[1], input$range[2]))
    
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
        Violent = ifelse(
          input$format == "Count",
          sum(violent, na.rm = TRUE),
          apply_rate(sum(violent, na.rm = TRUE), sum(population))
        ), 
        Property = ifelse(
          input$format == "Count",
          sum(property, na.rm = TRUE),
          apply_rate(sum(property, na.rm = TRUE), sum(population))
        )
      ) %>%
      gather(
        Violent, Property,
        key = "crimeCat",
        value = "count"
      )
    
    if (input$crimeCat != "All") {
      plot <- filter(data_plot, crimeCat == input$crimeCat) %>%
        hchart("line", hcaes(x = year, y = count, group = crimeCat, name = crimeCat))
    } else {
      plot <- data_plot %>%
        hchart("line", hcaes(x = year, y = count, group = crimeCat, name = crimeCat))
    }
    
    if (input$format == "Count") {
      plot %>%
        hc_yAxis(min = 0) %>%
        hc_add_theme(hc_theme_sandsignika())  
    } else {
      plot %>%
        hc_yAxis(title = list(text = "rate (per 100k)"), min = 0) %>%
        hc_add_theme(hc_theme_sandsignika())  
    }
    
  })

  
  # Offense type pie chart----------------------------------------------
  output$pie_title <- renderText({
    
    text_output <- paste0("Offense Type (", input$range[2],")")
    
    if(input$crimeCat != "All"){
      text_output <- paste(text_output, input$crimeCat, sep = ": ")
    } 
    text_output
  })
  
  
  output$pie <- renderHighchart({
    
    data_pie <- mydata
    
    if (input$region != "All") {
      data_pie <- mydata[mydata$region == input$region,]
    }
    if (input$type != "All") {
      data_pie <- mydata[mydata$type == input$type,]
    }
    if (input$county != "All") {
      data_pie <- mydata[mydata$county == input$county,]
    }
    
    data_plot <- data_pie %>%
      filter(year == input$range[2]) %>%
      summarise(
        Murder = ifelse(
          input$format == "Count",
          sum(murder, na.rm = TRUE),
          apply_rate(sum(murder, na.rm = TRUE), sum(population))
        ),
        Rape = ifelse(
          input$format == "Count",
          sum(rape, na.rm = TRUE),
          apply_rate(sum(rape, na.rm = TRUE), sum(population))
        ),
        Robbery = ifelse(
          input$format == "Count",
          sum(robbery, na.rm = TRUE),
          apply_rate(sum(robbery, na.rm = TRUE), sum(population))
        ),
        Assault = ifelse(
          input$format == "Count",
          sum(assault, na.rm = TRUE),
          apply_rate(sum(assault, na.rm = TRUE), sum(population))
        ),
        Burglary = ifelse(
          input$format == "Count",
          sum(burglary, na.rm = TRUE),
          apply_rate(sum(burglary, na.rm = TRUE), sum(population))
        ),
        LarcenyTft = ifelse(
          input$format == "Count",
          sum(larcenytft, na.rm = TRUE),
          apply_rate(sum(larcenytft, na.rm = TRUE), sum(population))
        ),
        MVTft = ifelse(
          input$format == "Count",
          sum(mvtft, na.rm = TRUE),
          apply_rate(sum(mvtft, na.rm = TRUE), sum(population))
        ),
        Arson = ifelse(
          input$format == "Count",
          sum(arson, na.rm = TRUE),
          apply_rate(sum(arson, na.rm = TRUE), sum(population))
        )
      ) %>%
      gather(Murder:Arson, key="crimeCat", value="count")
    
    if (input$crimeCat == "Violent") {
      plot <- data_plot %>%
        filter(crimeCat %in% c("Murder", "Rape", "Robbery", "Assult")) %>%
        hchart("pie", hcaes(x = crimeCat, y = count), name = ifelse(input$format == "Count", "Count", "Rate"))
    } else if (input$crimeCat == "Property") {
      plot <- data_plot %>%
        filter(crimeCat %in% c("Burglary", "LarcenyTft", "MVTft", "Arson")) %>%
        hchart("pie", hcaes(x = crimeCat, y = count), name = ifelse(input$format == "Count", "Count", "Rate"))
    } else {
      plot <- data_plot %>%
        hchart("pie", hcaes(x = crimeCat, y = count), name = ifelse(input$format == "Count", "Count", "Rate"))
    }
    
    plot %>%
      hc_add_theme(hc_theme_sandsignika())
  })

  
  # Distribution map-------------------------------------------------------
  output$map_title <- renderText({
    
    text_output <- paste0("Distribution (", input$range[2],")")
    
    if(input$crimeCat != "All"){
      text_output <- paste(text_output, input$crimeCat, sep=": ")
    } 
    text_output
  })
  
  output$map <- renderLeaflet({
    
    map_selected <- mymap
    data_output <- data_output %>%
      filter(year == input$range[2])

    if (input$crimeCat == "Property") {
      if (input$format == "Count") {
        my_attr <- data_output %>%
          mutate(data = property)
      } else {
        my_attr <- data_output %>%
          mutate(data = apply_rate(property, population))
      }
        my_attr <- select(my_attr, name = county, data)
    } else if (input$crimeCat == "Violent") {
      if (input$format == "Count") {
        my_attr <- data_output %>%
          mutate(data = violent)
      } else {
        my_attr <- data_output %>%
          mutate(data = apply_rate(violent, population))
      }
      my_attr <- select(my_attr, name = county, data)
    } else {
      my_attr <- data_output %>%
        group_by(name = county) %>%
        summarise(
          data = ifelse(
            input$format == "Count",
            sum(violent, property),
            apply_rate(sum(violent, property), sum(population))
          )
        )
    }
      
    map_selected@data <- map_selected@data %>%
      left_join(my_attr, by = "name")
    map_selected2 <- map_selected

    if (input$region != "All") {
      if (input$region == "Cook") {
        map_selected2 <- map_selected2[map_selected2$name == "Cook", ]
      } else {
        map_selected2 <- map_selected2[map_selected2$region == input$region, ]
      }
    }
    if (input$type != "All") {
      if (input$type == "Cook") {
        map_selected2 <- map_selected2[map_selected2$name == "Cook", ]
      } else {
        map_selected2 <- map_selected2[map_selected2$type == input$type, ]
      }
    }
    if (input$county != "All") {
      map_selected2 <- map_selected2[map_selected2$name == input$county, ]
    }
    
    fill_color <- colorQuantile("YlOrRd", map_selected$data)

    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -89.3, lat = 39.8, zoom = 6) %>%
      addPolygons(
        data = mymap,
        weight = 1.5,
        color = "darkgrey",
        fillOpacity = 0.2,
        fillColor = "lightgrey"
      ) %>%
      addPolygons(
        data = map_selected2,
        weight = 1.5,
        color = "darkgrey",
        fillOpacity = 0.8,
        fillColor = ~fill_color(data),
        label = ~paste0(
          as.character(name),
          ifelse(input$format == "Count", " (count)", " (rate)"),
          ": ",
          round(data, 2)
        ),
        labelOptions = labelOptions(
          offset = c(0, -45),
          direction = "top",
          textsize = "12px",
          style = list("background" = "white")
        ),
        highlightOptions = highlightOptions(
          weight = 4,
          color = "black",
          bringToFront = FALSE
        )
      )
  })

  # data table--------------------------------------------------------------
  output$dataTable <- renderDataTable({
    
    data_output <- data_output %>%
      filter(year %in% seq(input$range[1], input$range[2]))

    if(input$region != "All"){
      data_output <- data_output[data_output$region == input$region, ]
    }
    if(input$type != "All"){
      data_output <- data_output[data_output$type == input$type, ]
    }
    if(input$county != "All"){
      data_output <- data_output[data_output$county == input$county, ]
    }
    
    if (input$format == "Count") {
      data_output  
    } else {
      data_output %>%
        mutate(
          violent = apply_rate(violent, population),
          property = apply_rate(property, population)
        )
    }
  }, rownames = FALSE)
  
}