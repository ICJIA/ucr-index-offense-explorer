# Author: Bobae Kang (Bobae.Kang@illinois.gov)
# Created: 2017-10-31
# Last revised: 2018-04-20
# Script title: server.R
#-------------------------------------------------------------------------------


# PREPARE FOR THE SESSION #
#-------------------------------------------------------------------------------
options(shiny.sanitize.errors = FALSE)


# import packages
library(shiny)
library(shinyjs)
library(DT)
library(leaflet)
library(highcharter)
library(dplyr)
library(tidyr)
library(rgdal)


# import data
load("data/data.rda")


# define a function to use
apply_rate <- function(count, population) {
  round(count / population * 100000, 2)
}


# highcharter option
hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
options(highcharter.lang = hcoptslang)


# rural labels
rural_labels <- c("Completely Rural", "Mostly Rural", "Mostly Urban", "Completely Urban")


# DEFINE SERVER LOGIC #
#-------------------------------------------------------------------------------
server <- function (input, output, session) {
  # toggle sidebar
  observeEvent(input$toggleSidebar, {
    toggleCssClass("main", "col-sm-9")
    js$toggleSidebar()
  })


  # data output-----------------------------------------------------------------
  data_output <- mydata %>%
    select(year:population, violent = violent_crime, property = property_crime)
  
  # reset filtering
  observeEvent(input$reset, {
    updateRadioButtons(
      session,
      inputId  = "format",
      selected = "Count"
    )
    
    updateRadioButtons(
      session,
      inputId  = "category",
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
      inputId  = "rural",
      choices  = c("All", rural_labels),
      selected = "All"
    )
    
    updateSelectInput(
      session,
      inputId  = "county",
      choices  = c("All", sort(unique(as.character(data_output$county)))),
      selected = "All"
    )
  })
  
  
  # filtering data--------------------------------------------------------------
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
        inputId  = "rural",
        choices  = c("All", intersect(rural_labels, data_output$rural)),
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
        inputId = "rural",
        choices = c("All", intersect(rural_labels, data_update$rural))
      )
      
      updateSelectInput(
        session,
        inputId = "county",
        choices = c("All", sort(unique(as.character(data_update$county))))
      )
    }
  })
  
  observeEvent(input$rural, {
    
    if (input$rural == "All") {
      updateSelectInput(
        session,
        inputId  = "county",
        selected = "All"
      )
    } else {
      data_update <- mydata %>%
        filter(rural == input$rural)
      
      updateSelectInput(
        session,
        inputId = "county",
        choices = c("All", sort(unique(as.character(data_update$county))))
      )
      
      if (input$region != "All") {
        data_update <- data_update %>%
          filter(region == input$region)
        
        updateSelectInput(
          session,
          inputId = "county",
          choices = c("All", sort(unique(as.character(data_update$county))))
        )
      }
    }
  })
  
 
  # download data---------------------------------------------------------------
  dataDownload <- reactive({
    data_download <- mydata %>%
      filter(year %in% seq(input$range[1], input$range[2]))
    
    if (input$region != "All") {
      data_download <- filter(data_download, region == input$region)
    }
    if (input$rural != "All") {
      data_download <- filter(data_download, type == input$rural)
    }
    if (input$county != "All") {
      data_download <- filter(data_download, county == input$county)
    }
    na.omit(data_download)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-filtered-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dataDownload(), file, row.names = FALSE)
    }
  )

    
  # Text on the top-------------------------------------------------------------
  output$current <- renderText({
    text_output <- "Illinois State"
    
    if (input$region != "All") {
      text_output <- paste(input$region, "Region")
      if (input$rural != "All") {
        text_output <- paste(text_output, input$rural, sep = ", ")
      }
      if (input$region == "Cook") {
        text_output = paste(input$region, "County")
      }
    } else if (input$rural != "All") {
      text_output <- input$rural
      if (input$county != "All") {
        text_output <- paste(input$county, "County")
      }
    }

    if(input$county != "All"){
      text_output <- paste(input$county, "County")
    }
    
    paste("UCR Profile:", text_output, sep=" ")
  })
  
  
  # KPIs -----------------------------------------------------------------------
  output$kpi_box_1 <- renderUI({
    data_output <- data_output %>%
      filter(year == input$range[2])
    
    if (input$region != "All") {
      data_output <- data_output[data_output$region == input$region, ]
    }
    if (input$rural != "All") {
      data_output <- data_output[data_output$rural == input$rural, ]
    }
    if (input$county != "All") {
      data_output <- data_output[data_output$county == input$county, ]
    }
    
    value <- sum(data_output$violent, data_output$property, na.rm = TRUE)
    
    if (input$category == "Violent") {
      value <- value - sum(data_output$property, na.rm = TRUE)
    } else if (input$category == "Property") {
      value <- value - sum(data_output$violent, na.rm = TRUE)
    }
    
    if(input$format == "Count") {
      desc <- paste0("Offenses in ", input$range[2])
      if (value > 10000) {
        value <- paste0(round(value / 1000), "K")
      }
    } else {
      desc <- paste0("Crime rate in ", input$range[2])
      value <- apply_rate(value, sum(data_output$population, na.rm = TRUE))
    }
    
    tagList(
      tags$h1(value),
      tags$p(
        icon("bar-chart"),
        desc,
        style="font-size:1.1em;"
      )
    )
  })
  
  output$kpi_box_2 <- renderUI({
    if (input$region != "All") {
      data_output <- data_output[data_output$region == input$region, ]
    }
    if (input$rural != "All") {
      data_output <- data_output[data_output$rural == input$rural, ]
    }
    if (input$county != "All") {
      data_output <- data_output[data_output$county == input$county, ]
    }

    if (input$format != "Count") {
      data_output <- data_output %>%
        mutate(
          violent = apply_rate(violent, population),
          property =apply_rate(property, population)
        )
    }
        
    data_max <- data_output[data_output$year == input$range[2], ]
    data_pre <- data_output[data_output$year == input$range[2] - 1, ]

    value_max <- sum(data_max$violent, data_max$property, na.rm = TRUE)
    value_pre <- sum(data_pre$violent, data_pre$property, na.rm = TRUE)

    
    if (input$category == "Violent") {
      value_max <- sum(data_max$violent, na.rm = TRUE)
      value_pre <- sum(data_pre$violent, na.rm = TRUE)
    } else if (input$category == "Property") {
      value_max <- sum(data_max$property, na.rm = TRUE)
      value_pre <- sum(data_pre$property, na.rm = TRUE)
    }
  
    value <- paste0(round((value_max - value_pre) / value_pre * 100, 1), "%")
      
    tagList(
      tags$h1(value),
      tags$p(
        icon("sort"),
        paste0("Change, ", input$range[2] - 1, "-", input$range[2]),
        style="font-size:1.1em;"
      )
    )
  })
  
  output$kpi_box_3 <- renderUI({
    if (input$region != "All") {
      data_output <- data_output[data_output$region == input$region, ]
    }
    if (input$rural != "All") {
      data_output <- data_output[data_output$rural == input$rural, ]
    }
    if (input$county != "All") {
      data_output <- data_output[data_output$county == input$county, ]
    }
    
    if (input$format != "Count") {
      data_output <- data_output %>%
        mutate(
          violent = apply_rate(violent, population),
          property =apply_rate(property, population)
        )
    }
    
    data_max <- data_output[data_output$year == input$range[2], ]
    data_min <- data_output[data_output$year == input$range[1], ]
    
    value_max <- sum(data_max$violent, data_max$property, na.rm = TRUE)
    value_min <- sum(data_min$violent, data_min$property, na.rm = TRUE)
    
    
    if(input$category == "Violent"){
      value_max <- sum(data_max$violent, na.rm = TRUE)
      value_min <- sum(data_min$violent, na.rm = TRUE)
    } else if(input$category == "Property"){
      value_max <- sum(data_max$property, na.rm = TRUE)
      value_min <- sum(data_min$property, na.rm = TRUE)
    }
    
    value <- paste0(round((value_max - value_min) / value_min * 100, 1), "%")
    
    tagList(
      tags$h1(value),
      tags$p(
        icon("sort"),
        paste0("Change, ", input$range[1], "-", input$range[2]),
        style="font-size:1.1em;"
      )
    )
  })
  

  # historical trend line chart-------------------------------------------------
  output$line_title <- renderText({
    text_output <- paste0("Trend (", input$range[1], "-", input$range[2], ")")
    
    if (input$category != "All"){
      paste(text_output, input$category, sep=": ")
    } else {
      text_output
    }
  })
  
  output$line <- renderHighchart({
    data_line <- data_output %>%
      filter(year %in% seq(input$range[1], input$range[2]))
    
    if(input$region != "All"){
      data_line <- data_line[data_line$region == input$region, ]
    }
    if (input$rural != "All") {
      data_line <- data_line[data_line$rural == input$rural,]
    }
    if(input$county != "All"){
      data_line <- data_line[data_line$county == input$county, ]
    }
    
    data_plot <- data_line %>%
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
      )
    
    plot <- highchart() %>% 
      hc_xAxis(categories = data_plot$year)
    
    if (input$category == "All") {
      plot <- plot %>% 
        hc_add_series(
          name = "Violent",
          data = data_plot$Violent,
          color = "#f45b5b"
        ) %>% 
        hc_add_series(
          name = "Property",
          data = data_plot$Property,
          color = "#8085e9"
        )
    } else {
      hc_color <- ifelse(input$category == "Violent", "#f45b5b", "#8085e9")
      plot <- plot %>%
        hc_add_series(
          name = input$category,
          data = data_plot[[input$category]],
          color = hc_color
        ) %>%
        hc_legend(enabled = FALSE)
    }
    
    plot %>%
      hc_xAxis(title = "") %>%
      hc_yAxis(
        title = list(text = ifelse(input$format == "Count", "count", "rate (per 100k)")),
        min = 0
      ) %>%
      hc_add_theme(hc_theme_sandsignika())
  })

  
  # Offense type bar chart------------------------------------------------------
  output$bar_title <- renderText({
    text_output <- paste0("Offense Type (", input$range[2], ")", "*")
    
    if(input$category != "All"){
      paste(text_output, input$category, sep = ": ")
    } else {  
      text_output
    }
  })
  
  output$bar <- renderHighchart({
    data_bar <- mydata
    
    if (input$region != "All") {
      data_bar <- data_bar[data_bar$region == input$region,]
    }
    if (input$rural != "All") {
      data_bar <- data_bar[data_bar$rural == input$rural,]
    }
    if (input$county != "All") {
      data_bar <- data_bar[data_bar$county == input$county,]
    }
    
    data_plot <- data_bar %>%
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
        `Aggravated assault` = ifelse(
          input$format == "Count",
          sum(assault, na.rm = TRUE),
          apply_rate(sum(assault, na.rm = TRUE), sum(population))
        ),
        Burglary = ifelse(
          input$format == "Count",
          sum(burglary, na.rm = TRUE),
          apply_rate(sum(burglary, na.rm = TRUE), sum(population))
        ),
        `Larceny theft` = ifelse(
          input$format == "Count",
          sum(larcenytft, na.rm = TRUE),
          apply_rate(sum(larcenytft, na.rm = TRUE), sum(population))
        ),
        `Moter vehical theft` = ifelse(
          input$format == "Count",
          sum(mvtft, na.rm = TRUE),
          apply_rate(sum(mvtft, na.rm = TRUE), sum(population))
        ),
        Arson = ifelse(
          input$format == "Count",
          sum(arson, na.rm = TRUE),
          apply_rate(sum(arson, na.rm = TRUE), sum(population))
        )
      )
    
    if (input$category == "All") {
      data_plot2 <-
        rbind(
          data_plot %>%
            select(Murder:`Aggravated assault`) %>%
            gather(Murder:`Aggravated assault`, key = "category", value = "count") %>%
            mutate(type = "Violent"),
          data_plot %>%
            select(Burglary:Arson) %>%
            gather(Burglary:Arson, key = "category", value = "count") %>%
            mutate(type = "Property")
        )
      
      plot <-
        hchart(
          data_plot2,
          type = "column",
          mapping = hcaes(x = category, y = count, group = factor(type, levels = c("Violent", "Property")))
        )
    } else {
      hc_color <- ifelse(input$category == "Violent", "#f45b5b", "#8085e9")
      
      if (input$category == "Violent") {
        data_plot2 <-
          data_plot %>%
          select(Murder:`Aggravated assault`) %>%
          gather(Murder:`Aggravated assault`, key = "category", value ="count") %>%
          mutate(type = "Violent")  
      } else {
        data_plot2 <-
          data_plot %>%
          select(Burglary:Arson) %>%
          gather(Burglary:Arson, key = "category", value = "count") %>%
          mutate(type = "Property")
      }
      
      plot <-
        hchart(
          data_plot2,
          type = "column",
          mapping = hcaes(x = category, y = count),
          name = input$category,
          color = hc_color
        )
    }
    
    plot %>%
      hc_xAxis(title = "") %>%
      hc_yAxis(
        title = list(text = ifelse(input$format == "Count", "count", "rate (per 100k)")),
        type = "logarithmic"
      ) %>%
      hc_add_theme(hc_theme_sandsignika())
  })

  
  # Distribution map------------------------------------------------------------
  output$map_title <- renderText({
    text_output <- paste0("Geography (", input$range[2], ")", "*")
    
    if(input$category != "All"){
      paste(text_output, input$category, sep=": ")
    } else {
      text_output
    }
  })
  
  output$map <- renderLeaflet({
    map_selected <- mymap
    data_map <- data_output %>%
      filter(year == input$range[2])

    if (input$category == "Property") {
      if (input$format == "Count") {
        my_attr <- data_map %>%
          mutate(data = property)
      } else {
        my_attr <- data_map %>%
          mutate(data = apply_rate(property, population))
      }
        my_attr <- select(my_attr, name = county, data)
    } else if (input$category == "Violent") {
      if (input$format == "Count") {
        my_attr <- data_map %>%
          mutate(data = violent)
      } else {
        my_attr <- data_map %>%
          mutate(data = apply_rate(violent, population))
      }
      my_attr <- select(my_attr, name = county, data)
    } else {
      my_attr <- data_map %>%
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
    if (input$rural != "All") {
      map_selected2 <- map_selected2[map_selected2$rural == input$rural, ]
    }
    if (input$county != "All") {
      map_selected2 <- map_selected2[map_selected2$name == input$county, ]
    }
    
    fill_palette <- ifelse(
      input$category == "Violent", "Reds", ifelse(
        input$category == "Property", "Purples", "RdPu"
      )
    )
    fill_color <- colorQuantile(fill_palette, map_selected$data)

    leaflet(map_selected2) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -89.5, lat = 39.8, zoom = 6) %>%
      addPolygons(
        data = mymap,
        weight = 1.1,
        color = "darkgray",
        fillOpacity = 0.2,
        fillColor = "lightgrey"
      ) %>%
      addPolygons(
        weight = 1.3,
        color = "#333",
        fillOpacity = 0.8,
        fillColor = ~fill_color(data),
        label = ~paste0(
          as.character(name),
          ifelse(input$format == "Count", " (count)", " (rate)"),
          ": ",
          prettyNum(round(data, 2), big.mark=",")
        ),
        labelOptions = labelOptions(
          offset = c(0, -20),
          direction = "top",
          textsize = "12px",
          style = list("background" = "white")
        ),
        highlightOptions = highlightOptions(
          weight = 4,
          color = "black",
          bringToFront = FALSE
        )
      ) %>%
      addLegend(
        "bottomleft",
        title = "Quantiles",
        pal = fill_color,
        values = ~data,
        na.label = "No data",
        opacity = .8
      )
  })

  
  # data table------------------------------------------------------------------
  output$dataTable <- renderDataTable({
    data_table <- data_output %>%
      filter(year %in% seq(input$range[1], input$range[2]))

    if(input$region != "All"){
      data_table <- data_output[data_table$region == input$region, ]
    }
    if(input$rural != "All"){
      data_table <- data_output[data_table$rural == input$rural, ]
    }
    if(input$county != "All"){
      data_table <- data_output[data_table$county == input$county, ]
    }
    
    if (input$format == "Count") {
      data_table  
    } else {
      data_table %>%
        mutate(
          violent = apply_rate(violent, population),
          property = apply_rate(property, population)
        )
    }
  }, rownames = FALSE)
}