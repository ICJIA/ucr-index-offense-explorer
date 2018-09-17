# Author: Bobae Kang (Bobae.Kang@illinois.gov)
# Created: 2017-10-31
# Last revised: 2018-09-14
# Script title: server.R
#-------------------------------------------------------------------------------


# PREPARE FOR THE SESSION #
#-------------------------------------------------------------------------------
# define functions to use
apply_rate <- function(n, d) round(n / d * 100000, 2)

colname_to_value <- function(x) {
  gsub("_", " ", paste0(toupper(substring(x, 1, 1)), tolower(substring(x, 2))))
}

get_select_area <- function(x, input) {
  if (input$region != "All") {
    x <- filter(x, region == input$region)
  }
  if (input$rural != "All") {
    x <- filter(x, rural == input$rural)
  }
  if (input$county != "All") {
    x <- filter(x, county == input$county)
  }
  
  x
}

get_select_range <- function(x, input) {
  filter(x, year %in% seq(input$range[1], input$range[2]))
}

get_latest_year <- function(x, input) filter(x, year == input$range[2])

gather_selected <- function(x, type, ...) {
  select(x, ...) %>%
    gather(..., key = "category", value = "count") %>%
    mutate(
      type = type,
      category = colname_to_value(category)
    )
}

count_or_rate <- function(x, input, ...) {
  if(input$format == "Count") x
  else mutate_at(x, vars(...), function(z) apply_rate(z, x$population))
}


# DEFINE SERVER LOGIC #
#-------------------------------------------------------------------------------
server <- function (input, output, session) {
  # toggle sidebar
  observeEvent(input$toggleSidebar, {
    toggleCssClass("main", "col-sm-9")
    js$toggleSidebar()
  })


  # data output-----------------------------------------------------------------
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
      choices  = c("All", sort(unique(as.character(mydata$region)))),
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
      choices  = c("All", sort(unique(as.character(mydata$county)))),
      selected = "All"
    )
  })
  
  
  # filtering data--------------------------------------------------------------
  observeEvent(input$region, {
    
    if (input$region == "All") {
      updateSelectInput(
        session,
        inputId  = "region",
        choices  = c("All", sort(unique(as.character(mydata$region)))),
        selected = "All"
      )
      
      updateSelectInput(
        session,
        inputId  = "rural",
        choices  = c("All", intersect(rural_labels, mydata$rural)),
        selected = "All"
      )
      
      updateSelectInput(
        session,
        inputId  = "county",
        choices  = c("All", sort(unique(as.character(mydata$county)))),
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
  
  
  # filtered data---------------------------------------------------------------
  data_select_area <- reactive({
    get_select_area(mydata, input)
  })
  
  data_select_area_and_range <- reactive({
    get_select_range(data_select_area(), input)
  })
  
  data_select_area_lastest_year <- reactive({
    get_latest_year(data_select_area(), input)
  })
  
  data_select_area_count_or_rate <- reactive({
    count_or_rate(data_select_area(), input, violent_crime, property_crime)
  })
  
 
  # download data---------------------------------------------------------------
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-filtered-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(na.omit(data_select_area_and_range()), file, row.names = FALSE)
    }
  )

  
  # Text on the top-------------------------------------------------------------
  output$current <- renderText({
    text_output <- "Illinois"
    
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
    data_kpi <- data_select_area_lastest_year()

    if (input$category == "Violent") {
      value <- sum(data_kpi$violent_crime, na.rm = TRUE)
    } else if (input$category == "Property") {
      value <- sum(data_kpi$property_crime, na.rm = TRUE)
    } else {
      value <- sum(data_kpi$violent_crime, data_kpi$property_crime, na.rm = TRUE)
    }
    
    if(input$format == "Count") {
      desc <- paste0("Offenses in ", input$range[2])
      if (value > 10000) {
        value <- paste0(round(value / 1000), "K")
      }
    } else {
      desc <- paste0("Crime rate in ", input$range[2])
      value <- apply_rate(value, sum(data_kpi$population, na.rm = TRUE))
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
    data_kpi <- data_select_area_count_or_rate()
        
    data_max <- filter(data_kpi, year == input$range[2])
    data_pre <- filter(data_kpi, year == input$range[2] - 1)
    
    if (input$category == "Violent") {
      value_max <- sum(data_max$violent_crime, na.rm = TRUE)
      value_pre <- sum(data_pre$violent_crime, na.rm = TRUE)
    } else if (input$category == "Property") {
      value_max <- sum(data_max$property_crime, na.rm = TRUE)
      value_pre <- sum(data_pre$property_crime, na.rm = TRUE)
    } else {
      value_max <- sum(data_max$violent_crime, data_max$property_crime, na.rm = TRUE)
      value_pre <- sum(data_pre$violent_crime, data_pre$property_crime, na.rm = TRUE)
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
    data_kpi <- data_select_area_count_or_rate()
    
    data_max <- filter(data_kpi, year == input$range[2])
    data_min <- filter(data_kpi, year == input$range[1])
    
    if (input$category == "Violent"){
      value_max <- sum(data_max$violent_crime, na.rm = TRUE)
      value_min <- sum(data_min$violent_crime, na.rm = TRUE)
    } else if (input$category == "Property"){
      value_max <- sum(data_max$property_crime, na.rm = TRUE)
      value_min <- sum(data_min$property_crime, na.rm = TRUE)
    } else {
      value_max <- sum(data_max$violent_crime, data_max$property_crime, na.rm = TRUE)
      value_min <- sum(data_min$violent_crime, data_min$property_crime, na.rm = TRUE)
    }
    
    value <- paste0(round((value_max - value_min) / value_min * 100, 1), "%")
    
    tagList(
      tags$h1(value),
      tags$p(
        icon("sort"),
        paste0("Change, ", input$range[1], "-", input$range[2]),
        style = "font-size:1.1em;"
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
    data_plot <- data_select_area_and_range() %>%
      group_by(year) %>%
      summarise_at(
        vars(Violent = violent_crime, Property = property_crime),
        function(x) {
          ifelse(
            input$format == "Count",
            sum(x, na.rm = TRUE),
            apply_rate(sum(x, na.rm = TRUE), sum(.$population))
          )
        }
      )
    
    plot <- highchart() %>% 
      hc_xAxis(categories = data_plot$year)
    
    if (input$category == "All") {
      plot <- plot %>% 
        hc_add_series(
          name  = "Violent",
          data  = data_plot$Violent,
          color = "#f45b5b"
        ) %>% 
        hc_add_series(
          name  = "Property",
          data  = data_plot$Property,
          color = "#8085e9"
        )
    } else {
      hc_color <- ifelse(input$category == "Violent", "#f45b5b", "#8085e9")
      plot <- plot %>%
        hc_add_series(
          name  = input$category,
          data  = data_plot[[input$category]],
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
    data_bar <- data_select_area_lastest_year() %>%
      summarise_at(
        vars(murder:aggravated_assault, burglary:arson),
        function(x) {
          ifelse(
            input$format == "Count",
            sum(x, na.rm = TRUE),
            apply_rate(sum(x, na.rm = TRUE), sum(.$population))
          ) 
        }
      )
    
    if (input$category == "All") {
      data_bar <-
        rbind(
          gather_selected(data_bar, "Violent", murder:aggravated_assault),
          gather_selected(data_bar, "Property", burglary:arson)
        )
      
      plot <-
        hchart(
          data_bar,
          type = "column",
          mapping = hcaes(
            x     = category,
            y     = count,
            group = factor(type, levels = c("Violent", "Property"))
          )
        )
    } else {
      hc_color <- ifelse(input$category == "Violent", "#f45b5b", "#8085e9")
      
      if (input$category == "Violent") {
        data_bar <-
          data_bar %>%
          gather_selected(murder:aggravated_assault, type = "Violent")
      } else {
        data_bar <-
          data_bar %>%
          gather_selected(burglary:arson, type = "Property")
      }
      
      plot <-
        hchart(
          data_bar,
          type    = "column",
          mapping = hcaes(x = category, y = count),
          name    = input$category,
          color   = hc_color
        )
    }
    
    plot %>%
      hc_xAxis(title = "") %>%
      hc_yAxis(
        title = list(text = ifelse(input$format == "Count", "count", "rate (per 100k)")),
        type  = "logarithmic"
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
    data_map <- get_latest_year(mydata, input)
    
    if (input$category == "Property") {
      
      if (input$format == "Count") {
        my_attr <- mutate(data_map, data = property_crime)
      } else {
        my_attr <- mutate(data_map, data = apply_rate(property_crime, population))
      }
      my_attr <- select(my_attr, name = county, data)
      
    } else if (input$category == "Violent") {
      
      if (input$format == "Count") {
        my_attr <- mutate(data_map, data = violent_crime)
      } else {
        my_attr <- mutate(data_map, data = apply_rate(violent_crime, population))
      }
      my_attr <- select(my_attr, name = county, data)
      
    } else {
      my_attr <- data_map %>%
        group_by(name = county) %>%
        summarise(
          data = ifelse(
            input$format == "Count",
            sum(violent_crime, property_crime),
            apply_rate(sum(violent_crime, property_crime), sum(population))
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
    
    map_selected2 %>%
      leaflet() %>%
      setView(lng = -89.5, lat = 39.8, zoom = 6) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        data        = map_selected,
        weight      = 1.1,
        color       = "darkgray",
        fillOpacity = 0.2,
        fillColor   = "lightgrey"
      ) %>%
      addPolygons(
        weight      = 1.3,
        color       = "#333",
        fillOpacity = 0.8,
        fillColor   = ~fill_color(data),
        label       = ~paste0(
          as.character(name),
          ifelse(input$format == "Count", " (count)", " (rate)"),
          ": ",
          prettyNum(round(data, 2), big.mark=",")
        ),
        labelOptions = labelOptions(
          offset    = c(0, -20),
          direction = "top",
          textsize  = "12px",
          style     = list("background" = "white")
        ),
        highlightOptions = highlightOptions(
          weight       = 4,
          color        = "black",
          bringToFront = FALSE
        )
      ) %>%
      addLegend(
        "bottomleft",
        title    = "Quantiles",
        pal      = fill_color,
        values   = ~data,
        na.label = "No data",
        opacity  = .8
      )
  })

  
  # data table------------------------------------------------------------------
  output$dataTable <- renderDataTable({
    data_select_area_and_range() %>%
      count_or_rate(input, violent_crime, property_crime) %>%
      select(year:population, violent_crime, property_crime)
  }, rownames = FALSE)
}