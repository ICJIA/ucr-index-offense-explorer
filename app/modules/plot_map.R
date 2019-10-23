plot_map <- function(input, output) {
  output$map_title <- renderText({
    text_output <- paste0("Geography (", input$range[2], ")", "*")

    if(input$category != "All"){
      paste(text_output, input$category, sep=": ")
    } else {
      text_output
    }
  })

  output$map <- renderLeaflet({
    map_selected <- APP_MAP
    data_map <- filter_latest_year(APP_DATA, input)

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
}
