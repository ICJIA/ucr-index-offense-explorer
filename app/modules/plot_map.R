.get_map_attr <- function(data, input) {
  count_or_rate <- function(count, population, unit) {
    if (unit == "count") count
    else apply_rate(count, population)
  }

  filter_latest_year(data, input) %>%
    {
      if (input$category == "Property")
        mutate(., data = count_or_rate(property_crime, population, input$unit))
      else if (input$category == "Violent")
        mutate(., data = count_or_rate(violent_crime, population, input$unit))
      else {
        group_by(., county) %>%
        summarise(
          data = count_or_rate(
            sum(violent_crime, property_crime),
            sum(population),
            input$unit
          )
        )
      }
    } %>%
    select(name = county, data)
}

.get_map_data <- function(input) {
  map_data <- APP_MAP
  map_attr <- .get_map_attr(APP_DATA, input)
  
  map_data@data <-
    map_data@data %>%
    left_join(map_attr, by = "name")
  
  map_data
}

.filter_map_data <- function(data, input) {
  data %>%
    {
      if (input$region != "All") {
        if (input$region == "Cook") .[.$name == "Cook", ]
        else .[.$region == input$region, ]
      }
      else .
    } %>%
    { if (input$rural != "All") .[.$rural == input$rural, ] else . } %>%
    { if (input$county != "All") .[.$name == input$county, ] else . }
}

plot_map <- function(input, output) {
  output$map_title <- renderText({
    paste0("Geography (", input$range[2], ")", "*") %>%
      decorate_plot_title(input$category)
  })

  output$map <- renderLeaflet({
    map_data <- .get_map_data(input)
    map_filtered <- .filter_map_data(map_data, input)

    fill_color <-
      {
        if (input$category == "Violent") "Reds"
        else if (input$category == "Property") "Purples"
        else "RdPu"
      } %>%
      colorQuantile(map_data$data)

    map_filtered %>%
      leaflet() %>%
      setView(lng = -89.5, lat = 39.8, zoom = 6) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        data        = map_data,
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
          { if (input$unit == "Count") " (count)" else " (rate)" },
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
