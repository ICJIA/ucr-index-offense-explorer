kpi_1 <- function(input, output, data) {
  output$kpi_1 <- renderUI({
    if (input$category == "Violent") {
      value <- sum(data$violent_crime, na.rm = TRUE)
    } else if (input$category == "Property") {
      value <- sum(data$property_crime, na.rm = TRUE)
    } else {
      value <- sum(data$violent_crime, data$property_crime, na.rm = TRUE)
    }

    if(input$unit == "Count") {
      desc <- paste0("Offenses in ", input$range[2])
      if (value > 10000) {
        value <- paste0(round(value / 1000), "K")
      }
    } else {
      desc <- paste0("Crime rate in ", input$range[2])
      value <- apply_rate(value, sum(data$population, na.rm = TRUE))
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
}

kpi_2 <- function(input, output, data) {
  output$kpi_2 <- renderUI({
    data_max <- filter(data, year == input$range[2])
    data_pre <- filter(data, year == input$range[2] - 1)

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
}

kpi_3 <- function(input, output, data) {
  output$kpi_3 <- renderUI({
    data_max <- filter(data, year == input$range[2])
    data_min <- filter(data, year == input$range[1])

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
}
