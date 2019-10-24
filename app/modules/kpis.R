compute_percent_change <- function(data, category, year1, year2) {
  sum2 <- function(...) sum(..., na.rm = TRUE)

  data %>%
    rename(v = violent_crime, p = property_crime) %>%
    list(y1 = filter(., year == year1), y2 = filter(., year == year2)) %>%
    with({
      if (category == "Violent") list(y1 = sum2(y1$v), y2 = sum2(y2$v))
      else if (category == "Property") list(y1 = sum2(y1$p), y2 = sum2(y2$p))
      else list(y1 = sum2(y1$v, y1$p), y2 = sum2(y2$v, y2$p))
    }) %>%
    with({ (y2 - y1) / y1 * 100 })
}

kpi_1 <- function(input, output, data_reactive) {
  output$kpi_1 <- renderUI({
    data <- data_reactive()

    value <-
      {
        if (input$category == "Violent") sum(data$violent_crime, na.rm = TRUE)
        else if (input$category == "Property") sum(data$property_crime, na.rm = TRUE)
        else sum(data$violent_crime, data$property_crime, na.rm = TRUE)
      } %>%
      {
        if (input$unit == "Count")
          if (. > 10000) paste0(round(. / 1000), "K") else .
        else apply_rate(., sum(data$population, na.rm = TRUE))
      }

    desc <-
      { if (input$unit == "Count") "Offenses in" else "Crime rate in" } %>%
      paste(input$range[2]) 

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

kpi_2 <- function(input, output, data_reactive) {
  output$kpi_2 <- renderUI({
    value <-
      data_reactive() %>%
      compute_percent_change(
        category = input$category,
        year1 = input$range[2] - 1,
        year2 = input$range[2]
      ) %>%
      round(1) %>%
      paste0("%")

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

kpi_3 <- function(input, output, data_reactive) {
  output$kpi_3 <- renderUI({
    value <-
      data_reactive() %>%
      compute_percent_change(
        category = input$category,
        year1 = input$range[1],
        year2 = input$range[2]
      ) %>%
      round(1) %>%
      paste0("%")

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
