plot_line <- function(input, output, data_reactive) {
  output$line_title <- renderText({
    paste0("Trend (", input$range[1], "-", input$range[2], ")") %>%
      decorate_plot_title(input$category)
  })

  output$line <- renderHighchart({
    reporting <-
      data_reactive() %>%
      na.omit() %>%
      group_by(year) %>%
      count(name = 'reporting')
    
    data <-
      data_reactive() %>%
      group_by(year) %>%
      summarise_at(
        vars(Person = person_crime, Property = property_crime),
        function(x) {
          sum2 <-
            if (input$county == "All") function(x) sum(x, na.rm = TRUE)
            else sum
          if (input$unit == "Count") sum2(x)
          else apply_rate(sum2(x), sum(.$population))
        }
      ) %>%
      left_join(reporting, by = "year")

    highchart() %>%
      {
        if (input$category == "All") {
          hc_add_series(
            .,
            name  = "Person",
            data  = data$Person,
            color = "#f45b5b"
          ) %>%
          hc_add_series(
            name  = "Property",
            data  = data$Property,
            color = "#8085e9"
          )
        } else {
          hc_add_series(
            .,
            name  = input$category,
            data  = data[[input$category]],
            color = if (input$category == "Person") "#f45b5b" else "#8085e9"
          ) %>%
          hc_legend(enabled = FALSE)
        }
      } %>%
      hc_add_series(
        name = "Reporting county",
        data = data$reporting,
        color = "#fff",
        dashStyle = "Dot",
        lineWidth = 0,
        marker = list(
          radius = 0
        ),
        showInLegend = FALSE,
        states = list(
          hover = list(
            enabled = FALSE
          )
        )
      ) %>%
      hc_tooltip(shared = TRUE) %>%
      hc_xAxis(categories = data$year) %>%
      hc_yAxis(
        title = list(
          text = if (input$unit == "Count") "count" else "rate (per 100k)"
        ),
        min = 0,
        max = {
          if (input$category == "All") max(data$Property)
          else max(data[[input$category]])
        }
      ) %>%
      hc_add_theme(hc_theme_sandsignika())
  })
}
