plot_line <- function(input, output, data_reactive) {
  output$line_title <- renderText({
    paste0("Trend (", input$range[1], "-", input$range[2], ")") %>%
      decorate_plot_title(input$category)
  })

  output$line <- renderHighchart({
    data <-
      data_reactive() %>%
      group_by(year) %>%
      summarise_at(
        vars(Violent = violent_crime, Property = property_crime),
        function(x) {
          if (input$unit == "Count") sum(x, na.rm = TRUE)
          else apply_rate(sum(x, na.rm = TRUE), sum(.$population))
        }
      )

    highchart() %>% 
      {
        if (input$category == "All") {
          hc_add_series(
            .,
            name  = "Violent",
            data  = data$Violent,
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
            color = if (input$category == "Violent") "#f45b5b" else "#8085e9"
          ) %>%
          hc_legend(enabled = FALSE)
        }
      } %>%
      hc_xAxis(categories = data$year) %>%
      hc_yAxis(
        title = list(
          text = if (input$unit == "Count") "count" else "rate (per 100k)"
        ),
        min = 0
      ) %>%
      hc_add_theme(hc_theme_sandsignika())
  })
}
