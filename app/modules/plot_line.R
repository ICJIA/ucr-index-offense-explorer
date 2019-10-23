plot_line <- function(input, output, data) {
  output$line_title <- renderText({
    text_output <- paste0("Trend (", input$range[1], "-", input$range[2], ")")

    if (input$category != "All"){
      paste(text_output, input$category, sep=": ")
    } else {
      text_output
    }
  })

  output$line <- renderHighchart({
    data_plot <- data %>%
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
}
