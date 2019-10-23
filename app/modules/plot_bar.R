colname_to_value <- function(x) {
  gsub("_", " ", paste0(toupper(substring(x, 1, 1)), tolower(substring(x, 2))))
}

gather_selected <- function(x, type, ...) {
  select(x, ...) %>%
    gather(..., key = "category", value = "count") %>%
    mutate(
      type = type,
      category = colname_to_value(category)
    )
}

plot_bar <- function(input, output, data) {
  output$bar_title <- renderText({
    text_output <- paste0("Offense Type (", input$range[2], ")", "*")

    if(input$category != "All"){
      paste(text_output, input$category, sep = ": ")
    } else {
      text_output
    }
  })

  output$bar <- renderHighchart({
    data_bar <- data %>%
      summarise_at(
        vars(murder:aggravated_assault, burglary:arson),
        function(x) {
          ifelse(
            input$unit == "Count",
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
            x = category,
            y = count,
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
          type = "column",
          mapping = hcaes(x = category, y = count),
          name = input$category,
          color = hc_color
        )
    }

    plot %>%
      hc_xAxis(title = "") %>%
      hc_yAxis(
        title = list(text = ifelse(input$unit == "Count", "count", "rate (per 100k)")),
        type = "logarithmic"
      ) %>%
      hc_add_theme(hc_theme_sandsignika())
  })
}
