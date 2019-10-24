.gather_selected <- function(x, type, ...) {
  capitalize <- function(x)
    paste0(toupper(substring(x, 1, 1)), substring(x, 2))
  
  select(x, ...) %>%
    gather(..., key = "category", value = "count") %>%
    mutate(
      type = type,
      category = gsub("_", " ", capitalize(category))
    )
}

plot_bar <- function(input, output, data_reactive) {
  output$bar_title <- renderText({
    paste0("Offense Type (", input$range[2], ")", "*") %>%
      decorate_plot_title(input$category)
  })

  output$bar <- renderHighchart({
    data <-
      data_reactive() %>%
      summarise_at(
        vars(murder:aggravated_assault, burglary:arson),
        function(x) {
          if (input$unit == "Count") sum(x, na.rm = TRUE)
          else apply_rate(sum(x, na.rm = TRUE), sum(.$population))
        }
      ) %>%
      {
        if (input$category == "All") {
          rbind(
            .gather_selected(., "Violent", murder:aggravated_assault),
            .gather_selected(., "Property", burglary:arson)
          )
        } else {
          if (input$category == "Violent")
            .gather_selected(., murder:aggravated_assault, type = "Violent")
          else .gather_selected(., burglary:arson, type = "Property")
        }
      }

    data %>%
      {
        if (input$category == "All") {
          hchart(
            .,
            type = "column",
            mapping = hcaes(
              x = category,
              y = count,
              group = factor(type, levels = c("Violent", "Property"))
            )
          )
        } else {
          hchart(
            .,
            type = "column",
            mapping = hcaes(x = category, y = count),
            name = input$category,
            color = if (input$category == "Violent") "#f45b5b" else "#8085e9"
          )
        }
      } %>%
      hc_xAxis(title = "") %>%
      hc_yAxis(
        title = list(
          text = if (input$unit == "Count") "count" else "rate (per 100k)"
        ),
        type = "logarithmic"
      ) %>%
      hc_add_theme(hc_theme_sandsignika())
  })
}
