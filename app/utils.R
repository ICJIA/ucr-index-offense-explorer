apply_rate <- function(n, d) round(n / d * 100000, 2)

filter_area <- function(data, input) {
  data %>%
    { if (input$region != "All") filter(., region == input$region) else . } %>%
    { if (input$rural != "All") filter(., rural == input$rural) else . } %>%
    { if (input$county != "All") filter(., county == input$county) else . }
}

filter_latest_year <- function(data, input)
  filter(data, year == input$range[2])

filter_range <- function(data, input)
  filter(data, year %in% seq(input$range[1], input$range[2]))

select_data_unit <- function(data, input, ...) {
  if (input$unit == "Count") data
  else mutate_at(data, vars(...), function(z) apply_rate(z, data$population))
}

decorate_plot_title <- function(title, category) {
  if (category != "All") paste(title, category, sep=": ")
  else title
}