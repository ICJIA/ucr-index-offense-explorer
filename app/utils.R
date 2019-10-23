apply_rate <- function(n, d) round(n / d * 100000, 2)

filter_area <- function(data, input) {
  if (input$region != "All") {
    data <- filter(data, region == input$region)
  }
  if (input$rural != "All") {
    data <- filter(data, rural == input$rural)
  }
  if (input$county != "All") {
    data <- filter(data, county == input$county)
  }
  
  data
}

filter_latest_year <- function(data, input) filter(data, year == input$range[2])

filter_range <- function(data, input) {
  filter(data, year %in% seq(input$range[1], input$range[2]))
}

select_data_unit <- function(data, input, ...) {
  if (input$format == "Count") data
  else mutate_at(data, vars(...), function(z) apply_rate(z, data$population))
}

