data_table <- function(input, output, data) {
  output$dataTable <- renderDataTable({
    data %>%
      select_data_unit(input, violent_crime, property_crime) %>%
      select(year:population, violent_crime, property_crime)
  }, rownames = FALSE)
}
