data_table <- function(input, output, data_reactive) {
  output$dataTable <- renderDataTable({
    data_reactive() %>%
      select_data_unit(input, violent_crime, property_crime) %>%
      select(year:population, violent_crime, property_crime)
  }, rownames = FALSE)
}
