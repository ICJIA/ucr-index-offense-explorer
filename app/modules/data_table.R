data_table <- function(input, output, data_reactive) {
  output$dataTable <- renderDataTable({
    data_reactive() %>%
      select_data_unit(input, person_crime, property_crime) %>%
      select(year:population, person_crime, property_crime)
  }, rownames = FALSE)
}
