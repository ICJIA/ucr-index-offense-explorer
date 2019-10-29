filter_by_community_type <- function(input, session, data_update) {
  observeEvent(input$rural, {
    
    if (input$rural == "All") {
      updateSelectInput(
        session,
        inputId  = "county",
        selected = "All"
      )
    } else {
      data_update <-
        APP_DATA %>%
        filter(rural == input$rural)

      updateSelectInput(
        session,
        inputId = "county",
        choices = c("All", sort(unique(as.character(data_update$county))))
      )

      if (input$region != "All") {
        data_update <-
          data_update %>%
          filter(region == input$region)

        updateSelectInput(
          session,
          inputId = "county",
          choices = c("All", sort(unique(as.character(data_update$county))))
        )
      }
    }
  })
}
