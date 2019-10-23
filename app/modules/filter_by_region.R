filter_by_region <- function(input, session, data_update) {
  observeEvent(input$region, {

    if (input$region == "All") {
      updateSelectInput(
        session,
        inputId  = "region",
        choices  = c("All", sort(unique(as.character(APP_DATA$region)))),
        selected = "All"
      )

      updateSelectInput(
        session,
        inputId  = "rural",
        choices  = c("All", intersect(COMMUNITY_TYPES, APP_DATA$rural)),
        selected = "All"
      )

      updateSelectInput(
        session,
        inputId  = "county",
        choices  = c("All", sort(unique(as.character(APP_DATA$county)))),
        selected = "All"
      )
    } else {
      data_update <-
        APP_DATA %>%
        filter(region == input$region)

      updateSelectInput(
        session,
        inputId = "rural",
        choices = c("All", intersect(COMMUNITY_TYPES, data_update$rural))
      )

      updateSelectInput(
        session,
        inputId = "county",
        choices = c("All", sort(unique(as.character(data_update$county))))
      )
    }
  })
}
