filter_inputs <- function(input, session) {
  observeEvent(input$reset, {
    updateRadioButtons(
      session,
      inputId = "unit",
      selected = "Count"
    )

    updateRadioButtons(
      session,
      inputId = "category",
      selected = "All"
    )

    updateSliderInput(
      session,
      inputId = "range",
      value = c(min(as.integer(APP_DATA$year)), max(as.integer(APP_DATA$year)))
    )

    updateSelectInput(
      session,
      inputId = "region",
      choices = c("All", sort(unique(as.character(APP_DATA$region)))),
      selected = "All"
    )

    updateSelectInput(
      session,
      inputId = "rural",
      choices = c("All", COMMUNITY_TYPES),
      selected = "All"
    )

    updateSelectInput(
      session,
      inputId = "county",
      choices = c("All", sort(unique(as.character(APP_DATA$county)))),
      selected = "All"
    )
  })
}

