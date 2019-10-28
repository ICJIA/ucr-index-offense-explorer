input_unit_ui <- function() {
  radioButtons(
    "unit",
    "Select data unit",
    choices = c("Count", "Rate (per 100K)"),
    selected = "Count"
  )
}

input_category_ui <- function() {
  radioButtons(
    "category",
    "Select crime category",
    choices = c("All", "Person", "Property"),
    selected = "All"
  )
}

input_range_ui <- function() {
  year <- APP_DATA$year

  sliderInput(
    "range",
    "Select years",
    min = min(as.integer(year)),
    max = max(as.integer(year)),
    value = c(min(as.integer(year)), max(as.integer(year))),
    step = 1,
    sep = ""
  )
}

input_region_ui <- function() {
  selectInput(
    "region",
    "Select region",
    choices = c("All", sort(unique(as.character(APP_DATA$region)))),
    selected = "All"
  )
}

input_community_ui <- function() {
  list(
    selectInput(
      "rural",
      span("Select community type*", id="rural-text"),
      choices = c("All", COMMUNITY_TYPES),
      selected = "All"
    ),
    bsPopover(
      id = "rural-text",
      title = "What is community type?",
      content = "The community type of a county is based on the definition of \"rural\" by the U.S. Census Bureau (Ratcliffe et al. 2016). The original categorization consists of three categories: (1) completely rural, (2) mostly rural, and (3) mostly urban. The UCR Index Offense Explorer has added the fourth category, completely urban, for counties consisting fully of urban areas as defined by the Bureau. Please note that the categorization in this Explorer is based on the latest Census data (2010) and may diverge from the true status of each county for other years.",
      placement = "right",
      trigger = "hover",
      options = list(container = 'body')
    )
  )
}

input_county_ui <- function() {
  selectInput(
    "county",
    "Select county",
    choices = c("All", sort(unique(as.character(APP_DATA$county)))),
    selected = "All"
  )
}
