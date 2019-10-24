.plot_line_ui <- function() {
  column(4, class = "plot_line", h3(textOutput("line_title")), withSpinner(highchartOutput("line"), type = 4))
}

.plot_bar_ui <- function() {
  list(
    column(4, class = "plot_bar", h3(textOutput("bar_title"), id="bar-title-text"), withSpinner(highchartOutput("bar"), type = 4)),
    bsPopover(
      id = "bar-title-text",
      title = "Bar chart scale",
      content = "Please note that the y-axis of this chart is on the log-scale for visibility reasons.",
      placement = "top",
      trigger = "hover",
      options = list(container = 'body')
    )
  )
}

.plot_map_ui <- function() {
  list(
    column(4, class = "plot_map", h3(textOutput("map_title"), id="map-title-text"), withSpinner(leafletOutput("map"), type = 4)),
    bsPopover(
      id = "map-title-text",
      title = "What is this map?",
      content = "The map shows the geographical distribution of offenses in the most recent year for your selected interval. The coloring scheme for this map is based on quantiles, as shown in the legend. Please note that the legend is dynamically adjusted to your selection of area while the color of each county is based on the state-wide quantiles and therefore remains fixed.",
      placement = "top",
      trigger = "hover",
      options = list(container = 'body')
    )
  )
}

plots_ui <- function() {
  fluidRow(
    class = "plots",
    .plot_line_ui(),
    .plot_bar_ui(),
    .plot_map_ui()
  )
}
