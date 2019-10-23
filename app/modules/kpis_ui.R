kpis_ui <- function() {
  fluidRow(
    class = "kpis",
    column(4, uiOutput("kpi_1"), class = "kpi"),
    column(4, uiOutput("kpi_2"), class = "kpi"),
    column(4, uiOutput("kpi_3"), class = "kpi")
  )
}
