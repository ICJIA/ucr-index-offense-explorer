data_table_ui <- function() {
  fluidRow(
    id="data-table",
    column(
      12, style = "padding:0 20px;",
      h3("Data Table"),
      withSpinner(dataTableOutput("dataTable"), type = 4)
    )
  )
}
