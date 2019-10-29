download_data_ui <- function() {
  p(
    downloadButton("downloadData","Download"), style="text-align:center;",
    actionButton("reset", "Reset"), style="text-align:center;"
  )
}
