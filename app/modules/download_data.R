download_data <- function(output, data) {
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-filtered-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(na.omit(data), file, row.names = FALSE)
    }
  )
}
