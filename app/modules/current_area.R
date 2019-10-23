current_area <- function(input, output) {
  output$current_area <- renderText({
    text_output <- "Illinois"

    if (input$region != "All") {
      text_output <- paste(input$region, "Region")
      if (input$rural != "All") {
        text_output <- paste(text_output, input$rural, sep = ", ")
      }
      if (input$region == "Cook") {
        text_output = paste(input$region, "County")
      }
    } else if (input$rural != "All") {
      text_output <- input$rural
      if (input$county != "All") {
        text_output <- paste(input$county, "County")
      }
    }

    if(input$county != "All"){
      text_output <- paste(input$county, "County")
    }

    paste("UCR Profile:", text_output, sep=" ")
  })
}
