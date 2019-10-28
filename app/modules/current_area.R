current_area <- function(input, output) {
  output$current_area <- renderText({
    region <- input$region
    rural <- input$rural
    county <- input$county
    
    {
      if (region != "All") {
        if (region == "Cook") paste(region, "County")
        else {
          if (rural != "All") paste(region, "Region", rural, sep = ", ")
          else paste(region, "Region")
        }
      } else if (rural != "All") {
        if (county != "All") paste(county, "County") else rural
      } else "Illinois"
    } %>%
      paste("Index Offenses in", ., sep=" ")
  })
}
