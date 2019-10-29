toggle_sidebar <- function(input) {
  observeEvent(input$toggleSidebar, {
    toggleCssClass("main", "col-sm-9")
    toggleCssClass("sidebar", "hidden")
    js$resize()
  })
}
