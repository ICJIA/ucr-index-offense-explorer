title_ui <- function(url) {
  div(
    class="container-title",
    fluidRow(
      id = "title",
      column(
        10,
        style="display:flex; align-items: center;",
        actionButton("toggleSidebar", icon("bars")),
        span(id = "text-identity", "Uniform Crime Report Index Offense Explorer")
      ),
      column(
        2,
        a(
          img(id = "logo", src = "logo-icjia-small-blue-3.png"),
          href = URLS$home,
          target = "_blank",
          style = "text-decoration:none; float:right;"
        )
      )
    )
  )
}
