app_version_ui <- function() {
  p(
    strong("Version:"),
    a(
      APP_VERSION,
      icon("github", "fa-1x"),
      href = paste0(URLS$github, "/ucr-data-explorer"),
      target = "_blank"
    )
  )
}
