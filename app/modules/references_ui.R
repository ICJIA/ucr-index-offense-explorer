references_ui <- function() {
  p(
    strong("References:"),
    br(),
    "(1) Hughes, E. (2016). ",
    a(
      em("About Uniform Crime Reporting Program data."),
      ICON_EXTERNAL_LINK,
      href = paste0(URLS$home, "researchhub/articles/about-uniform-crime-reporting-program-data"),
      target = "_blank"
    ),
    " Chicago, IL: Illinois Criminal Justice Information Authority.",
    br(),
    "(2) Ratcliff, M., Burd, C., Holder, K., & Fields, A. (2016). ",
    a(
      em("Defining Rural at the U.S. Census Bureau: American Community Survey and geography Brief."),
      ICON_EXTERNAL_LINK,
      href = "https://www2.census.gov/geo/pdfs/reference/ua/Defining_Rural.pdf",
      target = "_blank"
    ),
    " Suitland, MD: U.S. Census Bureau."
  )
}
