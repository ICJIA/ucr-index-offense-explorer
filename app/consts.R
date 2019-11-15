# import data
load("data/data.rda")


# define constants
APP_DATA <- app_data

APP_MAP <- app_map

APP_VERSION <- "1.1.0"

COMMUNITY_TYPES <- c(
  "Completely Rural",
  "Mostly Rural",
  "Mostly Urban",
  "Completely Urban"
)

ICON_EXTERNAL_LINK <- shiny::tags$sup(
  style="color: grey;",
  icon("external-link", "fa")
)

URLS <- list(
  home        = "http://icjia.illinois.gov/",
  cj_dispatch = "http://visitor.r20.constantcontact.com/manage/optin?v=001MqUcqqvjwLCJXlLMSWbTe3zHHmEQgFeBuHvBcJWTbwgrxFbDSGx4HSUPpI6DJWMUPgbljtLxffqIcGFTgCnr-auak88ybvRxpoJlTMGPtZs%3D",
  facebook    = "http://www.facebook.com/ICJIA",
  twitter     = "http://www.twitter.com/ICJIA_Illinois",
  youtube     = "https://www.youtube.com/channel/UCtZMzk8D3P4OixYTwsfPeKA",
  soundcloud  = "https://www.soundcloud.com/icjia",
  github      = "https://github.com/ICJIA",
  ucr         = "https://www.isp.state.il.us/crime/ucrhome.cfm"
)
