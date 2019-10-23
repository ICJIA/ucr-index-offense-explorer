footer_social_dispatch_ui <- function() {
  list(
    h3(
      "Sign up for the cj dispatch",
      style = "text-transform:uppercase; font-size:17px; font-style:normal;"
    ),
    a(
      "Subscribe Now",
      href = URLS$cj_dispatch,
      class = "btn btn-default",
      style = "font-family:'Gentium Book Basic'; font-size:16px;"
    )
  )
}

footer_social_buttons_ui <- function() {
  div(
    style = "margin-top: 15px;",
    a(
      icon("facebook-square", "fa-3x"),
      href = URLS$facebook,
      target = "_blank",
      id = "social"
    ),
    a(
      icon("twitter-square", "fa-3x"),
      href = URLS$twitter,
      target = "_blank",
      id = "social"
    ),
    a(
      icon("youtube-square", "fa-3x"),
      href = URLS$youtube,
      target = "_blank",
      id = "social"
    ),
    a(
      icon("soundcloud", "fa-3x"),
      href = URLS$soundcloud,
      target = "_blank",
      id = "social"
    ),
    a(
      icon("github", "fa-3x"),
      href = URLS$github,
      target = "_blank",
      id = "social"
    ),
    a(
      icon("rss-square", "fa-3x"),
      href = paste0(URLS$home, "feed"),
      target = "_blank",
      id = "social"
    ),
    a(
      icon("envelope", "fa-3x"),
      href = paste0(URLS$home, "about/contact"),
      target = "_blank",
      id = "social"
    )
  )
}

footer_social_ui <- function() {
  column(
    12,
    style = "font-size: 10px; margin-top:10px; text-align:center; align-items: center",
    footer_social_dispatch_ui(),
    footer_social_buttons_ui()
  )
}

footer_contact_ui <- function() {
  column(
    12,
    style = "font-size: 11px; margin-top:15px; text-align:center;",
    p("(312) 793-8550 - Fax: (312) 793-8422 - ",
      a(
        "cja.irc@illinois.gov",
        href = "mailto:cja.irc@illinois.gov",
        target = "_blank",
        id = "social",
        style = "margin:0;"
      )
    )
  )
}

footer_copyright_ui <- function() {
  href <- URLS$home

  column(
    12,
    style = "font-size: 13px; text-align:center;",
    p(
      HTML("&copy;"),
      format(Sys.Date(), "%Y"),
      a(
        "Illinois Criminal Justice Information Authority",
        href = href,
        target = "_blank",
        id = "social",
        style = "margin:0;"
      ),
      "  | ",
      a(
        "Privacy",
        href = paste0(href, "about/privacy"),
        target = "_blank",
        id = "social",
        style = "margin:0;"
      )
    )
  )
}

footer_ui <- function() {
  tags$div(
    class="footer",
    fluidRow(
      footer_social_ui(),
      footer_contact_ui(),
      footer_copyright_ui()
    )
  )
}
