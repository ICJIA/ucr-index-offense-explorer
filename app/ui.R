# Author: Bobae Kang (Bobae.Kang@illinois.gov)
# Created: 2017-10-31
# Last revised: 2018-09-14
# Script title: ui.R
#-------------------------------------------------------------------------------


# PREPARE FOR THE SESSION #
#-------------------------------------------------------------------------------
version <- "1.0.4"

url <- list(
  home        = "http://www.icjia.state.il.us/",
  cj_dispatch = "http://visitor.r20.constantcontact.com/manage/optin?v=001MqUcqqvjwLCJXlLMSWbTe3zHHmEQgFeBuHvBcJWTbwgrxFbDSGx4HSUPpI6DJWMUPgbljtLxffqIcGFTgCnr-auak88ybvRxpoJlTMGPtZs%3D",
  facebook    = "http://www.facebook.com/ICJIA",
  twitter     = "http://www.twitter.com/ICJIA_Illinois",
  youtube     = "https://www.youtube.com/channel/UCtZMzk8D3P4OixYTwsfPeKA",
  soundcloud  = "https://www.soundcloud.com/icjia",
  github      = "https://github.com/ICJIA"
)

icon_external_link <- tags$sup(style="color: grey;", icon("external-link", "fa"))


# custom js code
jscode <- "shinyjs.toggleSidebar = function() { $('div.col-sm-3').has('form').toggle(); $(window).resize(); };"


# DEFINE UI #
#-------------------------------------------------------------------------------
ui <- shinyUI(fluidPage(
  
  # HEAD
  tags$head(
    includeCSS(path = "css/style.css"),
    includeHTML("google-analytics.html"),
    tags$title("Uniform Crime Report Data Dashboard")
  ),
  
  # SHINYJS
  useShinyjs(),
  extendShinyjs(text = jscode, functions = "toggleSidebar"),

  # TITLE
  titlePanel(
    div(
      class="container-title",    
      fluidRow(
        id = "title",
        column(
          10,
          style="display:flex; align-items: center;",
          actionButton("toggleSidebar", icon("bars")),
          span(id = "text-identity", "Uniform Crime Report Data Explorer")
        ),
        column(
          2,
          a(
            img(id = "logo", src = "logo-icjia-small-blue-3.png"),
            href = url$home,
            target = "_blank",
            style = "text-decoration:none; float:right;"
          )
        )
      )
    )
  ),
  
  
  sidebarLayout(
    # SIDEBAR
    #---------------------------------------------------------------------------
    sidebarPanel(
      id ="sidebar",
      width = 3,
      radioButtons(
        "format",
        "Select data format",
        choices = c("Count", "Rate (per 100K)"),
        selected = "Count"
      ),
      radioButtons(
        "category",
        "Select crime category",
        choices = c("All", "Property", "Violent"),
        selected = "All"
      ),
      sliderInput(
        "range",
        "Select years",
        min = min(as.integer(mydata$year)),
        max = max(as.integer(mydata$year)),
        value = c(min(as.integer(mydata$year)), max(as.integer(mydata$year))),
        step = 1,
        sep = ""
      ),
      selectInput(
        "region",
        "Select region",
        choices = c("All", sort(unique(as.character(mydata$region)))),
        selected = "All"
      ),
      selectInput(
        "rural",
        span("Select community type*", id="rural-text"),
        choices = c("All", rural_labels),
        selected = "All"
      ),
      bsPopover(
        id = "rural-text",
        title = "What is community type?",
        content = "The community type of a county is based on the definition of \"rural\" by the U.S. Census Bureau (Ratcliffe et al. 2016). The original categorization consists of three categories: (1) completely rural, (2) mostly rural, and (3) mostly urban. The UCR Data Explorer has added the fourth category, completely urban, for counties consisting fully of urban areas as defined by the Bureau. Please note that the categorization in this Explorer is based on the latest Census data (2010) and may diverge from the true status of each county for other years.",
        placement = "right",
        trigger = "hover",
        options = list(container = 'body')
      ),
      selectInput(
        "county",
        "Select county",
        choices = c("All", sort(unique(as.character(mydata$county)))),
        selected = "All"
      ),
      p(
        downloadButton("downloadData","Download"), style="text-align:center;",
        actionButton("reset", "Reset"), style="text-align:center;"
      ),
      br(),
      p(
        strong("Version:"),
        a(
          version,
          icon("github", "fa-1x"),
          href = paste0(url$github, "/ucr-data-explorer"),
          target = "_blank"
        )
      ),
      p(
        strong("References:"),
        br(),
        "(1) Hughes, E. (2016). ",
        a(
          em("About Uniform Crime Reporting Program data."),
          icon_external_link,
          href = paste0(url$home, "articles/about-uniform-crime-reporting-program-data"),
          target = "_blank"
        ),
        " Chicago, IL: Illinois Criminal Justice Information Authority.",
        br(),
        "(2) Ratcliff, M., Burd, C., Holder, K., & Fields, A. (2016). ",
        a(
          em("Defining Rural at the U.S. Census Bureau: American Community Survey and geography Brief."),
          icon_external_link,
          href="https://www2.census.gov/geo/pdfs/reference/ua/Defining_Rural.pdf",
          target="_blank"
        ),
        " Suitland, MD: U.S. Census Bureau."
      )
    ),
    
    # MAIN
    #---------------------------------------------------------------------------
    mainPanel(
      id = "main",
      width = 9,
      fluidRow(
        style = "margin: 0 auto;",
        column(
          12,
          h1(textOutput("current"), style="margin:0 0 10px 0; display:inline-block;"),
          p(
            strong("Uniform Crime Report Data Explorer"),
            "offers an interactive interface to the",
            em("Crime in Illinois Annual Uniform Crime Report (UCR)"),
            paste0("data (", min(mydata$year), "-", max(mydata$year), ") "),
            "originally published by Illinois State Police.",
            "All data used in the Data Explorer are available at",
            a(
              strong("the ICJIA website"),
              icon_external_link,
              ".",
              href = paste0(url$home, "research/overview#tab_research-data"),
              target="_blank"
            ),
            "Learn more about the UCR data by reading",
            a(
              strong("this article"),
              icon_external_link,
              href = paste0(url$home, "articles/about-uniform-crime-reporting-program-data"),
              target="_blank"
            ),
            " (Hughes, 2016)."
          ),
          p(
            strong("Use the filters on the side menu"),
            "to explore and analyze patterns in criminal offenses.",
            "You can toggle the side menu using the menu button on the top left.",
            "Also, clicking the \"Download\" button on the side menu will download the filtered data."
          )
        )
      ),
      
      tags$hr(style = "border-color: #ddd;"),
      
      fluidRow(
        class = "kpis",
        column(4, uiOutput("kpi_box_1"), class = "kpi"),
        column(4, uiOutput("kpi_box_2"), class = "kpi"),
        column(4, uiOutput("kpi_box_3"), class = "kpi")
      ),
      
      fluidRow(
        class = "plots",
        column(4, class = "plot_line", h3(textOutput("line_title")), withSpinner(highchartOutput("line"), type = 4)),
        column(4, class = "plot_bar", h3(textOutput("bar_title"), id="bar-title-text"), withSpinner(highchartOutput("bar"), type = 4)),
        column(4, class = "plot_map", h3(textOutput("map_title"), id="map-title-text"), withSpinner(leafletOutput("map"), type = 4)),
        bsPopover(
          id = "bar-title-text",
          title = "Bar chart scale",
          content = "Please note that the y-axis of this chart is on the log-scale for visibility reasons.",
          placement = "top",
          trigger = "hover",
          options = list(container = 'body')
        ),
        bsPopover(
          id = "map-title-text",
          title = "What is this map?",
          content = "The map shows the geographical distribution of offenses in the most recent year for your selected interval. The coloring scheme for this map is based on quantiles, as shown in the legend. Please note that the legend is dynamically adjusted to your selection of area while the color of each county is based on the state-wide quantiles and therefore remains fixed.",
          placement = "top",
          trigger = "hover",
          options = list(container = 'body')
        )
      ),
      
      
      fluidRow(
        id="data-table",
        column(
          12, style = "padding:0 20px;",
          h3("Data Table"),
          withSpinner(dataTableOutput("dataTable"), type = 4))
      )
    )
  ),
  
  # FOOTER
  #-----------------------------------------------------------------------------
  tags$div(
    class="footer",
    fluidRow(
      column(
        12,
        style = "font-size: 10px; margin-top:10px; text-align:center; align-items: center",
        h3(
          "Sign up for the cj dispatch",
          style = "text-transform:uppercase; font-size:17px; font-style:normal;"
        ),
        a(
          "Subscribe Now",
          href= url$cj_dispatch,
          class="btn btn-default",
          style="font-family:'Gentium Book Basic'; font-size:16px;"
        ),
        div(
          style = "margin-top: 15px;",
          a(
             icon("facebook-square", "fa-3x"),
             href = url$facebook,
             target = "_blank",
             id = "social"
           ),
           a(
             icon("twitter-square", "fa-3x"),
             href = url$twitter,
             target = "_blank",
             id = "social"
           ),
           a(
             icon("youtube-square", "fa-3x"),
             href = url$youtube,
             target = "_blank",
             id = "social"
           ),
          a(
            icon("soundcloud", "fa-3x"),
            href = url$soundcloud,
            target = "_blank",
            id = "social"
          ),
          a(
            icon("github", "fa-3x"),
            href = url$github,
            target = "_blank",
            id = "social"
          ),
          a(
            icon("rss-square", "fa-3x"),
            href = paste0(url$home, "feed"),
            target = "_blank",
            id = "social"
          ),
          a(
            icon("envelope", "fa-3x"),
            href = paste0(url$home, "about/contact"),
            target = "_blank",
            id = "social"
          )
        )
      ),
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
      ),
      column(
        12,
        style = "font-size: 13px; text-align:center;",
        p(
          HTML("&copy;"),
          format(Sys.Date(), "%Y"),
           a(
             "Illinois Criminal Justice Information Authority",
             href = url$home,
             target = "_blank",
             id = "social",
             style = "margin:0;"
           ),
          "  | ",
          a(
            "Privacy",
            href = paste0(url$home, "about/privacy"),
            target = "_blank",
            id = "social",
            style = "margin:0;"
          )
        )
      )
    )
  )
))
