# Author: Bobae Kang (Bobae.Kang@illinois.gov)
# Created: 2017-10-31
# Last revised: 2018-04-20
# Script title: ui.R
#-------------------------------------------------------------------------------


# PREPARE FOR THE SESSION #
#-------------------------------------------------------------------------------
options(shiny.sanitize.errors = FALSE)


# import packages
library(shiny)
library(shinyjs)
library(DT)
library(leaflet)
library(highcharter)
library(dplyr)
library(shinycssloaders)


# import data
load("data/data.rda")


# custom js code
jscode <- "shinyjs.toggleSidebar = function() { $('div.col-sm-3').has('form').toggle(); $(window).resize(); }"


# DEFINE UI #
#-------------------------------------------------------------------------------
ui <- shinyUI(fluidPage(
  
  # HEAD
  tags$head(
    includeCSS(path = "css/style.css"),
    tags$title("Uniform Crime Report Data Dashboard")
  ),
  
  # SHINYJS
  useShinyjs(),
  # extendShinyjs(text = jscode),
  extendShinyjs(text = jscode, functions = "toggleSidebar"), # for shinyappsio
  
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
          span(id = "text-identity", "Uniform Crime Report Data Dashboard")
        ),
        column(
          2,
          a(
            img(id = "logo", src = "logo-icjia-small-blue-3.png"),
            href = "http://www.icjia.state.il.us/",
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
      # selectInput(
      #   "circuit",
      #   "Select judicial circuit",
      #   choices = c("All", sort(unique(as.character(mydata$circuit)))),
      #   selected = "All"
      # ),
      selectInput(
        "county",
        "Select county",
        choices = c("All", sort(unique(as.character(mydata$county)))),
        selected = "All"
      ),
      p(
        downloadButton("downloadData","Download"), style="text-align:center;",
        actionButton("reset", "Reset"), style="text-align:center;"
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
          p("This dashboard offers an interactive way to explore the",
            em("Crime in Illinois Annual Uniform Crime Report (UCR)"),
            "data (1982-2015), originally provided by Illinois State Police and",
            "prepared by ICJIA.",
            "All datasets used in this dashboard are freely available at",
            a("the ICJIA website.", href="http://www.icjia.state.il.us/research/overview#tab_research-data", target="_blank"),
            "To learn more about the UCR data, read",
            a("this article", href="http://www.icjia.state.il.us/articles/about-uniform-crime-reporting-program-data", target="_blank"),
            "by ICJIA staff."
          ),
          p(
            "Use the filters on the side menu to explore and analyze patterns in criminal offenses.",
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
        column(4, h3(textOutput("line_title")), withSpinner(highchartOutput("line"), type = 4)),
        column(4, h3(textOutput("bar_title")), withSpinner(highchartOutput("bar"), type = 4)),
        column(4, h3(textOutput("map_title")), withSpinner(leafletOutput("map"), type = 4))
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
          href="http://visitor.r20.constantcontact.com/manage/optin?v=001MqUcqqvjwLCJXlLMSWbTe3zHHmEQgFeBuHvBcJWTbwgrxFbDSGx4HSUPpI6DJWMUPgbljtLxffqIcGFTgCnr-auak88ybvRxpoJlTMGPtZs%3D",
          class="btn btn-default",
          style="font-family:'Gentium Book Basic'; font-size:16px;"
        ),
        div(
          style = "margin-top: 15px;",
          a(
             icon("facebook-square", "fa-3x"),
             href = "http://www.facebook.com/ICJIA",
             target = "_blank",
             id = "social"
           ),
           a(
             icon("twitter-square", "fa-3x"),
             href = "http://www.twitter.com/ICJIA_Illinois",
             target = "_blank",
             id = "social"
           ),
           a(
             icon("youtube-square", "fa-3x"),
             href = "https://www.youtube.com/channel/UCtZMzk8D3P4OixYTwsfPeKA",
             target = "_blank",
             id = "social"
           ),
          a(
            icon("soundcloud", "fa-3x"),
            href = "https://www.soundcloud.com/icjia",
            target = "_blank",
            id = "social"
          ),
          a(
            icon("github", "fa-3x"),
            href = "https://github.com/ICJIA",
            target = "_blank",
            id = "social"
          ),
          a(
            icon("rss-square", "fa-3x"),
            href = "https://www.icjia.state.il.us/feed",
            target = "_blank",
            id = "social"
          ),
          a(
            icon("envelope", "fa-3x"),
            href = "https://www.icjia.state.il.us/about/contact",
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
             href = "http://www.icjia.state.il.us/",
             target = "_blank",
             id = "social",
             style = "margin:0;"
           ),
          "  | ",
          a(
            "Privacy",
            href = "http://www.icjia.state.il.us/about/privacy",
            target = "_blank",
            id = "social",
            style = "margin:0;"
          )
        )
      )
    )
  )
))
