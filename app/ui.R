# Author: Bobae Kang (Bobae.Kang@illinois.gov)
# Created: 2017-10-31
# Last revised: 2018-04-16
# Script title: ui.R
#---------------------------------------------------------------------------
# Script description:
# The current script is to define UI for the Shiny application for ISP data.
# (develop) v2.0: Without shinydashboard
#---------------------------------------------------------------------------


# PREPARE FOR THE SESSION #
#---------------------------------------------------------------------------
options(shiny.sanitize.errors = FALSE)

# import packages
library(shiny)
library(shinycssloaders)
library(DT)
library(leaflet)
library(highcharter)
library(dplyr)


# import data
load("data/data.rda")
# load("data/mydata.rda")


# DEFINE UI #
#---------------------------------------------------------------------------
ui <- shinyUI(fluidPage(
  
  # HEAD
  tags$head(
    includeCSS(path = "css/style.css"),
    tags$title("Uniform Crime Report Data Dashboard")
  ),
  
  # # SHINYJS
  # useShinyjs(),
  # extendShinyjs(text = jscode, functions = "reload"),
  
  # TITLE
  titlePanel(
    tags$div(
      img(id="logo", src="logo-icjia-small-blue-3.png"),
      span(id="text-identity", "Uniform Crime Report Data Dashboard")
    )
  ),
  
  
  sidebarLayout(
    # SIDEBAR
    #---------------------------------------------------------------------------
    sidebarPanel(
      width = 3,
      actionButton("reset", "Reset"),
      hr(),
      radioButtons(
        "crimeCat",
        "Select crime category",
        choices = c("All", "Property", "Violent"),
        selected = "All"
      ),
      selectInput(
        "region",
        "Select region",
        choices = c("All", sort(unique(as.character(mydata$region)))),
        selected = "All"
      ),
      selectInput(
        "type",
        "Select county type",
        choices = c("All", sort(unique(as.character(mydata$type)))),
        selected = "All"
      ),
      selectInput(
        "county",
        "Select county",
        choices = c("All", sort(unique(as.character(mydata$county)))),
        selected = "All"
      ),
      hr(),
      downloadButton("downloadData","Download")
    ),
    
    # MAIN
    #---------------------------------------------------------------------------
    mainPanel(
      width = 9,
      fluidRow(
        column(
          12,
          h1(textOutput("current"), id="title"),
          p("This dashboard offers an interactive way to explore the latest",
            em("Crime in Illinois Annual Uniform Crime Report"),
            "data (2001-2015). All data sets used here are freely available at",
            a("Illinois State Police website.", href="http://www.isp.state.il.us/crime/ucrhome.cfm"),
            "You can also download the filtered data for this dashboard by clicking the \"Download\" button on the sidebar menu."
          )
        )
      ),
      
      tags$hr(style = "border-color: #ddd;"),
      
      fluidRow(
        class = "kpis",
        column(3, uiOutput("kpi_box_1"), class = "kpi"),
        column(3, uiOutput("kpi_box_2"), class = "kpi"),
        column(3, uiOutput("kpi_box_3"), class = "kpi"),
        column(3, uiOutput("kpi_box_4"), class = "kpi")
      ),
      
      fluidRow(
        column(4, h3(textOutput("lineChart_title")), withSpinner(highchartOutput("lineChart"), type = 4)),
        column(4, h3(textOutput("pieChart_title")), withSpinner(highchartOutput("pieChart"), type = 4)),
        column(4, h3("Selected Area"), withSpinner(leafletOutput("map"), type = 4))
      ),
      
      fluidRow(
        column(12, style = "padding:20px;",
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
      column(12, style = "font-size: 10px; margin-top:10px; text-align:center;",
             a(icon("facebook-square", "fa-3x"),
               href = "http://www.facebook.com/ICJIA",
               id = "social"),
             "    ",
             a(icon("twitter-square", "fa-3x"),
               href = "http://www.twitter.com/ICJIA_Illinois",
               id = "social"),
             "    ",
             a(icon("youtube-square", "fa-3x"),
               href = "https://www.youtube.com/channel/UCtZMzk8D3P4OixYTwsfPeKA",
               id = "social")
      ),
      column(12, style = "font-size: 12px; margin-top:10px; text-align:center;",
             p("(312) 793-8550 - Fax: (312) 793-8422-",
               a("cja.irc@illinois.gov",
                 href = "mailto:cja.irc@illinois.gov",
                 id = "social")
             )
      ),
      column(12, style = "font-size: 13px; text-align:center;",
             p(HTML("&copy;"), "2018",
               a("Illinois Criminal Justice Information Authority",
                 href = "http://www.icjia.state.il.us/",
                 id = "social")
             )
      )
    )
  )
))
