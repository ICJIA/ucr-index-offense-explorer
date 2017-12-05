# Author: Bobae Kang (Bobae.Kang@illinois.gov)
# Created: 10/31/2017
# Last revised: 11/7/2017
# Script title: ui.R
#---------------------------------------------------------------------------
# Script description:
# The current script is to define UI for the Shiny application for ISP data.
#---------------------------------------------------------------------------
# Table of contents:
# * PREPARE FOR THE SESSION
# * DEFINE UI
#---------------------------------------------------------------------------


# PREPARE FOR THE SESSION #
#---------------------------------------------------------------------------
options(shiny.sanitize.errors = FALSE)

# import packages
library(shiny)
library(shinydashboard)
library(DT)
library(leaflet)
library(highcharter)
library(readr)
library(dplyr)


# import data
mydata <- read.csv('data.csv')


# DEFINE UI #
#---------------------------------------------------------------------------
ui <- dashboardPage(
  
  # dashboard header--------------------------------------------------------
  dashboardHeader(
    title = "ICJIA Uniform Crime Report Data Dashbaord",
    titleWidth = 350
  ),
  
  
  # dashboard sidebar-------------------------------------------------------
  dashboardSidebar(
    
    fluidRow(
      column(12, style = 'margin-top:20px;',
             actionButton("reset", "Reset")
      )
    ),
    
    radioButtons(
      'crimeCat',
      'Select crime category',
      choices = c('All', 'Property', 'Violent'),
      selected = 'All'
    ),
    
    selectInput(
      'region',
      'Select region',
      choices = c("All", sort(unique(as.character(mydata$region)))),
      selected = 'All'
    ),
    
    
    selectInput(
      'countyType',
      'Select county type',
      choices = c("All", sort(unique(as.character(mydata$countyType)))),
      selected = 'All'
    ),
    
    
    selectInput(
      'county',
      'Select county',
      choices = c("All", sort(unique(as.character(mydata$county)))),
      selected = 'All'
    ),
    
    
    fluidRow(
      column(12, style = 'padding:20px;', offset = 2,
             downloadButton(
               "downloadData",
               "Download Data"
             )
      )
    ),
    
    fluidRow(
      column(12, style = 'font-size: 10px; margin-top:155px; text-align:center;',
             a(icon("facebook-square", "fa-3x"),
               href = 'http://www.facebook.com/ICJIA',
               id = 'social'),
             a(icon("twitter-square", "fa-3x"),
               href = 'http://www.twitter.com/ICJIA_Illinois',
               id = 'social'),
             a(icon("youtube-square", "fa-3x"),
               href = 'https://www.youtube.com/channel/UCtZMzk8D3P4OixYTwsfPeKA',
               id = 'social')
      ),
      column(12, style = 'font-size: 10px; margin-top:10px; text-align:center;',
             p('(312) 793-8550 - Fax: (312) 793-8422-',
               a('cja.irc@illinois.gov',
                 href = 'mailto:cja.irc@illinois.gov',
                 id = 'social')
             )
      ),
      column(12, style = 'text-align:center;',
             p(HTML('&copy;'), '2017',
               a('Illinois Criminal Justice Information Authority',
                 href = 'http://www.icjia.state.il.us/',
                 id = 'social')
             )
      )
    )
  ),
  
  # dashboard body----------------------------------------------------------
  dashboardBody(
    
    fluidRow(
      column(8, h1(textOutput('current'))),
      column(4, a(img(src = 'logo-icjia-small-blue-3.png',
                      align = 'right',
                      style = 'height:50px;'),
                  href = 'http://www.icjia.state.il.us/'))
    ),
    
    fluidRow(
      column(12,
             p('This dashboard offers an interactive way to explore the latest',
               em('Crime in Illinois Annual Uniform Crime Report'),
               'data (2011-2015). All data sets used here are freely available at',
               a('Illinois State Police website.', href='http://www.isp.state.il.us/crime/ucrhome.cfm'),
               'You can also download the filtered data for this dashboard by clicking the "Download Data" button on the sidebar menu.'))
    ),
    
    tags$hr(style = "border-color: #ddd;"),
    
    fluidRow(
      valueBoxOutput("kpi_box_1", width = 3),
      valueBoxOutput("kpi_box_2", width = 3),
      valueBoxOutput("kpi_box_3", width = 3),
      valueBoxOutput("kpi_box_4", width = 3)
    ),
    
    fluidRow(
      column(5, h3(textOutput("lineChart_title")), highchartOutput("lineChart")),
      column(4, h3(textOutput("pieChart_title")), highchartOutput("pieChart")),
      column(3, h3("Selected Area"), leafletOutput("map"))
    ),
    
    fluidRow(
      column(12, style = 'padding:20px;',
             h3("Data Table"),
             dataTableOutput("dataTable"))
    ),
    
    tags$head(
      includeCSS(path = 'style.css')
    )
  )
)
