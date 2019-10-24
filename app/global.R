# Author: Bobae Kang (Bobae.Kang@illinois.gov)
# Created: 2018-09-14
# Last revised: 2018-09-14
# Script title: global.R
#-------------------------------------------------------------------------------


# import packages
library(shiny)
library(shinyBS)
library(shinyjs)
library(DT)
library(leaflet)
library(highcharter)
library(dplyr)
library(tidyr)
library(rgdal)
library(shinycssloaders)


# options
options(shiny.sanitize.errors = FALSE)

hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
options(highcharter.lang = hcoptslang)


# import data
load("data/data.rda")


# import util functions
source("utils.R")


# define global vars
APP_DATA <- app_data

APP_MAP <- app_map

APP_VERSION <- "1.0.4"

COMMUNITY_TYPES <- c("Completely Rural", "Mostly Rural", "Mostly Urban", "Completely Urban")

ICON_EXTERNAL_LINK <- tags$sup(style="color: grey;", icon("external-link", "fa"))

URLS <- list(
  home        = "http://icjia.illinois.gov/",
  cj_dispatch = "http://visitor.r20.constantcontact.com/manage/optin?v=001MqUcqqvjwLCJXlLMSWbTe3zHHmEQgFeBuHvBcJWTbwgrxFbDSGx4HSUPpI6DJWMUPgbljtLxffqIcGFTgCnr-auak88ybvRxpoJlTMGPtZs%3D",
  facebook    = "http://www.facebook.com/ICJIA",
  twitter     = "http://www.twitter.com/ICJIA_Illinois",
  youtube     = "https://www.youtube.com/channel/UCtZMzk8D3P4OixYTwsfPeKA",
  soundcloud  = "https://www.soundcloud.com/icjia",
  github      = "https://github.com/ICJIA"
)
