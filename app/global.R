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


# rural labels
rural_labels <- c("Completely Rural", "Mostly Rural", "Mostly Urban", "Completely Urban")