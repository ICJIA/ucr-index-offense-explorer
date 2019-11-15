# import packages
library(shiny)
library(shinyBS)
library(shinyjs)
library(DT)
library(leaflet)
library(highcharter)
library(dplyr)
library(tidyr)
library(shinycssloaders)


# options
options(shiny.sanitize.errors = FALSE)

hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
options(highcharter.lang = hcoptslang)


# import global constants
source("consts.R")


# import util functions
source("utils.R")
