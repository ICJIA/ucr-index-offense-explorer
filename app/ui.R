# Author: Bobae Kang (Bobae.Kang@illinois.gov)
# Created: 2017-10-31
# Last revised: 2018-09-14
# Script title: ui.R
#-------------------------------------------------------------------------------


# LOAD MODULES #
#------------------------------------------------------------------------------
source("modules/title_ui.R")
source("modules/inputs_ui.R")
source("modules/download_data_ui.R")
source("modules/app_version_ui.R")
source("modules/references_ui.R")
source("modules/intro_ui.R")
source("modules/kpis_ui.R")
source("modules/plots_ui.R")
source("modules/data_table_ui.R")
source("modules/footer_ui.R")


# DEFINE UI #
#-------------------------------------------------------------------------------
ui <- shinyUI(fluidPage(
  useShinyjs(),
  extendShinyjs(text = "shinyjs.resize = function() { $(window).trigger('resize'); }"),

  tags$head(
    includeCSS(path = "css/style.css"),
    includeHTML("google-analytics.html"),
    tags$title("Uniform Crime Report Data Dashboard")
  ),

  titlePanel(
    title_ui()
  ),

  sidebarLayout(
    sidebarPanel(
      id ="sidebar",
      width = 3,
      input_format_ui(),
      input_category_ui(),
      input_range_ui(),
      input_region_ui(),
      input_community_ui(),
      input_county_ui(),
      download_data_ui(),
      br(),
      app_version_ui(),
      references_ui()
    ),

    mainPanel(
      id = "main",
      width = 9,
      intro_ui(),
      tags$hr(style = "border-color: #ddd;"),
      kpis_ui(),
      plots_ui(),
      data_table_ui()
    )
  ),
  
  footer_ui()
))
