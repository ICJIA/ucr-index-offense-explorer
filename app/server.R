# Author: Bobae Kang (Bobae.Kang@illinois.gov)
# Created: 2017-10-31
# Last revised: 2018-09-14
# Script title: server.R
#-------------------------------------------------------------------------------


# LOAD MODULES #
#-------------------------------------------------------------------------------
source("modules/toggle_sidebar.R")
source("modules/filter_inputs.R")
source("modules/filter_by_region.R")
source("modules/filter_by_community_type.R")
source("modules/download_data.R")
source("modules/current_area.R")
source("modules/kpis.R")
source("modules/plot_line.R")
source("modules/plot_bar.R")
source("modules/plot_map.R")
source("modules/data_table.R")


# DEFINE SERVER LOGIC #
#-------------------------------------------------------------------------------
server <- function (input, output, session) {
  # reactive vals
  data_by_area <- reactive({
    filter_area(APP_DATA, input)
  })
  data_by_area_range <- reactive({
    filter_range(data_by_area(), input)
  })
  data_by_area_lastest_year <- reactive({
    filter_latest_year(data_by_area(), input)
  })
  data_by_area_unit <- reactive({
    select_data_unit(data_by_area(), input, violent_crime, property_crime)
  })

  # title panel
  toggle_sidebar(input)

  # sidebar panel
  filter_inputs(input, session)
  filter_by_region(input, session, data_update)
  filter_by_community_type(input, session, data_update)

  download_data(output, data = data_by_area_range())

  # main panel
  current_area(input, output)

  kpi_1(input, output, data_by_area_lastest_year)
  kpi_2(input, output, data_by_area_unit)
  kpi_3(input, output, data_by_area_unit)

  plot_line(input, output, data_by_area_range)
  plot_bar(input, output, data_by_area_lastest_year)
  plot_map(input, output)

  data_table(input, output, data_by_area_range)
}
