# ICJIA Dashboard Demo

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

This repository stores R scripts as well as other necessary files for an R Shiny app dashboard created as a demo project by an ICJIA Research Analyst. 

(Develop)


## About the dashboard

The dashboard offers an interactive way to explore the latest Crime in Illinois Annual Uniform Crime Report data (2011-2015). All data sets used for this dashboard are freely and publicly available at [the Illinois State Police website](http://www.isp.state.il.us/crime/ucrhome.cfm). **This dashboard is only a demo project and should NOT be considered as ICJIA's official publication.**

## How to see the dashboard

**Note:** In order to view the dashboard demo, first install [R](https://cran.r-project.org/mirrors.html) and [R Studio](https://www.rstudio.com/products/rstudio/download/). Both R and R Studio are open source software.

Clone or download this repository and open `ui.R` or `server.R` file using R Studio. Click `Run App` button on the top right of the Script Pane to preview this demo Shiny app on your local server. It may be necessary to install dependencies before running the app. Use `install.packages()` command in R console to install missing packages or copy the following code block:

```r
dependencies <- c("shiny", "shinydashboard", "DT", "rgdal", "leaflet", "highcharter", "reader", "dplyr", "tidyr")
install.packages(dependencies)
```

Once all packages are installed, you are ready to start the app with the `shiny::runApp()` command, or if you are using RStudio, by clicking the `Run App` button.