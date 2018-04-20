# ICJIA Uniform Crime Report Data Dashboard

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

This repository stores R scripts as well as other necessary files for an R Shiny application titled "Uniform Crime Report Data Dashboard."


## About the dashboard

This dashboard offers an interactive way to explore the Crime in Illinois Annual Uniform Crime Report (UCR) data (1982-2015), originally provided by Illinois State Police and prepared by ICJIA. All datasets used in this dashboard are freely available at [the ICJIA website](http://www.icjia.state.il.us/research/overview#tab_research-data). To learn more about the UCR data, read [this article](http://www.icjia.state.il.us/articles/about-uniform-crime-reporting-program-data) by ICJIA staff.

## How to see the dashboard

**Note:** In order to view the dashboard demo, first install [R](https://cran.r-project.org/mirrors.html) and [R Studio](https://www.rstudio.com/products/rstudio/download/). Both R and R Studio are open source software.

Clone or download this repository and open `ui.R` or `server.R` file using R Studio. It may be necessary to install dependencies before running the app. Use `install.packages()` command in R console to install missing packages or copy the following code block:

```r
dependencies <- c("shiny", "shinyjs", "shinycssloaders", "DT", "rgdal", "leaflet", "highcharter", "dplyr", "tidyr")
install.packages(dependencies)
```

Once all packages are installed, you are ready to start the app with the `shiny::runApp()` command, or if you are using RStudio, by clicking the `Run App` button.