# Uniform Crime Report Data Explorer

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

> WARNING: 1.0.x version is deprecated and no longer maintained.

This repository stores R scripts as well as other necessary files for an R Shiny application titled "Uniform Crime Report Data Explorer."

## About this application

Uniform Crime Report Data Explorer offers an interactive interface to the _Crime in Illinois Annual Uniform Crime Report (UCR)_ data (1982-2016), originally provided by Illinois State Police and prepared by ICJIA. All datasets used in this dashboard are freely available at [the ICJIA website](http://www.icjia.state.il.us/research/overview#tab_research-data). To learn more about the UCR data, read [this article](http://www.icjia.state.il.us/articles/about-uniform-crime-reporting-program-data) by ICJIA staff.

## How to try the application

**The application is now deployed and available [here](http://app.icjia.cloud/app/ucr-data-explorer).**

In order to try the application on your local machine, you first want to install [R](https://cran.r-project.org/mirrors.html) and [R Studio IDE](https://www.rstudio.com/products/rstudio/download/). Both R and R Studio are free and open source.

Clone or download this repository and open `ui.R` or `server.R` file using R Studio IDE. It may be necessary to install dependencies before running the app. Use `install.packages()` command in R console to install missing packages or copy the following code block:

```r
dependencies <- c("shiny", "shinyBS", "shinyjs", "shinycssloaders", "DT", "rgdal", "leaflet", "highcharter", "dplyr", "tidyr")
install.packages(dependencies)
```

Once all packages are installed, you are ready to start the application with the `shiny::runApp()` command, or if you are using RStudio, by clicking the `Run App` button.
