# Uniform Crime Report Index Offense Explorer

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

This repository stores R scripts as well as other necessary files for an R Shiny application titled "Uniform Crime Report Index Offense Explorer."

## About this application

Uniform Crime Report (UCR) Index Offense Explorer offers an interactive interface to the _Crime in Illinois Annual Uniform Crime Report_ Index Crime offense data, originally provided by Illinois State Police and prepared by ICJIA. All datasets used in this dashboard are freely available at [ICJIA Research Hub](http://icjia.illinois.gov/researchhub/datasets). To learn more about the UCR data, read [this article](http://icjia.illinois.gov/researchhub/articles/about-uniform-crime-reporting-program-data) by ICJIA staff.

## How to try the application

**The application is now deployed and available [here](http://app.icjia.cloud/app/ucr-index-offense-explorer).**

In order to try the application on your local machine, you first want to install [R](https://cran.r-project.org/mirrors.html) and [R Studio IDE](https://www.rstudio.com/products/rstudio/download/). Both R and R Studio are free and open source.

Clone or download this repository and open `global.R`, `ui.R` or `server.R` file using R Studio IDE. It may be necessary to install dependencies before running the app. Use `install.packages()` command in R console to install missing packages or copy the following code block:

```r
dependencies <- c("shiny", "shinyBS", "shinyjs", "shinycssloaders", "DT", "leaflet", "highcharter", "dplyr", "tidyr")
install.packages(dependencies)
```

Once all packages are installed, you are ready to start the application with the `shiny::runApp()` command, or if you are using RStudio, by clicking the `Run App` button.

## License

[MIT](http://opensource.org/licenses/MIT)

Copyright (c) 2017 Illinois Criminal Justice Information Authority
