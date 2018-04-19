FROM openanalytics/r-base

MAINTAINER Bobae Kang "Bobae.Kang@illinois.gov"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.0.0

# system library dependency for the app
RUN apt-get update && apt-get install -y \
    libmpfr-dev

# install R packages for basic shiny functionality
RUN R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://cloud.r-project.org/')"

# install igraph, rgdal requirement
RUN apt-get update && apt-get install -y \
    libxml2-dev \
    libgdal-dev \
    libproj-dev

# install R packages specfic to the app
RUN R -e "install.packages(c('shinyjs', 'shinycssloaders', 'DT', 'rgdal', 'leaflet', 'highcharter', 'dplyr', 'tidyr'), repos='https://cloud.r-project.org/')"

# copy the app to the image
RUN mkdir /root/app
COPY app /root/app

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/app')"]
