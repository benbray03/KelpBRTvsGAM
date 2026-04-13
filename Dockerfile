FROM rocker/shiny:latest

# system dependencies
RUN apt-get update && apt-get install -y \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# R packages
RUN R -e "install.packages(c('shiny','dplyr','readr','ggplot2','leaflet','sf','viridisLite','scales','shinydashboard'), repos='https://cran.rstudio.com/')"

# copy app files
COPY app.R /srv/shiny-server/
COPY brt_projections_by_rock/ /srv/shiny-server/brt_projections_by_rock/
COPY shp/ /srv/shiny-server/shp/

# Hugging Face permissions
RUN chown -R 1000:1000 /srv/shiny-server/ \
    && chmod -R 755 /srv/shiny-server/

EXPOSE 7860

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/app.R', host='0.0.0.0', port=7860)"]