FROM rocker/shiny:latest

# Install system dependencies required for R packages
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    libv8-dev \
    libsodium-dev \
    libcairo2-dev \
    libxt-dev \
    libglpk-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages required by MRDviz
RUN R -e "install.packages(c(\
    'shiny', \
    'shinyjs', \
    'highcharter', \
    'memoise', \
    'survival', \
    'jsonlite', \
    'DT', \
    'RColorBrewer', \
    'shinydashboard', \
    'ggplot2', \
    'tidyr', \
    'purrr', \
    'simsurv', \
    'data.table', \
    'dplyr', \
    'openxlsx', \
    'survminer', \
    'devtools' \
))"

# Create directory for the app
RUN mkdir -p /srv/shiny-server/MRDviz

# Copy the MRDviz package into the container
COPY . /tmp/MRDviz

# Install the MRDviz package from source
RUN R -e "devtools::install('/tmp/MRDviz', upgrade = 'never')"

# Create an app.R file that launches MRDviz
RUN echo "library(MRDviz); run_mrdviz()" > /srv/shiny-server/MRDviz/app.R

# Create a simple index.html that redirects to the MRDviz app
RUN echo '<meta http-equiv="refresh" content="0; url=/MRDviz/" />' > /srv/shiny-server/index.html

# Copy custom shiny-server configuration
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

# Create necessary directories for logs and bookmarks
RUN mkdir -p /var/log/shiny-server/MRDviz \
    && mkdir -p /var/lib/shiny-server/bookmarks \
    && chmod -R 777 /var/log/shiny-server \
    && chmod -R 777 /var/lib/shiny-server

# Configure permissions
RUN chown -R shiny:shiny /srv/shiny-server

# Expose the application port
EXPOSE 3838

# Command to run when the container starts
CMD ["/usr/bin/shiny-server"]