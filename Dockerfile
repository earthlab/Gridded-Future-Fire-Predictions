FROM rocker/rstudio:4.3.2

RUN apt update && \
    apt install -y sudo curl && \
    apt clean && \
    rm -rf /var/lib/apt/lists/*

# # Build RStudio Geospatial
RUN apt-get update && \
    apt install -y \
    gdal-bin \
    lbzip2 \
    libfftw3-dev \
    libgdal-dev \
    libgeos-dev \
    libgsl0-dev \
    libgl1-mesa-dev \
    libglu1-mesa-dev \
    libhdf4-alt-dev \
    libhdf5-dev \
    libjq-dev \
    libpq-dev \
    libproj-dev \
    libprotobuf-dev \
    libnetcdf-dev \
    libsqlite3-dev \
    libudunits2-dev \
    lsb-release \
    netcdf-bin \
    postgis \
    protobuf-compiler \
    sqlite3 \
    tk-dev \
    libfmt-dev \
    unixodbc-dev && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

RUN apt-get update && apt-get install --yes screen vim

RUN R -e "install.packages(c('here', 'brms', 'raster', 'terra', 'stars', 'rcpp', 'patchwork', 'purrr', 'ggplot2', 'matrix', 'zoo', 'sp', 'abind', 'here', 'forcats', 'readr', 'lubridate', 'tidyverse', 'spdep', 'ncdf4', 'rstan', 'reshape2', 'sf', 'stringr', 'tidyr', 'spdata', 'pbapply', 'StanHeaders', 'assertthat', 'dplyr', 'tibble'))"

WORKDIR /home/rstudio

COPY . . 
RUN rm -rf .git .gitignore 

RUN chown -R rstudio /home/rstudio

# Add EPAs cert to the CA Bundle
COPY ./epa.gov.cer .
RUN cat ./epa.gov.cer >> /usr/local/lib/R/site-library/openssl/cacert.pem
RUN cat ./epa.gov.cer >> /usr/lib/ssl/certs/ca-certificates.crt

# This is where we can control which root permissions the jovyan user will have
ARG PRIV_CMDS='/bin/ch*,/bin/cat,/bin/gunzip,/bin/tar,/bin/mkdir,/bin/ps,/bin/mv,/bin/cp,/usr/bin/apt*,/usr/bin/pip*,/bin/yum,/usr/lib/ssl'

RUN usermod -aG sudo rstudio && \
    echo "rstudio ALL=NOPASSWD: $PRIV_CMDS" >> /etc/sudoers
RUN usermod -aG rstudio rstudio
ENV PASSWORD=password