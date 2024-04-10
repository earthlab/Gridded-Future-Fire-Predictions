FROM rocker/rstudio:latest

# Add sudo to jovyan user. 
RUN apt update && \
    apt install -y sudo && \
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

RUN R -e "install.packages(c('terra==1.7-71', 'stars==0.6-5', 'rcpp==1.0.12', 'patchwork==1.2.0', 'purrr==1.0.2', 'ggplot2==3.5.0', 'matrix==1.6-1.1', 'zoo==1.8-12', 'sp==2.1-3', 'abind==1.4-5', 'here==1.0.1', 'forcats==1.0.0', 'readr==2.1.5', 'lubridate==1.9.3', 'tidyverse==2.0.0', 'spdep==1.3-3', 'ncdf4==1.22', 'rstan==2.32.6', 'reshape2==1.4.4', 'sf==1.0-15', 'stringr==1.5.1', 'tidyr==1.3.1', 'spdata==2.3.0', 'pbapply==1.7-2', 'StanHeaders==2.32.6', 'assertthat==0.2.1', 'dplyr==1.1.4', 'tibble==3.2.1'))"
RUN R -e 'install.packages("brms", repos="https://cloud.r-project.org", version="2.21.0")'

COPY . . 
RUN -rf .git .gitignore 

RUN adduser rstudio && chown -R rstudio /home/rstudio

# This is where we can control which root permissions the jovyan user will have
ARG PRIV_CMDS='/bin/ch*,/bin/cat,/bin/gunzip,/bin/tar,/bin/mkdir,/bin/ps,/bin/mv,/bin/cp,/usr/bin/apt*,/usr/bin/pip*,/bin/yum',/opt

RUN usermod -aG sudo rstudio && \
    echo "$LOCAL_USER ALL=NOPASSWD: $PRIV_CMDS" >> /etc/sudoers
RUN addgroup rstudio
RUN usermod -aG rstudio rstudio