FROM jupyter/r-notebook:lab-3.5.3

COPY . .

USER root
RUN rm -rf .git/
RUN chmod -R a+w *

# Add sudo to jovyan user. 
RUN apt update && \
    apt install -y sudo && \
    apt clean && \
    rm -rf /var/lib/apt/lists/*

ARG LOCAL_USER=jovyan

# This is where we can control which root permissions the jovyan user will have
ARG PRIV_CMDS='/bin/ch*,/bin/cat,/bin/gunzip,/bin/tar,/bin/mkdir,/bin/ps,/bin/mv,/bin/cp,/usr/bin/apt*,/usr/bin/pip*,/bin/yum',/opt

RUN usermod -aG sudo jovyan && \
    echo "$LOCAL_USER ALL=NOPASSWD: $PRIV_CMDS" >> /etc/sudoers
RUN addgroup jovyan
RUN usermod -aG jovyan jovyan

# Build RStudio Geospatial
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

USER ${NB_UID}

RUN mamba install \
    r-terra==1.7-71 \
    r-stars==0.6-5 \
    r-rcpp==1.0.12 \
    r-patchwork==1.2.0 \
	r-purrr==1.0.2 \
    r-ggplot2==3.5.0 \
    r-matrix==1.6-1.1 \
    r-zoo==1.8-12 \
    r-sp==2.1-3 \
	r-abind==1.4-5 \
    r-here==1.0.1 \
    r-forcats==1.0.0 \
    r-readr==2.1.5 \
	r-lubridate==1.9.3 \ 
	r-tidyverse==2.0.0 \
    r-spdep==1.3-3 \
    r-ncdf4==1.22 \
    r-rstan==2.32.6 \
    r-reshape2==1.4.4 \
    r-sf==1.0-15 \
    r-stringr==1.5.1 \
    r-tidyr==1.3.1 \
    r-spdata==2.3.0 \
    r-pbapply==1.7-2 \
    r-StanHeaders==2.32.6 \
	r-brms==2.21.0 \
    r-assertthat_0.2.1 \
    r-dplyr==1.1.4 \
    r-tibble==3.2.1
