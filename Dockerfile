FROM jupyter/r-notebook:lab-4.3.1

COPY . .

USER root
RUN rm -rf .git/
RUN chown -R jovyan .

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

USER ${NB_UID}

RUN mamba install \
    libjpeg-turbo==3.0.0 \
    gdal==3.8.4 \
    geos==3.12.1 \
    proj==9.3.1 \
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
    r-assertthat==0.2.1 \
    r-dplyr==1.1.4 \
    r-tibble==3.2.1 \
    r-tidyselect=1.2.0 \
    r-loo=2.7.0 \ 
    r-TH.data=1.1-2 \
    r-tensorA=0.36.2.1 \
    r-timechange=0.3.0 \
    r-lifecycle=1.0.4 \
    r-LearnBayes=2.15.1 \
    r-survival=3.5-7 \
    r-magrittr=2.0.3 \ 
    r-posterior=1.5.0 \
    r-compiler=4.3.2 \
    r-rlang=1.1.3 \
    r-tools=4.3.2 \
    r-utf8=1.2.4 \
    r-bridgesampling=1.1-2 \
    r-pkgbuild=1.4.3 \
    r-classInt=0.4-10 \
    r-plyr=1.8.9 \
    r-multcomp=1.4-25 \
    r-KernSmooth=2.23-22 \
    r-withr=3.0.0 \ 
    r-grid=4.3.2 \
    r-stats4=4.3.2 \
    r-fansi=1.0.6 \
    r-e1071=1.7-14 \
    r-colorspace=2.1-0 \
    r-inline=0.3.19 \
    r-MASS=7.3-60 \
    r-scales=1.3.0 \
    r-cli=3.6.2 \
    r-mvtnorm=1.2-4 \
    r-generics=0.1.3 \
    r-RcppParallel=5.1.7 \
    r-tzdb=0.4.0 \
    r-DBI=1.2.2 \
    r-proxy=0.4-27 r-bayesplot=1.11.1 r-s2=1.1.6 r-matrixStats=1.2.0 r-vctrs=0.6.5 r-sandwich=3.1-0 r-boot=1.3-28.1 r-jsonlite=1.8.8 r-hms=1.1.3 r-units=0.8-5 r-glue=1.7.0 r-codetools=0.2-19 r-distributional=0.4.0 r-stringi=1.8.3 r-gtable=0.3.4 r-deldir=2.0-4 r-QuickJSR=1.1.3 r-munsell=0.5.0 r-pillar=1.9.0 r-Brobdingnag=1.2-9 r-R6=2.5.1 r-wk=0.9.1 r-rprojroot=2.0.4 r-lattice=0.21-9 r-backports=1.4.1 r-rstantools=2.4.0 r-class=7.3-22 r-coda=0.19-4.1 r-gridExtra=2.3 r-nlme=3.1-163 r-checkmate=2.3.1 r-pkgconfig=2.0.3

RUN R -e 'install.packages("brms", repos="https://cloud.r-project.org", version="2.21.0")'
