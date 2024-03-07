library(raster)
library(lubridate)
library(sf)
library(tidyverse)
library(parallel)
library(pbapply)
library(ncdf4)
library(here)


project_dir <- here()
raw_data_dir = file.path(project_dir, "data", "raw")
processed_data_dir = file.path(project_dir, "data", "processed")

shapefile_path <- file.path(raw_data_dir, "cb_2020_us_nation_20m", "cb_2020_us_nation_20m.shp")

usa_shp <- st_read(shapefile_path)
usa_shp_transformed = st_transform(usa_shp, crs = "EPSG:4326")
usa_shp_transformed <- as_Spatial(usa_shp_transformed$geom)

yearly_tiff <- function(url, mask_shp) {
  url <- trimws(url)
    
  # determine which model, climate variable, and which years we have
  file_split <- unlist(strsplit(basename(url), split = "_"))
  model <- file_split[3]
  var <- file_split[2]
  year_start <- substr(file_split[6], start = 1, stop = 4)
  year_end <- substr(file_split[7], start=1, stop = 4)

  var_dir <- file.path(processed_data_dir, "climate-data", var)
  dir.create(var_dir, showWarnings = FALSE, recursive=TRUE)

  Tiffnames=list()
  i <- 1
  for (yearst in year_start:year_end)
  {
   Tiffnames[i]<- file.path(var_dir, paste(model,var, get("yearst"),sep="_"))
   i <- i + 1
  }

  print(Tiffnames)

  files_written <- TRUE
  for (tiff_file in Tiffnames) {
    if (!file.exists(paste0(tiff_file, ".tif"))) {
      print(tiff_file)
      files_written <- FALSE
    }
  }

  if (files_written) {
    return("Files already written")
  }

  file <- file.path(var_dir, basename(url))
  print(file)
  # pass over file if necessary
  if (!file.exists(file)) {
    # if we haven't already exited, download the original data file
    download.file(url = url, destfile = file, mode = "wb")
  }
  
  # generate yearly files from 5 yr aggregated data
  raster <- raster::stack(file)
  start_date <- as.Date(paste(year_start, "01", "01", sep = "-"))
  end_date <- as.Date(paste(year_end, "12", "31", sep = "-"))
  date_seq <- seq(start_date, end_date, by = "1 month")
  date_seq <- date_seq[1:raster::nlayers(raster)]
  month_seq <- lubridate::month(date_seq)
  fdate = factor(format(date_seq,'%Y'))
  
  if (extent(raster)@xmin > 0 && extent(raster)@xmax > 0) {
    extent(raster) <- extent(raster) - c(360, 360, 0, 0)
  }
  proj_res <- projectRaster(raster, crs = "EPSG:4326")
  masked_ras <- raster::mask(proj_res, mask_shp)
  
   yrlyList=list()
   yearst= fdate[1]
   i <- 1
   for (yearst in year_start:year_end)
  {
   yrly <- subset(masked_ras,  grep(get("yearst"), names(masked_ras))) # subset based on year
   names(yrly) <- paste(var, get("yearst"),
                      unique(lubridate::month(date_seq, label = TRUE)),
                      sep = "_")
   yrlyList[i] <- yrly
   i <- i+1
  }
  
  out_names<-unlist(Tiffnames, use.names=FALSE)
  print(yrlyList)
  mapply(writeRaster, yrlyList, out_names, 'GTiff', overwrite=TRUE) 

  unlink(file)
  return(paste("Files", out_names, "written"))
  
}

for (file in list.files(raw_data_dir, pattern="ClimateRCP*")) {
  climate_data_urls <- read.csv(file.path(raw_data_dir, file), stringsAsFactors = FALSE)

  pboptions(type = 'txt', use_lb = TRUE)
  pblapply(X = climate_data_urls$url, 
          FUN = yearly_tiff, 
          mask_shp = usa_shp_transformed)

  print('Aggregated Monthly climate data split into yearly files.')

}

