library(raster)
library(lubridate)
library(tidyverse)
library(parallel)
library(pbapply)
library(here)
library(sf)
library(ncdf4)

project_dir = here()

raw_data_dir <- file.path(project_dir, "data", "raw")

cb_2020_us_20m_dir <- file.path(raw_data_dir, "cb_2020_us_nation_20m")
cb_2020_us_20m_file <- file.path(cb_2020_us_20m_dir, "cb_2020_us_nation_20m.zip")

if (!dir.exists(cb_2020_us_20m_dir)) {
  dir.create(cb_2020_us_20m_dir)
  download.file("http://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_us_nation_20m.zip",
                destfile = cb_2020_us_20m_file)
  unzip(cb_2020_us_20m_file,
        exdir = cb_2020_us_20m_dir)
}

usa_shp <- st_read(file.path(cb_2020_us_20m_dir, "cb_2020_us_nation_20m.shp"))
usa_shp_transformed = st_transform(usa_shp, crs = "EPSG:4326")
usa_shp_transformed <- as_Spatial(usa_shp_transformed$geom)

summarize_by_month <- function(url, mask_shp) {
  library(raster)
  library(lubridate)
  library(tidyverse)
  library(parallel)
  library(pbapply)
  library(here)
  library(sf)
  library(ncdf4)

  project_dir = here()

  # takes a climate data url and a masking spatial polygon as input
  # and makes a monthly summary of the climate data, masked by the polygon
  # pull the raw climate data file down locally
  print(url)
  
  # determine which climate variable, and which year we have
  file <- basename(url)
  file_split <- unlist(strsplit(basename(file), split = "_"))
  var <- file_split[1]

  if (var == "rmin") {
    var <- "rhsmin"
  }
  if (var == "tmmx") {
    var <- "tasmax"
  }
  if (var == "vs") {
    var <- "was"
  }

  year <- substr(file_split[2], start = 1, stop = 4)

  # generate an output filename
  var_dir <- file.path(project_dir, "data", "processed", "climate-data", var)
  dir.create(var_dir, showWarnings=FALSE, recursive=TRUE)

  nc_name <- gsub(file,
                  pattern = basename(file),
                  replacement = paste0("monthly_", basename(file)))
  out_path <-  file.path(var_dir, gsub(x = nc_name, pattern = ".nc", replacement = ".tif"))

  file = file.path(var_dir, file)

  # pass over file if necessary
  if (file.exists(out_path)) {
    print("file already exists")
    return(paste("File", out_path, "already exists"))
  }
  if (as.numeric(year) < 1983) {
    return("Year outside range of consideration")
  }

  # if we haven't already exited, download the original data file
  if (!file.exists(file)) {
    download.file(url = url, destfile = file)
  }
  
  # determine which function to use to aggregate
  if (var == "pr") {
    fun <- sum
  } else {
    fun <- mean
  }

  # generate monthly summary
  raster <- raster::stack(file)
  start_date <- as.Date(paste(year, "01", "01", sep = "-"))
  end_date <- as.Date(paste(year, "12", "31", sep = "-"))
  date_seq <- seq(start_date, end_date, by = "1 day")
  date_seq <- date_seq[1:raster::nlayers(raster)]
  month_seq <- lubridate::month(date_seq)
  res <- raster::stackApply(raster, month_seq, fun = fun)
  if (extent(res)@xmin > 0 && extent(res)@xmax > 0) {
    extent(res) <- extent(res) - c(360, 360, 0, 0)
  }
  names(res) <- paste(var, year,
                      unique(lubridate::month(date_seq, label = TRUE)),
                      sep = "_")
  proj_res <- projectRaster(res, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  masked_res <- raster::mask(proj_res, mask_shp)
  raster::writeRaster(masked_res, out_path, format = "GTiff")
  unlink(file)
  return(paste("File", out_path, "written"))
}

climate_data_urls <- read.csv(file.path(raw_data_dir, "climate-data.csv"),
                              stringsAsFactors = FALSE)


# Summarize daily climate data by month ---------------------------
print('Aggregating daily climate data to monthly means. May take a while...')
pboptions(type = 'txt', use_lb = TRUE)
cl <- makeCluster(getOption("cl.cores", detectCores() / 2))
pblapply(X = climate_data_urls$url, 
         FUN = summarize_by_month, 
         mask_shp = usa_shp_transformed, 
         cl = cl)
stopCluster(cl)

print('Daily climate data aggregated to monthly summaries.')
