library(raster)
library(parallel)
library(pbapply)
library(purrr)
library(tidyverse)
library(sf)
library(sp)
library(here)

options(timeout=1000)

project_dir <- here()
raw_data_dir <- file.path(project_dir, "data", "raw")
processed_data_dir <- file.path(project_dir, "data", "processed")

if (!dir.exists(processed_data_dir)) {
  dir.create(processed_data_dir)
}

ecoregions_l4_dir =  file.path(raw_data_dir, "ecoregions_L4")
ecoregions_l4_zip = file.path(raw_data_dir, "ecoregions_L4.zip")

### Download ecoregion shape files
if (!dir.exists(ecoregions_l4_dir)) {
  dir.create(ecoregions_l4_dir, recursive=TRUE)
  download.file("https://gaftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/us_eco_l4.zip",
                destfile = ecoregions_l4_zip
                )
  unzip(ecoregions_l4_zip,
        exdir = ecoregions_l4_dir)
}

iclus_pop_dir = file.path(raw_data_dir, "ICLUS_pop")
iclus_pop_file = file.path(iclus_pop_dir, "ICLUS_pop.zip")
### Download Population data (past census and projected future)
if (!dir.exists(iclus_pop_dir)) {
  dir.create(iclus_pop_dir)
  download.file("https://gaftp.epa.gov/EPADataCommons/ORD/NCEA/ICLUS_v2.1.1/population/ICLUS_v2_1_1_population.zip",
                destfile = iclus_pop_file)
   unzip (iclus_pop_file,
          exdir = iclus_pop_dir)
}

# Extracting monthly population summaries for L4 ecoregions ---------------------
ecoregion_shp <- st_make_valid(st_read(file.path(raw_data_dir, 'ecoregions_L4', 'us_eco_l4_no_st.shp')))
ecoregion_shp <- st_transform(ecoregion_shp, crs = "EPSG:4326")

ICLUS_pop <- st_make_valid(st_read(file.path(iclus_pop_dir, "ICLUS_v2_1_1_population.gdb")))
ICLUS_shp <- st_transform(ICLUS_pop, crs = "EPSG:4326")

ICLUS_90 <- aggregate(x = ICLUS_shp["TOTALPOP90"], by = ecoregion_shp, FUN = mean)
ICLUS_00 <- aggregate(x = ICLUS_shp["TOTALPOP00"], by = ecoregion_shp, FUN = mean)
ICLUS_10 <- aggregate(x = ICLUS_shp["TOTALPOP10"], by = ecoregion_shp, FUN = mean)
ICLUS_20 <- aggregate(x = ICLUS_shp["SSP22020"], by = ecoregion_shp, FUN = mean)
ICLUS_30 <- aggregate(x = ICLUS_shp["SSP22030"], by = ecoregion_shp, FUN = mean)
ICLUS_40 <- aggregate(x = ICLUS_shp["SSP22040"], by = ecoregion_shp, FUN = mean)
ICLUS_50 <- aggregate(x = ICLUS_shp["SSP22050"], by = ecoregion_shp, FUN = mean)
ICLUS_60 <- aggregate(x = ICLUS_shp["SSP22060"], by = ecoregion_shp, FUN = mean)
ICLUS_70 <- aggregate(x = ICLUS_shp["SSP22070"], by = ecoregion_shp, FUN = mean)

den90<-ICLUS_90$TOTALPOP90
den00<-ICLUS_00$TOTALPOP00
den10<-ICLUS_10$TOTALPOP10
den20<-ICLUS_20$SSP22020
den30<-ICLUS_30$SSP22030
den40<-ICLUS_40$SSP22040
den50<-ICLUS_50$SSP22050
den60<-ICLUS_60$SSP22060
den70<-ICLUS_70$SSP22070

extractions<- data.frame(den90,den00,den10,den20,den30,den40,den50,den60,den70)

extraction_df <- extractions %>%
  as_tibble %>%
  mutate(US_L4NAME = data.frame(ecoregion_shp)$US_L4NAME,
         Shape_Area = data.frame(ecoregion_shp)$Shape_Area) %>%
  dplyr::select(-starts_with('X')) %>%
  gather(variable, value, -US_L4NAME, -Shape_Area) %>%
  filter(!is.na(value)) %>%
  mutate(year = case_when(
    .$variable == 'den90' ~ 1990,
    .$variable == 'den00' ~ 2000,
    .$variable == 'den10' ~ 2010,
    .$variable == 'den20' ~ 2020,
    .$variable == 'den30' ~ 2030,
    .$variable == 'den40' ~ 2040,
    .$variable == 'den50' ~ 2050,
    .$variable == 'den60' ~ 2060,
    .$variable == 'den70' ~ 2070,
  ),
  US_L4NAME = as.character(US_L4NAME)) %>%
  group_by(US_L4NAME, year) %>%
  summarize(wmean = weighted.mean(value, Shape_Area)) %>%
  ungroup

# Then interpolate for each month and year from 2030-2060
# using a simple linear sequence
impute_density <- function(df) {
  year_seq <- min(df$year):(max(df$year)+10)
  predict_seq <- seq(min(df$year),
                     (max(df$year)+10),
                     length.out = (length(year_seq) - 1) * 12)
  preds <- approx(x = df$year,
                  y = df$wmean,
                  xout = predict_seq)
  res <- as_tibble(preds) %>%
    rename(t = x, wmean = y) %>%
    mutate(year = floor(t),
           month = rep(1:12, times = length(year_seq) - 1)) %>%
    filter(year < 2061)
  res$US_L4NAME <- unique(df$US_L4NAME)
  res
}

res <- extraction_df %>%
  split(.$US_L4NAME) %>%
  map(~impute_density(.)) %>%
  bind_rows %>%
  rename(housing_density = wmean)

out_file <- file.path(processed_data_dir, 'population_density.csv')

res %>%
  write_csv(out_file)
