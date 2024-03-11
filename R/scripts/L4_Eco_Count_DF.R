
## CREATE COUNT_DF by merging data
library(tidyverse)
library(sf)
library(zoo)
library(assertthat)
library(lubridate)
library(sp)
library(purrr)
library(dplyr)
library(here)

project_dir = here()
raw_data_dir = file.path(project_dir, "data", "raw")
processed_data_dir = file.path(project_dir, "data", "processed")

# Read ecoregion data
ecoregion_shp_sf <- st_make_valid(st_read(file.path(raw_data_dir, 'ecoregions_L4', 'us_eco_l4_no_st.shp')))
ecoregions <- st_transform(ecoregion_shp_sf, crs = "EPSG:4326")
ecoregions$US_L4NAME <- as.character(ecoregion_shp_sf$US_L4NAME)

# fix names for chihuahuan desert
ecoregions <- ecoregions %>%
  mutate(US_L4NAME = as.character(US_L4NAME),
         US_L4NAME = ifelse(US_L4NAME == 'Chihuahuan Desert',
                            'Chihuahuan Deserts',
                            US_L4NAME))


# Read fire data ----------------------
fire_data_path <- file.path(raw_data_dir, "mtbs", "mtbs_FODpoints_DD.shp")
fire_data_zip_path <- file.path(raw_data_dir, "mtbs.zip")

if (!file.exists(fire_data_path)) {
  download.file("https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/MTBS_Fire/data/composite_data/fod_pt_shapefile/mtbs_fod_pts_data.zip", destfile = fire_data_zip_path)
  unzip(fire_data_zip_path, exdir = file.path(raw_data_dir, "mtbs"))
}

mtbs <- st_make_valid(st_read(fire_data_path)) %>%
    st_transform(st_crs(ecoregions)) %>%
    filter(Incid_Type == "Wildfire", 
          BurnBndAc > 1e3) %>%
    mutate(year = year(Ig_Date), 
          month = month(Ig_Date))

# match each ignition to an ecoregion
ov_path <- file.path(processed_data_dir, "ov.rds")
if (!file.exists(ov_path)) {
  st_over <- function(x, y) {
    sapply(st_intersects(x,y), function(z) if (length(z)==0) NA_integer_ else z[1])
  }
  ov <- st_over(mtbs, ecoregions)
  write_rds(ov, ov_path)
}
ov <- read_rds(ov_path)

mtbs <- mtbs %>%
  mutate(US_L4NAME = ecoregions$US_L4NAME[ov]) %>%
  filter(!is.na(US_L4NAME))

unique_er_yms <- expand.grid(
  US_L4NAME = unique(ecoregions$US_L4NAME),
  year = unique(mtbs$year),
  month = unique(mtbs$month)
) %>%
  as_tibble

# count the number of fires in each ecoregion in each month
count_df <- mtbs %>%
  #tbl_df %>%
  as_tibble %>%
  dplyr::select(-geometry) %>%
  group_by(US_L4NAME, year, month) %>%
  summarize(n_fire = n()) %>%
  ungroup %>%
  full_join(unique_er_yms) %>%
  mutate(n_fire = ifelse(is.na(n_fire), 0, n_fire),
         ym = as.yearmon(paste(year, sprintf("%02d", month), sep = "-"))) %>%
  arrange(ym,US_L4NAME) %>%
  mutate(er_ym = paste(US_L4NAME, ym, sep = "_"))
  
assert_that(0 == sum(is.na(count_df$US_L4NAME)))
assert_that(sum(count_df$n_fire) == nrow(mtbs))
assert_that(all(ecoregions$US_L4NAME %in% count_df$US_L4NAME))

generate_counts <- function(model_name) {

  out_file <- file.path(processed_data_dir, paste0(model_name,  "_Count_df.csv"))

  if (file.exists(out_file)) {
    return ("File exists")
  }

  print(model_name)

  # load covariate data and link to count data frame
  MACA_sum <- read_csv(file.path(processed_data_dir, paste0(model_name, '_ecoregion_summaries.csv'))) %>%
    filter(year > 2019 & year < 2061)

  MET_eco_summaries <- read_csv(file.path(processed_data_dir, 'MET_ecoregion_summaries.csv')) %>%
    filter(year < 2020)

  ecoregion_summaries <- full_join(MET_eco_summaries, MACA_sum)%>%
    group_by(US_L4NAME, year, month) %>%
    mutate(ym = as.yearmon(paste(year, sprintf("%02d", month), sep = "-"))) %>%
    spread(variable, value) %>%
    arrange(US_L4NAME, ym)

  lagged_precip_path <- file.path(processed_data_dir, paste0(model_name, '_lagged_precip.rds'))

  # Compute previous 12 months total precip
  ecoregion_summaries$prev_12mo_precip <- NA
  pb <- txtProgressBar(max = nrow(ecoregion_summaries), style = 3)
  current_eco_region <- NULL
  ecoregion_start_index <- NULL
  for (i in 1:nrow(ecoregion_summaries)) {
    setTxtProgressBar(pb, i)
    if (ecoregion_summaries$year[i] > 1983) {
      row_eco_region <- ecoregion_summaries$US_L4NAME[[i]]
      if (is.null(current_eco_region) || row_eco_region != current_eco_region) {
        current_eco_region <- row_eco_region
        ecoregion_start_index <- i
      }
      start_index <- max(ecoregion_start_index, i - 12)
      ecoregion_summaries$prev_12mo_precip[i] <- sum(as.numeric(gsub("\"", "", ecoregion_summaries$pr[start_index:i-1])))
    }
  }

  if (!file.exists(lagged_precip_path)) {
    ecoregion_summaries %>%
      dplyr::select(US_L4NAME, year, prev_12mo_precip) %>%
      write_rds(lagged_precip_path)
  }

  housing_df <- read_csv(file.path(processed_data_dir, 'population_density.csv')) %>%
    mutate(ym = as.yearmon(paste(year, sprintf("%02d", month), sep = "-"))) %>%
    arrange(US_L4NAME, year, month)%>%
    filter(ym>'Dec 1989' & year < 2061)

  ecoregion_summarie<-ecoregion_summaries%>%
    filter(year > 1989)

  housing_dens <- housing_df$housing_density
  ecoregion_summarie$housing_density <- housing_dens

  ecoregion_summarie<-ecoregion_summarie%>%
    mutate(er_ym = paste(US_L4NAME, ym, sep = "_"))

  cnt_df<-ecoregion_summarie%>%
    mutate(ym = as.yearmon(paste(year, sprintf("%02d", month), sep = "-")))%>%
    mutate(er_ym = paste(US_L4NAME, ym, sep = "_"))%>%
    arrange(ym,US_L4NAME)

  cnt_df$n_fire<-0
  count_df<- count_df%>%
    filter(year>1989)

  # Find matching indices
  matching_indices <- match(cnt_df$er_ym, count_df$er_ym)

  # Update n_fire where there's a match
  cnt_df$n_fire[!is.na(matching_indices)] <- count_df$n_fire[!is.na(matching_indices)]

  # Set n_fire to 0 where there's no match
  cnt_df$n_fire[is.na(matching_indices)] <- 0

  write_csv(cnt_df, path = out_file)

  print('Count, climate, and housing data integrated and saved in count_df.csv')

}

models <- c("CanESM2", "CNRM", "CSIRO", "HadGEM2-CC", "HadGEM2-ES", "IPSL", "MIROC5", "MRI")

for (model in models) {
  generate_counts(model)
}
