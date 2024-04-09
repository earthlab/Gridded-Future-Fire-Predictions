### Combining to form CONUS results and then grid results
library(lubridate)
library(tidyverse)
library(patchwork)
library(assertthat)
library(sf)
library(here)
library(brms)
library(reshape2)
library(dplyr)
library(stars)


project_dir = here()
raw_data_dir = file.path(project_dir, "data", "raw")
processed_data_dir = file.path(project_dir, "data", "processed")

models <- c("CanESM2", "CNRM", "CSIRO", "HadGEM2-CC", "HadGEM2-ES", "IPSL", "MIROC5", "MRI")


combine_to_conus <- function(model) {

  stan_dir = file.path(processed_data_dir, "stan", model)

  L10_1 <- read_rds(file.path(stan_dir, 'AnnualL10_1.rds'))
  L10_2 <- read_rds(file.path(stan_dir, 'AnnualL10_2.rds'))
  L11 <- read_rds(file.path(stan_dir, 'AnnualL11.rds'))
  L5 <- read_rds(file.path(stan_dir, 'AnnualL5.rds'))
  L6_1 <- read_rds(file.path(stan_dir, 'AnnualL6_1.rds'))
  L6_2 <- read_rds(file.path(stan_dir, 'AnnualL6_2.rds'))
  L7 <- read_rds(file.path(stan_dir, 'AnnualL7.rds'))
  L8_1 <- read_rds(file.path(stan_dir, 'AnnualL8_1.rds'))
  L8_2 <- read_rds(file.path(stan_dir, 'AnnualL8_2.rds'))
  L8_3 <- read_rds(file.path(stan_dir, 'AnnualL8_3.rds'))
  L9_1 <- read_rds(file.path(stan_dir, 'AnnualL9_1.rds'))
  L9_2 <- read_rds(file.path(stan_dir, 'AnnualL9_2.rds'))

  Comb1<- full_join(L10_1,L10_2)
  Comb2<- full_join(Comb1,L11)
  Comb3<- full_join(Comb2,L5)
  Comb4<- full_join(Comb3,L6_1)
  Comb5<- full_join(Comb4,L6_2)
  Comb6<- full_join(Comb5,L7)
  Comb7<- full_join(Comb6,L8_1)
  Comb8<- full_join(Comb7,L8_2)
  Comb9<- full_join(Comb8,L8_3)
  Comb10<- full_join(Comb9,L9_1)
  CONUS_Can<- full_join(Comb10,L9_2)

  return(L6_2)

}

conus_dfs <- list()
for (i in 1:length(models)) {
  CONUS_Can <- combine_to_conus(models[[i]])
  conus_dfs[[i]] <- CONUS_Can
}

### Ensemble
Ensb_Area <- as.data.frame(do.call(cbind, lapply(conus_dfs, function(df) df$mean_total_BA)))
Ensb_NFire <- as.data.frame(do.call(cbind, lapply(conus_dfs, function(df) df$mean_total_n_event)))

En_Area<-rowMeans(Ensb_Area)
En_NFire<-rowMeans(Ensb_NFire)

US_L4NAME<-CONUS_Can$US_L4NAME
Year<- CONUS_Can$year
Ensemble<-data.frame(US_L4NAME,Year, En_Area, En_NFire)

write_rds(Ensemble, "Ensemble.rds")

ecoregions <- read_rds(file.path(processed_data_dir, "stan", "CanESM2", "ecoregions.rds"))

process_year <- function(year, ensemble, ecoregions, output_filename) {
  # Make Yearly files
  Ens <- ensemble%>%
    filter(Year == year)

  # Modify fire data as needed
  fires_data <- sp::merge(ecoregions, Ens, by = 'US_L4NAME', duplicateGeoms = T) 

  ## create 4km grid
  grid = st_as_stars(st_bbox(fires_data), dx = 0.04, dy = 0.04)
  grid = st_as_sf(grid)
  grid = grid[fires_data, ]

  # # Core processing logic
  # Add grid_id to grid
  grid_out <- grid %>%
  mutate(grid_id = row_number()) %>% # create unique ID
  st_join(fires_data) %>% # join the dataset
  group_by(grid_id) # group by the grid id

    write_rds(grid_out, output_filename)
}

for (year in 2023:2060) {
  process_year(year, Ensemble, ecoregions, paste0("Ens_", year, ".rds"))
}
