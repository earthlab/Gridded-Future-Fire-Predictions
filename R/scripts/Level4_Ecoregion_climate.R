library(sf)
library(terra)
library(tidyverse)
library(raster)
library(parallel)
library(pbapply)
library(assertthat)
library(purrr)
library(here)



project_dir = here()
raw_data_dir = file.path(project_dir, "data", "raw")
processed_data_dir = file.path(project_dir, "data", "processed")

# Extracting monthly climate summaries for ecoregions ---------------------
ecoregion_shp_sf <- st_make_valid(st_read(file.path(raw_data_dir, 'ecoregions_L4', 'us_eco_l4_no_st.shp')))
ecoregion_shp <- st_transform(ecoregion_shp_sf, crs = "EPSG:4326")
ecoregion_shp <- as_Spatial(ecoregion_shp$geom)
ecoregion_shp$US_L4NAME <- as.character(ecoregion_shp_sf$US_L4NAME)

# # define an efficient extraction function to get mean values by polygon
fast_extract <- function(rasterfile, index_list) {
  library(raster)

  r <- raster::brick(rasterfile)
  if (extent(r)@xmin > 0 && extent(r)@xmax > 0) {
    extent(r) <- extent(r) - c(360, 360, 0, 0)
  }

  polygon_means <- purrr::map(index_list, function(x) {
    extracts <- raster::extract(r, x)
    colMeans(extracts, na.rm = TRUE)
  })
  
  list_of_dfs <- purrr::map(polygon_means, function(x) {
    df <- as.data.frame(x)
    tibble::rownames_to_column(df)
  })
  
  merged_dfs <- dplyr::bind_rows(list_of_dfs, .id = 'US_L4NAME')
  wide_df <- tidyr::pivot_wider(merged_dfs, names_from = rowname, values_from = x)
  return(tibble::as_tibble(wide_df))
}


generate_summary <- function(model_name) {

  if (model_name == "monthly") {
    destfile <- file.path(processed_data_dir, paste0("MET", "_ecoregion_summaries.csv"))
  }
  else {
    destfile <- file.path(processed_data_dir, paste0(model_name, "_ecoregion_summaries.csv"))
  }

  if (file.exists(destfile)) {
    return(paste0(destfile,  " already exists"))
  }
  
  # File Handling
  tifs <- Sys.glob(file.path(processed_data_dir, "climate-data", "*", 
                            paste0("*", model_name, "*.tif")))

  # Read the first raster file
  r <- raster::brick(tifs[1])
  shp_raster_idx <- raster::cellFromPolygon(r, ecoregion_shp)

  names(shp_raster_idx) <- ecoregion_shp$US_L4NAME

  # Group raster indices by ecoregion
  ecoregion_raster_idx <- vector(mode = 'list',
                                 length = length(unique(ecoregion_shp$US_L4NAME)))
  ecoregion_names <- sort(unique(ecoregion_shp$US_L4NAME))
  names(ecoregion_raster_idx) <- ecoregion_names
  for (i in seq_along(ecoregion_names)) {
    list_elements <- names(shp_raster_idx) == ecoregion_names[i]
    assert_that(any(list_elements))
    ecoregion_raster_idx[[i]] <- shp_raster_idx[list_elements] %>%
      unlist
  }

  for (i in 1:length(ecoregion_raster_idx)) {write.csv(ecoregion_raster_idx[[i]], file.path(processed_data_dir, "new_ecoregions", gsub("/", "_", paste0(names(ecoregion_raster_idx)[[i]], '.csv'))))}

  # Extract climate data ---------------------------------------
  print(paste0("Aggregating monthly climate data to ecoregion means for ", model_name, ". May take a while..."))
  pboptions(type = 'txt', use_lb = TRUE)
  cl <- makeCluster(getOption("cl.cores", detectCores()))
  extractions <- pblapply(tifs, 
                          fast_extract, 
                          index_list = ecoregion_raster_idx,
                          cl = cl)
  stopCluster(cl)

  print("finished extractions")

  # Process extracted values into a usable data frame -----------------------
  ecoregion_summaries <- extractions %>%
    bind_cols %>%
    mutate(L4NAME = ecoregion_names)%>%
    dplyr::select(-starts_with('US_L4NAME'))%>%
    gather(variable, value, -L4NAME) %>%
    filter(!grepl(pattern = 'L4NAME', x = variable)) %>%
    separate(variable,
            into = c("model", "variable", "year", "month"),
            sep = "_") %>%
    arrange(year, month, variable, L4NAME) %>% 
    rename(US_L4NAME = L4NAME)

  ecoregion_summaries <- ecoregion_summaries %>%
   mutate(variable = ifelse(variable == "rmin", "rhsmin", variable)) %>%
   mutate(variable = ifelse(variable == "tmmx", "tasmax", variable)) %>%
   mutate(variable = ifelse(variable == "vs", "was", variable))

  ### Need to have a separate CSV file for each future climate model
  write_csv(ecoregion_summaries, destfile)

}

models <- c("CanESM2", "CNRM", "CSIRO", "HadGEM2-CC", "HadGEM2-ES", "IPSL", "MIROC5", "MRI", "monthly")

for (model in models) {
  generate_summary(model)
}
