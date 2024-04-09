### Deloitte count and BA model output processing
library(lubridate)
library(tidyverse)
library(patchwork)
library(assertthat)
library(sf)
library(here)
library(brms)
library(reshape2)
library(dplyr)

project_dir = here()
raw_data_dir = file.path(project_dir, "data", "raw")
processed_data_dir = file.path(project_dir, "data", "processed")

min_size <- 1000

process_output <- function(model, subregion) { 
  stan_dir = file.path(processed_data_dir, "stan", model)
  cutoff_year <- read_rds(file.path(stan_dir, 'cutoff_year.rds'))

  st_covs <- read_rds(file.path(stan_dir, paste0('st_covs_', subregion, '.rds')))

  # Extract posterior draws and visualize results ---------------------------
  post <- read_rds(file.path(stan_dir, paste0('post', subregion, '.rds')))
  # 
  # turn into data frame
  preds <- post$count_pred %>%
    reshape2::melt(varnames = c('iter', 'id')) %>%
    as_tibble %>%
    left_join(select(st_covs, id, ym, US_L4NAME, year))

  # using the ZINB + lognormal model
  zinb_preds <- preds %>%
    rename(n_event = value) %>%
    dplyr::select(-year) %>%
    arrange(iter, id)

  ln_post <- read_rds(file.path(stan_dir, paste0('ln_post', subregion, '.rds')))

  ln_mu <- ln_post$mu_full %>%
    reshape2::melt(varnames = c('iter', 'id'), value.name = 'ln_mu') %>%
    arrange(iter, id)

  ln_sigma <- ln_post$scale %>%
    reshape2::melt(varnames = c('iter'), value.name = 'ln_scale')

  test_preds <- left_join(zinb_preds, ln_mu) %>%
    left_join(ln_sigma)

  # subset to test period
  test_preds2 <- test_preds %>%
    left_join(dplyr::select(st_covs, id, year)) %>%
    filter(year >= cutoff_year)

  # Inference over total burn area for the test period ----------------------
  total_df <- test_preds2 %>%
    # filter out zero event records (don't contribute to sum)
    filter(n_event > 0) %>%
    rowwise() %>%
    mutate(total_area = sum(exp(rnorm(n_event, ln_mu, ln_scale)) + min_size)) %>%
    ungroup

  Sum_test<- test_preds2%>%
    left_join(total_df)

  Sum_test$total_area[is.na(Sum_test$total_area)] <- 0

  summarized_data <- Sum_test %>%
    group_by(iter, US_L4NAME, year) %>%
    summarize(total_n_event = sum(n_event),
              total_BA = sum(total_area))

  # Group by 'US_L4NAME' and 'year', then calculate the mea of yearly totals
  Annual <- summarized_data %>%
    group_by(US_L4NAME,year) %>%
    summarize(mean_total_n_event = mean(total_n_event, na.rm = TRUE),
              mean_total_BA = mean(total_BA, na.rm = TRUE))

  write_rds(Annual, file.path(stan_dir, paste0('Annual', subregion, '.rds')))

}

subregions <- c("L5", "L6_1", "L6_2", "L7", "L8_1", "L8_2", "L8_3", "L9_1", "L9_2", "L10_1", "L10_2", "L11")
models <- c("CanESM2", "CNRM", "CSIRO", "HadGEM2-CC", "HadGEM2-ES", "IPSL", "MIROC5", "MRI")

for (model in models) {
  for (subregion in subregions) {
    process_output(model, subregion)
  }
}
