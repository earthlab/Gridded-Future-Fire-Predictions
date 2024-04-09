library(readr)
library(rstan)
library(parallel)

library(here)


project_dir = here()
raw_data_dir = file.path(project_dir, "data", "raw")
processed_data_dir = file.path(project_dir, "data", "processed")
subregions = c("L5", "L6_1", "L6_2", "L7", "L8_1", "L8_2", "L8_3", "L9_1", "L9_2", "L10_1", "L10_2", "L11")

fit_burn_area <- function(model_name) { 
  stan_dir = file.path(processed_data_dir, "stan", model_name)
  no_cores <- detectCores() - 1  # Leave one core for the main process

  for (subregion in subregions) {
    stan_d <- read_rds(file.path(stan_dir, paste0('stan_d', subregion, '.rds')))
  
    ba_lognormal_init <- stan_model(file.path(raw_data_dir, 'area-lognormal.stan'))

    ba_lognormal_fit <- sampling(
      ba_lognormal_init,
      data = stan_d,
      cores = no_cores,
      init_r = .01,
      iter = 1000)

    ln_post <- rstan::extract(ba_lognormal_fit, pars = c('mu_full', 'scale'))

    write_rds(ln_post, file.path(stan_dir, paste0("ln_post", subregion, ".rds")))
  }
  

}

models <- c("CanESM2", "CNRM", "CSIRO", "HadGEM2-CC", "HadGEM2-ES", "IPSL", "MIROC5", "MRI")

for (model in models) {
  fit_burn_area(model)
}
