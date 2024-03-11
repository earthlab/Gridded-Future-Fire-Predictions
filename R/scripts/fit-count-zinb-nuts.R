library(readr)
library(rstan)

library(here)


project_dir = here()
raw_data_dir = file.path(project_dir, "data", "raw")
processed_data_dir = file.path(project_dir, "data", "processed")
subregions = c("L5", "L6_1", "L6_2", "L7", "L8_1", "L8_2", "L8_3", "L9_1", "L9_2", "L10_1", "L10_2", "L11")

fit_count <- function(model_name) { 
    stan_dir = file.path(processed_data_dir, "stan", model_name)

    for (subregion in subregions) {
        zi_d <- read_rds(file.path(stan_dir, paste0('zi_d', subregion, '.rds')))

        zinb_init <- stan_model(file.path(raw_data_dir, 'counts-zinb.stan'))
        zinb_full_fit <- sampling(zinb_init,
                                data = zi_d,
                                init_r = 0.01,
                                iter = 1000, 
                                pars =  c('mu_full','count_pred'),
                                cores = 16)

        post <- rstan::extract(zinb_full_fit,pars = 'count_pred')

        write_rds(post, path = file.path(stan_dir, paste0('post', subregion, '.rds')))
    }

}

models <- c("CanESM2", "CNRM", "CSIRO", "HadGEM2-CC", "HadGEM2-ES", "IPSL", "MIROC5", "MRI")

for (model in models) {
  fit_count(model)
}
