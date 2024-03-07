library(readr)
library(rstan)
stan_d <- read_rds('stan_d.rds')

ba_lognormal_init <- stan_model('wildfire-extremes/stan/area-lognormal.stan')
ba_lognormal_fit <- sampling(
  ba_lognormal_init,
  data = stan_d,
  cores = 16,
  init_r = .01,
  iter = 1000)

ln_post <- rstan::extract(ba_lognormal_fit, pars = c('mu_full', 'scale'))

write_rds(ln_post, 'ln_post.rds')