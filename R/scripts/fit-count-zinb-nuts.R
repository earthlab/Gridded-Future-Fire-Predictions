library(readr)
library(rstan)
zi_d <- read_rds('zi_d.rds')

zinb_init <- stan_model('wildfire-extremes/stan/counts-zinb.stan')
zinb_full_fit <- sampling(zinb_init,
                          data = zi_d,
                          init_r = 0.01,
                          iter = 1000, 
                          pars =  c('mu_full','count_pred'),
                          cores = 16)

post <- rstan::extract(zinb_full_fit,pars = 'count_pred')

write_rds(post, path = 'post.rds')
