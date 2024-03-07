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

min_size <- 1000
cutoff_year <- read_rds('data/processed/HadGEM2_CC/cutoff_year.rds')


## L10_1
st_covsL10_1 <- read_rds('data/processed/CanESM2/L10_1/st_covs_L10_1.rds')

# Extract posterior draws and visualize results ---------------------------
post10_1 <- read_rds('data/processed/CanESM2/L10_1/post_L10_1.rds')
# 
# turn into data frame
predsL10_1 <- post10_1$count_pred %>%
  reshape2::melt(varnames = c('iter', 'id')) %>%
  as_tibble %>%
  left_join(select(st_covsL10_1, id, ym, US_L4NAME, year))

# using the ZINB + lognormal model
zinb_preds <- predsL10_1 %>%
  rename(n_event = value) %>%
  dplyr::select(-year) %>%
  arrange(iter, id)

ln_post <- read_rds('data/processed/CanESM2/L10_1/ln_postL10_1.rds')

ln_mu <- ln_post$mu_full %>%
  reshape2::melt(varnames = c('iter', 'id'), value.name = 'ln_mu') %>%
  arrange(iter, id)

ln_sigma <- ln_post$scale %>%
  reshape2::melt(varnames = c('iter'), value.name = 'ln_scale')


test_preds <- left_join(zinb_preds, ln_mu) %>%
  left_join(ln_sigma)

# subset to test period
test_preds2 <- test_preds %>%
  left_join(dplyr::select(st_covsL10_1, id, year)) %>%
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
Annual_L10_1 <- summarized_data %>%
  group_by(US_L4NAME,year) %>%
  summarize(mean_total_n_event = mean(total_n_event),
            mean_total_BA = mean(total_BA))

write_rds(Annual_L10_1, 'AnnualL10_1.rds')


## L10_2
st_covsL10_2 <- read_rds('data/processed/CanESM2/L10_2/st_covs_L10_2.rds')

# Extract posterior draws and visualize results ---------------------------
post10_2 <- read_rds('data/processed/CanESM2/L10_2/post_L10_2.rds')

# turn into data frame
predsL10_2 <- post10_2$count_pred %>%
  reshape2::melt(varnames = c('iter', 'id')) %>%
  as_tibble %>%
  left_join(select(st_covsL10_2, id, ym, US_L4NAME, year))

# using the ZINB + lognormal model
zinb_preds <- predsL10_2 %>%
  rename(n_event = value) %>%
  dplyr::select(-year) %>%
  arrange(iter, id)

ln_post <- read_rds('data/processed/CanESM2/L10_2/ln_postL10_2.rds')

ln_mu <- ln_post$mu_full %>%
  reshape2::melt(varnames = c('iter', 'id'), value.name = 'ln_mu') %>%
  arrange(iter, id)

ln_sigma <- ln_post$scale %>%
  reshape2::melt(varnames = c('iter'), value.name = 'ln_scale')


test_preds <- left_join(zinb_preds, ln_mu) %>%
  left_join(ln_sigma)

# subset to test period
test_preds2 <- test_preds %>%
  left_join(dplyr::select(st_covsL10_2, id, year)) %>%
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

# Group by 'US_L4NAME' and 'year', then calculate the median of yearly totals
Annual_L10_2 <- summarized_data %>%
  group_by(US_L4NAME,year) %>%
  summarize(mean_total_n_event = mean(total_n_event),
            mean_total_BA = mean(total_BA))

write_rds(Annual_L10_2, 'AnnualL10_2.rds')


## L11
st_covsL11 <- read_rds('data/processed/CanESM2/L11/st_covs_L11.rds')

# Extract posterior draws and visualize results ---------------------------
post11 <- read_rds('data/processed/CanESM2/L11/post_L11.rds')

# turn into data frame
predsL11 <- post11$count_pred %>%
  reshape2::melt(varnames = c('iter', 'id')) %>%
  as_tibble %>%
  left_join(select(st_covsL11, id, ym, US_L4NAME, year))

# using the ZINB + lognormal model
zinb_preds <- predsL11 %>%
  rename(n_event = value) %>%
  dplyr::select(-year) %>%
  arrange(iter, id)

ln_post <- read_rds('data/processed/CanESM2/L11/ln_postL11.rds')

ln_mu <- ln_post$mu_full %>%
  reshape2::melt(varnames = c('iter', 'id'), value.name = 'ln_mu') %>%
  arrange(iter, id)

ln_sigma <- ln_post$scale %>%
  reshape2::melt(varnames = c('iter'), value.name = 'ln_scale')


test_preds <- left_join(zinb_preds, ln_mu) %>%
  left_join(ln_sigma)

# subset to test period
test_preds2 <- test_preds %>%
  left_join(dplyr::select(st_covsL11, id, year)) %>%
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

# Group by 'US_L4NAME' and 'year', then calculate the median of yearly totals
Annual_L11 <- summarized_data %>%
  group_by(US_L4NAME,year) %>%
  summarize(mean_total_n_event = mean(total_n_event),
            mean_total_BA = mean(total_BA))

write_rds(Annual_L11, 'AnnualL11.rds')


## L5
st_covsL5 <- read_rds('data/processed/CanESM2/L5/st_covs_L5.rds')

# Extract posterior draws and visualize results ---------------------------
post5 <- read_rds('data/processed/CanESM2/L5/post_L5.rds')

# turn into data frame
predsL5 <- post5$count_pred %>%
  reshape2::melt(varnames = c('iter', 'id')) %>%
  as_tibble %>%
  left_join(select(st_covsL5, id, ym, US_L4NAME, year))

# using the ZINB + lognormal model
zinb_preds <- predsL5 %>%
  rename(n_event = value) %>%
  dplyr::select(-year) %>%
  arrange(iter, id)

ln_post <- read_rds('data/processed/CanESM2/L5/ln_postL5.rds')

ln_mu <- ln_post$mu_full %>%
  reshape2::melt(varnames = c('iter', 'id'), value.name = 'ln_mu') %>%
  arrange(iter, id)

ln_sigma <- ln_post$scale %>%
  reshape2::melt(varnames = c('iter'), value.name = 'ln_scale')


test_preds <- left_join(zinb_preds, ln_mu) %>%
  left_join(ln_sigma)

# subset to test period
test_preds2 <- test_preds %>%
  left_join(dplyr::select(st_covsL5, id, year)) %>%
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

# Group by 'US_L4NAME' and 'year', then calculate the median of yearly totals
Annual_L5 <- summarized_data %>%
  group_by(US_L4NAME,year) %>%
  summarize(mean_total_n_event = mean(total_n_event, na.rm= TRUE),
            mean_total_BA = mean(total_BA))

write_rds(Annual_L5, 'AnnualL5.rds')

## L6_1
st_covsL6_1 <- read_rds('data/processed/CanESM2/L6_1/st_covs_L6_1.rds')

# Extract posterior draws and visualize results ---------------------------
post6_1 <- read_rds('data/processed/CanESM2/L6_1/post_L6_1.rds')

# turn into data frame
predsL6_1 <- post6_1$count_pred %>%
  reshape2::melt(varnames = c('iter', 'id')) %>%
  as_tibble %>%
  left_join(select(st_covsL6_1, id, ym, US_L4NAME, year))

# using the ZINB + lognormal model
zinb_preds <- predsL6_1 %>%
  rename(n_event = value) %>%
  dplyr::select(-year) %>%
  arrange(iter, id)

ln_post <- read_rds('data/processed/CanESM2/L6_1/ln_postL6_1.rds')

ln_mu <- ln_post$mu_full %>%
  reshape2::melt(varnames = c('iter', 'id'), value.name = 'ln_mu') %>%
  arrange(iter, id)

ln_sigma <- ln_post$scale %>%
  reshape2::melt(varnames = c('iter'), value.name = 'ln_scale')


test_preds <- left_join(zinb_preds, ln_mu) %>%
  left_join(ln_sigma)

# subset to test period
test_preds2 <- test_preds %>%
  left_join(dplyr::select(st_covsL6_1, id, year)) %>%
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

# Group by 'US_L4NAME' and 'year', then calculate the median of yearly totals
Annual_L6_1 <- summarized_data %>%
  group_by(US_L4NAME,year) %>%
  summarize(mean_total_n_event = mean(total_n_event),
            mean_total_BA = mean(total_BA))

write_rds(Annual_L6_1, 'AnnualL6_1.rds')


## L6_2
st_covsL6_2 <- read_rds('data/processed/HadGEM2_CC/L6_2/st_covs_L6_2.rds')

# Extract posterior draws and visualize results ---------------------------
post6_2 <- read_rds('data/processed/HadGEM2_CC/L6_2/post_L6_2.rds')

# turn into data frame
predsL6_2 <- post6_2$count_pred %>%
  reshape2::melt(varnames = c('iter', 'id')) %>%
  as_tibble %>%
  left_join(select(st_covsL6_2, id, ym, US_L4NAME, year))

# using the ZINB + lognormal model
zinb_preds <- predsL6_2 %>%
  rename(n_event = value) %>%
  dplyr::select(-year) %>%
  arrange(iter, id)

ln_post <- read_rds('data/processed/HadGEM2_CC/L6_2/ln_postL6_2.rds')

ln_mu <- ln_post$mu_full %>%
  reshape2::melt(varnames = c('iter', 'id'), value.name = 'ln_mu') %>%
  arrange(iter, id)

ln_sigma <- ln_post$scale %>%
  reshape2::melt(varnames = c('iter'), value.name = 'ln_scale')


test_preds <- left_join(zinb_preds, ln_mu) %>%
  left_join(ln_sigma)

# subset to test period
test_preds2 <- test_preds %>%
  left_join(dplyr::select(st_covsL6_2, id, year)) %>%
  filter(year >= cutoff_year)

# Inference over total burn area for the test period ----------------------
total_df <- test_preds2 %>%
  # filter out zero event records (don't contribute to sum)
  filter(n_event > 0) %>%
  rowwise() %>%
  mutate(total_area = sum(exp(rnorm(n_event, ln_mu, ln_scale)) + min_size)) %>%
  ungroup

Sum_test<- test_preds2%>%
  filter(iter>500)%>%
  left_join(total_df)

Sum_test$total_area[is.na(Sum_test$total_area)] <- 0

summarized_data <- Sum_test %>%
  group_by(iter, US_L4NAME, year) %>%
  summarize(total_n_event = sum(n_event),
            total_BA = sum(total_area))

# Group by 'US_L4NAME' and 'year', then calculate the median of yearly totals
Annual_L6_2 <- summarized_data %>%
  group_by(US_L4NAME,year) %>%
  summarize(mean_total_n_event = mean(total_n_event, na.rm = TRUE),
            mean_total_BA = mean(total_BA))

write_rds(Annual_L6_2, 'AnnualL6_2.rds')


## L7
st_covsL7 <- read_rds('data/processed/CanESM2/L7/st_covs_L7.rds')

# Extract posterior draws and visualize results ---------------------------
post7 <- read_rds('data/processed/CanESM2/L7/post_L7.rds')

# turn into data frame
predsL7 <- post7$count_pred %>%
  reshape2::melt(varnames = c('iter', 'id')) %>%
  as_tibble %>%
  left_join(select(st_covsL7, id, ym, US_L4NAME, year))

# using the ZINB + lognormal model
zinb_preds <- predsL7 %>%
  rename(n_event = value) %>%
  dplyr::select(-year) %>%
  arrange(iter, id)

ln_post <- read_rds('data/processed/CanESM2/L7/ln_postL7.rds')

ln_mu <- ln_post$mu_full %>%
  reshape2::melt(varnames = c('iter', 'id'), value.name = 'ln_mu') %>%
  arrange(iter, id)

ln_sigma <- ln_post$scale %>%
  reshape2::melt(varnames = c('iter'), value.name = 'ln_scale')


test_preds <- left_join(zinb_preds, ln_mu) %>%
  left_join(ln_sigma)

# subset to test period
test_preds2 <- test_preds %>%
  left_join(dplyr::select(st_covsL7, id, year)) %>%
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

# Group by 'US_L4NAME' and 'year', then calculate the median of yearly totals
Annual_L7 <- summarized_data %>%
  group_by(US_L4NAME,year) %>%
  summarize(mean_total_n_event = mean(total_n_event, na.rm= TRUE),
            mean_total_BA = mean(total_BA))

write_rds(Annual_L7, 'AnnualL7.rds')


## L8_1
st_covsL8_1 <- read_rds('data/processed/CanESM2/L8_1/st_covs_L8_1.rds')

# Extract posterior draws and visualize results ---------------------------
post8_1 <- read_rds('data/processed/CanESM2/L8_1/post_L8_1.rds')

# turn into data frame
predsL8_1 <- post8_1$count_pred %>%
  reshape2::melt(varnames = c('iter', 'id')) %>%
  as_tibble %>%
  left_join(select(st_covsL8_1, id, ym, US_L4NAME, year))

# using the ZINB + lognormal model
zinb_preds <- predsL8_1 %>%
  rename(n_event = value) %>%
  dplyr::select(-year) %>%
  arrange(iter, id)

ln_post <- read_rds('data/processed/CanESM2/L8_1/ln_postL8_1.rds')

ln_mu <- ln_post$mu_full %>%
  reshape2::melt(varnames = c('iter', 'id'), value.name = 'ln_mu') %>%
  arrange(iter, id)

ln_sigma <- ln_post$scale %>%
  reshape2::melt(varnames = c('iter'), value.name = 'ln_scale')


test_preds <- left_join(zinb_preds, ln_mu) %>%
  left_join(ln_sigma)

# subset to test period
test_preds2 <- test_preds %>%
  left_join(dplyr::select(st_covsL8_1, id, year)) %>%
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

# Group by 'US_L4NAME' and 'year', then calculate the median of yearly totals
Annual_L8_1 <- summarized_data %>%
  group_by(US_L4NAME,year) %>%
  summarize(mean_total_n_event = mean(total_n_event),
            mean_total_BA = mean(total_BA))

write_rds(Annual_L8_1, 'AnnualL8_1.rds')


## L8_2
st_covsL8_2 <- read_rds('data/processed/CanESM2/L8_2/st_covs_L8_2.rds')

# Extract posterior draws and visualize results ---------------------------
post8_2 <- read_rds('data/processed/CanESM2/L8_2/post_L8_2.rds')

# turn into data frame
predsL8_2 <- post8_2$count_pred %>%
  reshape2::melt(varnames = c('iter', 'id')) %>%
  as_tibble %>%
  left_join(select(st_covsL8_2, id, ym, US_L4NAME, year))

# using the ZINB + lognormal model
zinb_preds <- predsL8_2 %>%
  rename(n_event = value) %>%
  dplyr::select(-year) %>%
  arrange(iter, id)

ln_post <- read_rds('data/processed/CanESM2/L8_2/ln_postL8_2.rds')

ln_mu <- ln_post$mu_full %>%
  reshape2::melt(varnames = c('iter', 'id'), value.name = 'ln_mu') %>%
  arrange(iter, id)

ln_sigma <- ln_post$scale %>%
  reshape2::melt(varnames = c('iter'), value.name = 'ln_scale')


test_preds <- left_join(zinb_preds, ln_mu) %>%
  left_join(ln_sigma)

# subset to test period
test_preds2 <- test_preds %>%
  left_join(dplyr::select(st_covsL8_2, id, year)) %>%
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

# Group by 'US_L4NAME' and 'year', then calculate the median of yearly totals
Annual_L8_2 <- summarized_data %>%
  group_by(US_L4NAME,year) %>%
  summarize(mean_total_n_event = mean(total_n_event, na.rm = TRUE),
            mean_total_BA = mean(total_BA))

write_rds(Annual_L8_2, 'AnnualL8_2.rds')


## L8_3
st_covsL8_3 <- read_rds('data/processed/HadGEM2_CC/L8_3/st_covs_L8_3.rds')

# Extract posterior draws and visualize results ---------------------------
post8_3 <- read_rds('data/processed/HadGEM2_CC/L8_3/post_L8_3.rds')

# turn into data frame
predsL8_3 <- post8_3$count_pred %>%
  reshape2::melt(varnames = c('iter', 'id')) %>%
  as_tibble %>%
  left_join(select(st_covsL8_3, id, ym, US_L4NAME, year))

# using the ZINB + lognormal model
zinb_preds <- predsL8_3 %>%
  rename(n_event = value) %>%
  dplyr::select(-year) %>%
  arrange(iter, id)

ln_post <- read_rds('data/processed/HadGEM2_CC/L8_3/ln_postL8_3.rds')

ln_mu <- ln_post$mu_full %>%
  reshape2::melt(varnames = c('iter', 'id'), value.name = 'ln_mu') %>%
  arrange(iter, id)

ln_sigma <- ln_post$scale %>%
  reshape2::melt(varnames = c('iter'), value.name = 'ln_scale')


test_preds <- left_join(zinb_preds, ln_mu) %>%
  left_join(ln_sigma)

# subset to test period
test_preds2 <- test_preds %>%
  left_join(dplyr::select(st_covsL8_3, id, year)) %>%
  filter(year >= cutoff_year)

# Inference over total burn area for the test period ----------------------
total_df <- test_preds2 %>%
  # filter out zero event records (don't contribute to sum)
  filter(n_event > 0) %>%
  rowwise() %>%
  mutate(total_area = sum(exp(rnorm(n_event, ln_mu, ln_scale)) + min_size)) %>%
  ungroup

Sum_test<- test_preds2%>%
  filter(iter > 500) %>%
  left_join(total_df)

Sum_test$total_area[is.na(Sum_test$total_area)] <- 0

summarized_data <- Sum_test %>%
  group_by(iter, US_L4NAME, year) %>%
  summarize(total_n_event = sum(n_event),
            total_BA = sum(total_area))

# Group by 'US_L4NAME' and 'year', then calculate the median of yearly totals
Annual_L8_3 <- summarized_data %>%
  group_by(US_L4NAME,year) %>%
  summarize(mean_total_n_event = mean(total_n_event, na.rm = TRUE),
            mean_total_BA = mean(total_BA))

write_rds(Annual_L8_3, 'AnnualL8_3.rds')


## L9_1
st_covsL9_1 <- read_rds('data/processed/CanESM2/L9_1/st_covs_L9_1.rds')

# Extract posterior draws and visualize results ---------------------------
post9_1 <- read_rds('data/processed/CanESM2/L9_1/post_L9_1.rds')

# turn into data frame
predsL9_1 <- post9_1$count_pred %>%
  reshape2::melt(varnames = c('iter', 'id')) %>%
  as_tibble %>%
  left_join(select(st_covsL9_1, id, ym, US_L4NAME, year))

# using the ZINB + lognormal model
zinb_preds <- predsL9_1 %>%
  rename(n_event = value) %>%
  dplyr::select(-year) %>%
  arrange(iter, id)

ln_post <- read_rds('data/processed/CanESM2/L9_1/ln_postL9_1.rds')

ln_mu <- ln_post$mu_full %>%
  reshape2::melt(varnames = c('iter', 'id'), value.name = 'ln_mu') %>%
  arrange(iter, id)

ln_sigma <- ln_post$scale %>%
  reshape2::melt(varnames = c('iter'), value.name = 'ln_scale')


test_preds <- left_join(zinb_preds, ln_mu) %>%
  left_join(ln_sigma)

# subset to test period
test_preds2 <- test_preds %>%
  left_join(dplyr::select(st_covsL9_1, id, year)) %>%
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

# Group by 'US_L4NAME' and 'year', then calculate the median of yearly totals
Annual_L9_1 <- summarized_data %>%
  group_by(US_L4NAME,year) %>%
  summarize(mean_total_n_event = mean(total_n_event),
            mean_total_BA = mean(total_BA))

write_rds(Annual_L9_1, 'AnnualL9_1.rds')


## L9_2
st_covsL9_2 <- read_rds('data/processed/CanESM2/L9_2/st_covs_L9_2.rds')

# Extract posterior draws and visualize results ---------------------------
post9_2 <- read_rds('data/processed/CanESM2/L9_2/post_L9_2.rds')

# turn into data frame
predsL9_2 <- post9_2$count_pred %>%
  reshape2::melt(varnames = c('iter', 'id')) %>%
  as_tibble %>%
  left_join(select(st_covsL9_2, id, ym, US_L4NAME, year))

# using the ZINB + lognormal model
zinb_preds <- predsL9_2 %>%
  rename(n_event = value) %>%
  dplyr::select(-year) %>%
  arrange(iter, id)

ln_post <- read_rds('data/processed/CanESM2/L9_2/ln_postL9_2.rds')

ln_mu <- ln_post$mu_full %>%
  reshape2::melt(varnames = c('iter', 'id'), value.name = 'ln_mu') %>%
  arrange(iter, id)

ln_sigma <- ln_post$scale %>%
  reshape2::melt(varnames = c('iter'), value.name = 'ln_scale')


test_preds <- left_join(zinb_preds, ln_mu) %>%
  left_join(ln_sigma)

# subset to test period
test_preds2 <- test_preds %>%
  left_join(dplyr::select(st_covsL9_2, id, year)) %>%
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

# Group by 'US_L4NAME' and 'year', then calculate the median of yearly totals
Annual_L9_2 <- summarized_data %>%
  group_by(US_L4NAME,year) %>%
  summarize(mean_total_n_event = mean(total_n_event, na.rm = TRUE),
            mean_total_BA = mean(total_BA))

write_rds(Annual_L9_2, 'AnnualL9_2.rds')
