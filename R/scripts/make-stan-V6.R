
library(tidyverse)
library(lubridate)
library(rstan)
library(splines)
library(spdep)
library(sf)
library(zoo)
library(assertthat)
library(lubridate)
library(sp)
library(spatialreg)
library(readr)
library(here)


project_dir = here()
raw_data_dir = file.path(project_dir, "data", "raw")
processed_data_dir = file.path(project_dir, "data", "processed")

ecoregions <- st_make_valid(st_read(file.path(raw_data_dir, "ecoregions_L4", 'us_eco_l4_no_st.shp')))

eco_L10_1<-ecoregions %>%
  filter(NA_L2CODE==10.1)%>%
  filter(!(US_L4NAME %in% c("Partly Forested Mountains", "Pluvial Lake Basins")))
eco_L10_2<- ecoregions%>%
  filter(NA_L2CODE %in% c("10.2", "12.1","13.1"))%>%
  filter(!(US_L4NAME %in% c("Lava Malpais", "Rio Grande Floodplain")))
eco_L11<- ecoregions%>%
  filter(NA_L1CODE==11)%>%
  filter(!(US_L4NAME %in% c("Northern Channel Islands", "Southern Channel Islands")))
eco_L5<- ecoregions%>%
  filter(NA_L1CODE==5)
eco_L6_1<- ecoregions%>%
  filter(NA_L3CODE>= "6.2.3" & NA_L3CODE<= "6.2.9")
eco_L6_2<- ecoregions%>%
  filter(NA_L3CODE>= "6.2.10" & NA_L3CODE<= "6.2.15")%>%
  filter(!(US_L4NAME %in% "Semiarid Foothills"))
eco_L7<- ecoregions%>%
  filter(NA_L1CODE==7)
eco_L8_1<- ecoregions%>%
  filter(NA_L2CODE>= 8.1 & NA_L2CODE<= 8.2)
eco_L8_2<- ecoregions%>%
  filter(NA_L2CODE== 8.3)%>%
 filter(!(US_L4NAME %in% c("Floodplains and Low Terraces", "Sand Hills")))
eco_L8_3<- ecoregions%>%
  filter(NA_L2CODE %in% c("8.4", "8.5","15.4"))%>%
  filter(!(US_L4NAME %in% c("Grand Prairie", "Floodplains and Low Terraces")))
eco_L9_1<- ecoregions%>%
  filter(NA_L2CODE>= 9.2 & NA_L2CODE<= 9.3)
eco_L9_2<- ecoregions%>%
  filter(NA_L2CODE>= 9.4 & NA_L2CODE<= 9.6)

# generate spatial neighbors
nb_L10_1 <- poly2nb(as(eco_L10_1, 'Spatial'))
nb_L10_2 <- poly2nb(as(eco_L10_2, 'Spatial'))
nb_L11 <- poly2nb(as(eco_L11, 'Spatial'))
nb_L5 <- poly2nb(as(eco_L5, 'Spatial'))
nb_L6_1 <- poly2nb(as(eco_L6_1, 'Spatial'))
nb_L6_2 <- poly2nb(as(eco_L6_2, 'Spatial'))
nb_L7 <- poly2nb(as(eco_L7, 'Spatial'))
nb_L8_1 <- poly2nb(as(eco_L8_1, 'Spatial'))
nb_L8_2 <- poly2nb(as(eco_L8_2, 'Spatial'))
nb_L8_3 <- poly2nb(as(eco_L8_3, 'Spatial'))
nb_L9_1 <- poly2nb(as(eco_L9_1, 'Spatial'))
nb_L9_2 <- poly2nb(as(eco_L9_2, 'Spatial'))

nb_agg_L10_1 <- aggregate(nb_L10_1, eco_L10_1$US_L4NAME)
nb_agg_L10_2 <- aggregate(nb_L10_2, eco_L10_2$US_L4NAME)
nb_agg_L11 <- aggregate(nb_L11, eco_L11$US_L4NAME)
nb_agg_L5 <- aggregate(nb_L5, eco_L5$US_L4NAME)
nb_agg_L6_1 <- aggregate(nb_L6_1, eco_L6_1$US_L4NAME)
nb_agg_L6_2 <- aggregate(nb_L6_2, eco_L6_2$US_L4NAME)
nb_agg_L7 <- aggregate(nb_L7, eco_L7$US_L4NAME)
nb_agg_L8_1 <- aggregate(nb_L8_1, eco_L8_1$US_L4NAME)
nb_agg_L8_2 <- aggregate(nb_L8_2, eco_L8_2$US_L4NAME)
nb_agg_L8_3 <- aggregate(nb_L8_3, eco_L8_3$US_L4NAME)
nb_agg_L9_1 <- aggregate(nb_L9_1, eco_L9_1$US_L4NAME)
nb_agg_L9_2 <- aggregate(nb_L9_2, eco_L9_2$US_L4NAME)

#nb_mat <- nb2mat(nb_agg, style = 'B')

# generate neighborhood data for car prior
listw_L10_1 <- nb2listw(nb_agg_L10_1, style = 'B', zero.policy = TRUE)
listw_L10_2 <- nb2listw(nb_agg_L10_2, style = 'B', zero.policy = TRUE)
listw_L11 <- nb2listw(nb_agg_L11, style = 'B', zero.policy = TRUE)
listw_L5 <- nb2listw(nb_agg_L5, style = 'B', zero.policy = TRUE)
listw_L6_1 <- nb2listw(nb_agg_L6_1, style = 'B', zero.policy = TRUE)
listw_L6_2 <- nb2listw(nb_agg_L6_2, style = 'B', zero.policy = TRUE)
listw_L7 <- nb2listw(nb_agg_L7, style = 'B', zero.policy = TRUE)
listw_L8_1 <- nb2listw(nb_agg_L8_1, style = 'B', zero.policy = TRUE)
listw_L8_2 <- nb2listw(nb_agg_L8_2, style = 'B', zero.policy = TRUE)
listw_L8_3 <- nb2listw(nb_agg_L8_3, style = 'B', zero.policy = TRUE)
listw_L9_1 <- nb2listw(nb_agg_L9_1, style = 'B', zero.policy = TRUE)
listw_L9_2 <- nb2listw(nb_agg_L9_2, style = 'B', zero.policy = TRUE)

listw_L10_1 $style

# B is suitable for building N, N_edges, node1, and node2
# following http://mc-stan.org/users/documentation/case-studies/icar_stan.html
B_L10_1  <- as(listw_L10_1 , 'symmetricMatrix')
B_L10_2  <- as(listw_L10_2 , 'symmetricMatrix')
B_L11  <- as(listw_L11 , 'symmetricMatrix')
B_L5  <- as(listw_L5 , 'symmetricMatrix')
B_L6_1  <- as(listw_L6_1 , 'symmetricMatrix')
B_L6_2  <- as(listw_L6_2 , 'symmetricMatrix')
B_L7  <- as(listw_L7 , 'symmetricMatrix')
B_L8_1  <- as(listw_L8_1 , 'symmetricMatrix')
B_L8_2  <- as(listw_L8_2 , 'symmetricMatrix')
B_L8_3  <- as(listw_L8_3 , 'symmetricMatrix')
B_L9_1  <- as(listw_L9_1 , 'symmetricMatrix')
B_L9_2  <- as(listw_L9_2 , 'symmetricMatrix')


ecoregion_df <- as(ecoregions, "Spatial") %>%
  data.frame

# get areas for each L3 ecoregion
area_df <- ecoregion_df %>%
  as.data.frame %>%
  as_tibble %>%
  group_by(US_L4NAME) %>%
  summarize(area = sum(Shape_Area))
  
er_df <- dplyr::distinct(data.frame(ecoregions),
                         US_L4NAME,US_L3NAME, NA_L2NAME, NA_L1NAME,
                         NA_L3CODE,NA_L2CODE, NA_L1CODE) %>%
  as_tibble

make_stan <- function(model_name) { 
  stan_dir = file.path(processed_data_dir, "stan", model_name)

  if (!dir.exists(stan_dir)) {
    dir.create(stan_dir)
  }

  cnt_df <- read.csv(file.path(processed_data_dir, paste0(model_name, "_Count_df.csv")))

  st_covs <- cnt_df %>%
    left_join(er_df) %>%
    mutate(log_housing_density = log(housing_density),
          pr = ifelse(pr < 0 , 0, pr)) %>%
    mutate(ym = as.yearmon(paste(year, sprintf("%02d", month), sep = "-"))) %>%
    left_join(area_df) %>%
    droplevels %>%
    mutate(er_ym = paste(US_L4NAME, ym, sep = "_")) %>%
    arrange(ym, US_L4NAME) %>%
    distinct(er_ym, .keep_all = TRUE) %>%
    filter(!(US_L4NAME %in% c("Northern Channel Islands", "Southern Channel Islands")))

  st_covs <- na.omit(st_covs)

  ### Subset data into manageable chunks by NA_L1, L2 & L3 code 
  L10_1 <- st_covs %>%
    filter(NA_L2CODE==10.1)
  L10_2<- st_covs%>%
    filter(NA_L2CODE %in% c("10.2", "12.1","13.1"))
  L11<- st_covs%>%
    filter(NA_L1CODE==11)
  L5<- st_covs %>%
    filter(NA_L1CODE==5)
  L6_1<- st_covs%>%
    filter(NA_L3CODE>= "6.2.3" & NA_L3CODE<= "6.2.9")
  L6_2<- st_covs%>%
    filter(NA_L3CODE>= "6.2.10" & NA_L3CODE<= "6.2.15")
  L7<- st_covs%>%
    filter(NA_L1CODE==7)
  L8_1<- st_covs%>%
    filter(NA_L2CODE>= 8.1 & NA_L2CODE<= 8.2)
  L8_2<- st_covs%>%
    filter(NA_L2CODE== 8.3)
  L8_3<- st_covs%>%
    filter(NA_L2CODE %in% c("8.4", "8.5","15.4"))
  L9_1<- st_covs%>%
    filter(NA_L2CODE>= 9.2 & NA_L2CODE<= 9.3)
  L9_2<- st_covs%>%
    filter(NA_L2CODE>= 9.4 & NA_L2CODE<= 9.6)

  L10_1$id <- 1:nrow(L10_1)
  L10_2$id <- 1:nrow(L10_2)
  L11$id <- 1:nrow(L11)
  L5$id <- 1:nrow(L5)
  L6_1$id <- 1:nrow(L6_1)
  L6_2$id <- 1:nrow(L6_2)
  L7$id <- 1:nrow(L7)
  L8_1$id <- 1:nrow(L8_1)
  L8_2$id <- 1:nrow(L8_2)
  L8_3$id <- 1:nrow(L8_3)
  L9_1$id <- 1:nrow(L9_1)
  L9_2$id <- 1:nrow(L9_2)

  # Create training sets for each subset, 
  ## including years from 1990 to cutoff_year
  cutoff_year <- 2020

  ### train_counts
  train_countsL10_1 <- L10_1 %>%
    filter(year < cutoff_year) 
  train_countsL10_2 <- L10_2 %>%
    filter(year < cutoff_year) 
  train_countsL11 <- L11 %>%
    filter(year < cutoff_year) 
  train_countsL5 <- L5 %>%
    filter(year < 2017) 
  train_countsL6_1 <- L6_1 %>%
    filter(year < cutoff_year) 
  train_countsL6_2 <- L6_2 %>%
    filter(year < cutoff_year) 
  train_countsL7 <- L7 %>%
    filter(year < cutoff_year) 
  train_countsL8_1 <- L8_1 %>%
    filter(year < cutoff_year) 
  train_countsL8_2 <- L8_2 %>%
    filter(year < cutoff_year)
  train_countsL8_3 <- L8_3 %>%
    filter(year < cutoff_year) 
  train_countsL9_1 <- L9_1 %>%
    filter(year < cutoff_year) 
  train_countsL9_2 <- L9_2 %>%
    filter(year < cutoff_year) 

  ov_path <- file.path(processed_data_dir, "ov.rds")
  ov <- read_rds(ov_path)

  fire_data_path <- file.path(raw_data_dir, "mtbs", "mtbs_FODpoints_DD.shp")

  mtbs <- st_make_valid(st_read(fire_data_path)) %>%
      st_transform(st_crs(ecoregions)) %>%
      filter(Incid_Type == "Wildfire", 
            BurnBndAc > 1e3) %>%
      mutate(year = year(Ig_Date), 
            month = month(Ig_Date),
            US_L4NAME = ecoregions$US_L4NAME[ov]) %>%
      filter(!is.na(US_L4NAME))

  ### Create train_burns
  train_burns <- mtbs %>%
    filter(year > 1989 & year < cutoff_year) %>%
    left_join(er_df)
  train_burns<- train_burns[-c(5391,5789),]  

  print("A")

  train_burnsL10_1 <- train_burns %>%
    filter(NA_L2CODE==10.1)%>%
    left_join(train_countsL10_1)%>%
    filter (!is.na(pr))
  train_burnsL10_2<- train_burns %>%
    filter(NA_L2CODE %in% c("10.2", "12.1","13.1"))%>%
    left_join(train_countsL10_2)%>%
    filter (!is.na(pr))
  train_burnsL11<- train_burns %>%
    filter(NA_L1CODE==11)%>%
    left_join(train_countsL11)%>%
    filter (!is.na(pr))
  train_burnsL5<- train_burns %>%
    filter(year < 2017)%>%
    filter(NA_L1CODE==5)%>%
    left_join(train_countsL5)%>%
    filter (!is.na(pr))
  train_burnsL6_1<- train_burns %>%
    filter(NA_L3CODE>= "6.2.3" & NA_L3CODE<= "6.2.9")%>%
    left_join(train_countsL6_1)%>%
    filter (!is.na(pr))
  train_burnsL6_2<- train_burns %>%
    filter(NA_L3CODE>= "6.2.10" & NA_L3CODE<= "6.2.15")%>%
    left_join(train_countsL6_2)%>%
    filter (!is.na(pr))
  train_burnsL7<- train_burns %>%
    filter(NA_L1CODE==7)%>%
    left_join(train_countsL7)%>%
    filter (!is.na(pr))
  train_burnsL8_1<- train_burns %>%
    filter(NA_L2CODE>= 8.1 & NA_L2CODE<= 8.2)%>%
    left_join(train_countsL8_1)%>%
    filter (!is.na(pr))
  train_burnsL8_2<- train_burns %>%
    filter(NA_L2CODE==8.3)%>%
    left_join(train_countsL8_2)%>%
    filter (!is.na(pr))
  train_burnsL8_3<- train_burns %>%
    filter(NA_L2CODE %in% c("8.4", "8.5","15.4"))%>%
    left_join(train_countsL8_3)%>%
    filter (!is.na(pr))
  train_burnsL9_1<- train_burns %>%
    filter(NA_L2CODE>= 9.2 & NA_L2CODE<= 9.3)%>%
    left_join(train_countsL9_1)%>%
    filter (!is.na(pr))
  train_burnsL9_2<- train_burns %>%
    filter(NA_L2CODE>= 9.4 & NA_L2CODE<= 9.6)%>%
    left_join(train_countsL9_2)%>%
    filter (!is.na(pr))

  ### Holdout counts
  holdout_countsL10_1 <- L10_1 %>%
    filter(year >= cutoff_year) 
  holdout_countsL10_2 <- L10_2 %>%
    filter(year >= cutoff_year) 
  holdout_countsL11 <- L11 %>%
    filter(year >= cutoff_year) 
  holdout_countsL5 <- L5 %>%
    filter(year >= 2017) 
  holdout_countsL6_1 <- L6_1 %>%
    filter(year >= cutoff_year) 
  holdout_countsL6_2 <- L6_2 %>%
    filter(year >= cutoff_year) 
  holdout_countsL7 <- L7 %>%
    filter(year >= cutoff_year) 
  holdout_countsL8_1 <- L8_1 %>%
    filter(year >= cutoff_year) 
  holdout_countsL8_2 <- L8_2 %>%
    filter(year >= cutoff_year)
  holdout_countsL8_3 <- L8_3 %>%
    filter(year >= cutoff_year) 
  holdout_countsL9_1 <- L9_1 %>%
    filter(year >= cutoff_year) 
  holdout_countsL9_2 <- L9_2 %>%
    filter(year >= cutoff_year) 

  print("B")

  holdout_cidL10_1 <- match(holdout_countsL10_1$er_ym, L10_1$er_ym)
  holdout_cidL10_2 <- match(holdout_countsL10_2$er_ym, L10_2$er_ym)
  holdout_cidL11 <- match(holdout_countsL11$er_ym, L11$er_ym)
  holdout_cidL5 <- match(holdout_countsL5$er_ym, L5$er_ym)
  holdout_cidL6_1 <- match(holdout_countsL6_1$er_ym, L6_1$er_ym)
  holdout_cidL6_2 <- match(holdout_countsL6_2$er_ym, L6_2$er_ym)
  holdout_cidL7 <- match(holdout_countsL7$er_ym, L7$er_ym)
  holdout_cidL8_1 <- match(holdout_countsL8_1$er_ym, L8_1$er_ym)
  holdout_cidL8_2 <- match(holdout_countsL8_2$er_ym, L8_2$er_ym)
  holdout_cidL8_3 <- match(holdout_countsL8_3$er_ym, L8_3$er_ym)
  holdout_cidL9_1 <- match(holdout_countsL9_1$er_ym, L9_1$er_ym)
  holdout_cidL9_2 <- match(holdout_countsL9_2$er_ym, L9_2$er_ym)
  print("C")

  ## Holdout_burns
  holdout_burns <-  mtbs %>%
    filter(year >= cutoff_year) %>%
    left_join(st_covs)%>%
    arrange(ym, US_L4NAME)
  holdout_burns<- holdout_burns[-510,]

  print("D")

  holdout_L5 <-  mtbs %>%
    filter(year >= 2017) %>%
    left_join(st_covs)%>%
    arrange(ym, US_L4NAME)
  holdout_burns<- holdout_burns[-510,]

  print("E")

  holdout_burnsL10_1<- holdout_burns%>%
    filter(NA_L2CODE==10.1)
  holdout_burnsL10_2<- holdout_burns%>%
    filter(NA_L2CODE %in% c("10.2", "12.1","13.1"))
  holdout_burnsL11<- holdout_burns%>%
    filter(NA_L1CODE==11)
  holdout_burnsL5<- holdout_L5%>%
    filter(NA_L1CODE==5)
  holdout_burnsL6_1<- holdout_burns%>%
    filter(NA_L3CODE>= "6.2.3" & NA_L3CODE<= "6.2.9")
  holdout_burnsL6_2<- holdout_burns%>%
    filter(NA_L3CODE>= "6.2.10" & NA_L3CODE<= "6.2.15")
  holdout_burnsL7<- holdout_burns%>%
    filter(NA_L1CODE==7)
  holdout_burnsL8_1<- holdout_burns%>%
    filter(NA_L2CODE>= 8.1 & NA_L2CODE<= 8.2)
  holdout_burnsL8_2<- holdout_burns%>%
    filter(NA_L2CODE==8.3)
  holdout_burnsL8_3<- holdout_burns%>%
    filter(NA_L2CODE %in% c("8.4", "8.5","15.4"))
  holdout_burnsL9_1<- holdout_burns%>%
    filter(NA_L2CODE>= 9.2 & NA_L2CODE<= 9.3)
  holdout_burnsL9_2<- holdout_burns%>%
    filter(NA_L2CODE>= 9.4 & NA_L2CODE<= 9.6)

  holdout_bidL10_1 <- match(holdout_burnsL10_1$er_ym, holdout_countsL10_1$er_ym)
  holdout_bidL10_2 <- match(holdout_burnsL10_2$er_ym, holdout_countsL10_2$er_ym)
  holdout_bidL11 <- match(holdout_burnsL11$er_ym, holdout_countsL11$er_ym)
  holdout_bidL5 <- match(holdout_burnsL5$er_ym, holdout_countsL5$er_ym)
  holdout_bidL6_1 <- match(holdout_burnsL6_1$er_ym, holdout_countsL6_1$er_ym)
  holdout_bidL6_2 <- match(holdout_burnsL6_2$er_ym, holdout_countsL6_2$er_ym)
  holdout_bidL7 <- match(holdout_burnsL7$er_ym, holdout_countsL7$er_ym)
  holdout_bidL8_1 <- match(holdout_burnsL8_1$er_ym, holdout_countsL8_1$er_ym)
  holdout_bidL8_2 <- match(holdout_burnsL8_2$er_ym, holdout_countsL8_2$er_ym)
  holdout_bidL8_3 <- match(holdout_burnsL8_3$er_ym, holdout_countsL8_3$er_ym)
  holdout_bidL9_1 <- match(holdout_burnsL9_1$er_ym, holdout_countsL9_1$er_ym)
  holdout_bidL9_2 <- match(holdout_burnsL9_2$er_ym, holdout_countsL9_2$er_ym)

  # this data frame has no duplicate ecoregion X timestep combos
  N_L10_1 <- length(unique(L10_1$US_L4NAME))
  N_L10_2 <- length(unique(L10_2$US_L4NAME))
  N_L11 <- length(unique(L11$US_L4NAME))
  N_L5 <- length(unique(L5$US_L4NAME))
  N_L6_1 <- length(unique(L6_1$US_L4NAME))
  N_L6_2 <- length(unique(L6_2$US_L4NAME))
  N_L7 <- length(unique(L7$US_L4NAME))
  N_L8_1 <- length(unique(L8_1$US_L4NAME))
  N_L8_2 <- length(unique(L8_2$US_L4NAME))
  N_L8_3 <- length(unique(L8_3$US_L4NAME))
  N_L9_1 <- length(unique(L9_1$US_L4NAME))
  N_L9_2 <- length(unique(L9_2$US_L4NAME))

  T <- length(unique(L10_1$ym))

  ### Testing that the lengths are correct 
  assert_that(identical(nrow(L10_1), N_L10_1 * T))
  assert_that(identical(nrow(L10_2), N_L10_2 * T))
  assert_that(identical(nrow(L11), N_L11 * T))
  assert_that(identical(nrow(L5), N_L5 * T))
  assert_that(identical(nrow(L6_1), N_L6_1 * T))
  assert_that(identical(nrow(L6_2), N_L6_2 * T))
  assert_that(identical(nrow(L7), N_L7 * T))
  assert_that(identical(nrow(L8_1), N_L8_1 * T))
  assert_that(identical(nrow(L8_2), N_L8_2 * T))
  assert_that(identical(nrow(L8_3), N_L8_3 * T))
  assert_that(identical(nrow(L9_1), N_L9_1 * T))
  assert_that(identical(nrow(L9_2), N_L9_2 * T))

  # Create b-splines for climate vars
  vars <- c('log_housing_density', 'was',
            'pr', 'prev_12mo_precip', 'tasmax',
            'rhsmin')
  df_each <- 5
  X_bs <- list()
  X_bs_df <- list()
  for (i in seq_along(vars)) {
    X_bs[[i]] <- bs(L10_1[[vars[i]]], df = df_each, intercept = TRUE)
    
    X_bs_df[[i]] <- X_bs[[i]] %>%
      as_tibble
    names(X_bs_df[[i]]) <- paste('bs', vars[[i]], 1:df_each, sep = '_')
  }

  X_bs_L10_1 <- bind_cols(X_bs_df)
  assert_that(!any(is.na(X_bs_L10_1)))

  X_bs <- list()
  X_bs_df <- list()
  for (i in seq_along(vars)) {
    X_bs[[i]] <- bs(L10_2[[vars[i]]], df = df_each, intercept = TRUE)
    
    X_bs_df[[i]] <- X_bs[[i]] %>%
      as_tibble
    names(X_bs_df[[i]]) <- paste('bs', vars[[i]], 1:df_each, sep = '_')
  }
  X_bs_L10_2 <- bind_cols(X_bs_df)
  assert_that(!any(is.na(X_bs_L10_2)))

  X_bs <- list()
  X_bs_df <- list()
  for (i in seq_along(vars)) {
    X_bs[[i]] <- bs(L11[[vars[i]]], df = df_each, intercept = TRUE)
    
    X_bs_df[[i]] <- X_bs[[i]] %>%
      as_tibble
    names(X_bs_df[[i]]) <- paste('bs', vars[[i]], 1:df_each, sep = '_')
  }
  X_bs_L11 <- bind_cols(X_bs_df)
  assert_that(!any(is.na(X_bs_L11)))


  X_bs <- list()
  X_bs_df <- list()
  for (i in seq_along(vars)) {
    X_bs[[i]] <- bs(L5[[vars[i]]], df = df_each, intercept = TRUE)
    
    X_bs_df[[i]] <- X_bs[[i]] %>%
      as_tibble
    names(X_bs_df[[i]]) <- paste('bs', vars[[i]], 1:df_each, sep = '_')
  }
  X_bs_L5 <- bind_cols(X_bs_df)
  assert_that(!any(is.na(X_bs_L5)))

  X_bs <- list()
  X_bs_df <- list()
  for (i in seq_along(vars)) {
    X_bs[[i]] <- bs(L6_1[[vars[i]]], df = df_each, intercept = TRUE)
    
    X_bs_df[[i]] <- X_bs[[i]] %>%
      as_tibble
    names(X_bs_df[[i]]) <- paste('bs', vars[[i]], 1:df_each, sep = '_')
  }
  X_bs_L6_1 <- bind_cols(X_bs_df)
  assert_that(!any(is.na(X_bs_L6_1)))

  X_bs <- list()
  X_bs_df <- list()
  for (i in seq_along(vars)) {
    X_bs[[i]] <- bs(L6_2[[vars[i]]], df = df_each, intercept = TRUE)
    
    X_bs_df[[i]] <- X_bs[[i]] %>%
      as_tibble
    names(X_bs_df[[i]]) <- paste('bs', vars[[i]], 1:df_each, sep = '_')
  }
  X_bs_L6_2 <- bind_cols(X_bs_df)
  assert_that(!any(is.na(X_bs_L6_2)))

  X_bs <- list()
  X_bs_df <- list()
  for (i in seq_along(vars)) {
    X_bs[[i]] <- bs(L7[[vars[i]]], df = df_each, intercept = TRUE)
    
    X_bs_df[[i]] <- X_bs[[i]] %>%
      as_tibble
    names(X_bs_df[[i]]) <- paste('bs', vars[[i]], 1:df_each, sep = '_')
  }
  X_bs_L7 <- bind_cols(X_bs_df)
  assert_that(!any(is.na(X_bs_L7)))

  X_bs <- list()
  X_bs_df <- list()
  for (i in seq_along(vars)) {
    X_bs[[i]] <- bs(L8_1[[vars[i]]], df = df_each, intercept = TRUE)
    
    X_bs_df[[i]] <- X_bs[[i]] %>%
      as_tibble
    names(X_bs_df[[i]]) <- paste('bs', vars[[i]], 1:df_each, sep = '_')
  }
  X_bs_L8_1 <- bind_cols(X_bs_df)
  assert_that(!any(is.na(X_bs_L8_1)))

  X_bs <- list()
  X_bs_df <- list()
  for (i in seq_along(vars)) {
    X_bs[[i]] <- bs(L8_2[[vars[i]]], df = df_each, intercept = TRUE)
    
    X_bs_df[[i]] <- X_bs[[i]] %>%
      as_tibble
    names(X_bs_df[[i]]) <- paste('bs', vars[[i]], 1:df_each, sep = '_')
  }
  X_bs_L8_2 <- bind_cols(X_bs_df)
  assert_that(!any(is.na(X_bs_L8_2)))

  X_bs <- list()
  X_bs_df <- list()
  for (i in seq_along(vars)) {
    X_bs[[i]] <- bs(L8_3[[vars[i]]], df = df_each, intercept = TRUE)
    
    X_bs_df[[i]] <- X_bs[[i]] %>%
      as_tibble
    names(X_bs_df[[i]]) <- paste('bs', vars[[i]], 1:df_each, sep = '_')
  }
  X_bs_L8_3 <- bind_cols(X_bs_df)
  assert_that(!any(is.na(X_bs_L8_3)))

  X_bs <- list()
  X_bs_df <- list()
  for (i in seq_along(vars)) {
    X_bs[[i]] <- bs(L9_1[[vars[i]]], df = df_each, intercept = TRUE)
    
    X_bs_df[[i]] <- X_bs[[i]] %>%
      as_tibble
    names(X_bs_df[[i]]) <- paste('bs', vars[[i]], 1:df_each, sep = '_')
  }
  X_bs_L9_1 <- bind_cols(X_bs_df)
  assert_that(!any(is.na(X_bs_L9_1)))

  X_bs <- list()
  X_bs_df <- list()
  for (i in seq_along(vars)) {
    X_bs[[i]] <- bs(L9_2[[vars[i]]], df = df_each, intercept = TRUE)
    
    X_bs_df[[i]] <- X_bs[[i]] %>%
      as_tibble
    names(X_bs_df[[i]]) <- paste('bs', vars[[i]], 1:df_each, sep = '_')
  }
  X_bs_L9_2 <- bind_cols(X_bs_df)
  assert_that(!any(is.na(X_bs_L9_2)))

  # Create design matrices --------------------------------------------------
  l4_terms <- paste0('US_L4NAME * ', names(X_bs_L10_1)) %>%
    paste(collapse = ' + ')
  l3_terms <- paste0('US_L3NAME * ', names(X_bs_df)) %>%
    paste(collapse = ' + ')
  l2_terms <- paste0('NA_L2NAME * ', names(X_bs_df)) %>%
    paste(collapse = ' + ')
  l1_terms <- paste0('NA_L1NAME * ', names(X_bs_df)) %>%
    paste(collapse = ' + ')

  L10_1 <- L10_1 %>%
    bind_cols(lapply(X_bs_L10_1, c)) %>%
    as_tibble
  L10_2 <- L10_2 %>%
    bind_cols(lapply(X_bs_L10_2, c)) %>%
    as_tibble
  L11 <- L11 %>%
    bind_cols(lapply(X_bs_L11, c)) %>%
    as_tibble
  L5 <- L5 %>%
    bind_cols(lapply(X_bs_L5, c)) %>%
    as_tibble
  L6_1 <- L6_1 %>%
    bind_cols(lapply(X_bs_L6_1, c)) %>%
    as_tibble
  L6_2 <- L6_2 %>%
    bind_cols(lapply(X_bs_L6_2, c)) %>%
    as_tibble
  L7 <- L7 %>%
    bind_cols(lapply(X_bs_L7, c)) %>%
    as_tibble
  L8_1 <- L8_1 %>%
    bind_cols(lapply(X_bs_L8_1, c)) %>%
    as_tibble
  L8_2 <- L8_2 %>%
    bind_cols(lapply(X_bs_L8_2, c)) %>%
    as_tibble
  L8_3 <- L8_3 %>%
    bind_cols(lapply(X_bs_L8_3, c)) %>%
    as_tibble
  L9_1 <- L9_1 %>%
    bind_cols(lapply(X_bs_L9_1, c)) %>%
    as_tibble
  L9_2 <- L9_2 %>%
    bind_cols(lapply(X_bs_L9_2, c)) %>%
    as_tibble

  X_L10_1 <- model.matrix(as.formula(paste('~ 0 + ',
                                    l4_terms,
                                    sep = ' + ')),
                    data = L10_1)
  X_L10_2 <- model.matrix(as.formula(paste('~ 0 + ',
                                          l4_terms,
                                          sep = ' + ')),
                          data = L10_2)
  X_L11 <- model.matrix(as.formula(paste('~ 0 + ',
                                          l4_terms,
                                          sep = ' + ')),
                          data = L11)

  X_L5 <- model.matrix(as.formula(paste('~ 0 + ',
                                        l4_terms,
                                        sep = ' + ')),
                        data = L5)
  X_L6_1 <- model.matrix(as.formula(paste('~ 0 + ',
                                          l4_terms,
                                          sep = ' + ')),
                          data = L6_1)
  X_L6_2 <- model.matrix(as.formula(paste('~ 0 + ',
                                          l4_terms,
                                          sep = ' + ')),
                          data = L6_2)
  X_L7 <- model.matrix(as.formula(paste('~ 0 + ',
                                        l4_terms,
                                        sep = ' + ')),
                        data = L7)
  X_L8_1 <- model.matrix(as.formula(paste('~ 0 + ',
                                          l4_terms,
                                          sep = ' + ')),
                          data = L8_1)
  X_L8_2 <- model.matrix(as.formula(paste('~ 0 + ',
                                          l4_terms,
                                          sep = ' + ')),
                          data = L8_2)
  X_L8_3 <- model.matrix(as.formula(paste('~ 0 + ',
                                          l4_terms,
                                          sep = ' + ')),
                        data = L8_3)
  X_L9_1 <- model.matrix(as.formula(paste('~ 0 + ',
                                          l4_terms,
                                          sep = ' + ')),
                          data = L9_1)
  X_L9_2 <- model.matrix(as.formula(paste('~ 0 + ',
                                          l4_terms,
                                          sep = ' + ')),
                          data = L9_2)

  sparse_XL10_1 <- extract_sparse_parts(X_L10_1)
  sparse_XL10_2 <- extract_sparse_parts(X_L10_2)
  sparse_XL11 <- extract_sparse_parts(X_L11)
  sparse_XL5 <- extract_sparse_parts(X_L5)
  sparse_XL6_1 <- extract_sparse_parts(X_L6_1)
  sparse_XL6_2 <- extract_sparse_parts(X_L6_2)
  sparse_XL7 <- extract_sparse_parts(X_L7)
  sparse_XL8_1 <- extract_sparse_parts(X_L8_1)
  sparse_XL8_2 <- extract_sparse_parts(X_L8_2)
  sparse_XL8_3 <- extract_sparse_parts(X_L8_3)
  sparse_XL9_1 <- extract_sparse_parts(X_L9_1)
  sparse_XL9_2 <- extract_sparse_parts(X_L9_2)

  colnamesXL10_1 <- colnames(X_L10_1)
  colnamesXL10_2 <- colnames(X_L10_2)
  colnamesXL11 <- colnames(X_L11)
  colnamesXL5 <- colnames(X_L5)
  colnamesXL6_1 <- colnames(X_L6_1)
  colnamesXL6_2 <- colnames(X_L6_2)
  colnamesXL7 <- colnames(X_L7)
  colnamesXL8_1 <- colnames(X_L8_1)
  colnamesXL8_2 <- colnames(X_L8_2)
  colnamesXL8_3 <- colnames(X_L8_3)
  colnamesXL9_1 <- colnames(X_L9_1)
  colnamesXL9_2 <- colnames(X_L9_2)

  # design matrix for training counts
  # is a subset of the rows of X, based on which rows show up in train_counts
  eps_idx_trainL10_1 <- match(train_countsL10_1$er_ym, L10_1$er_ym)
  eps_idx_trainL10_2 <- match(train_countsL10_2$er_ym, L10_2$er_ym)
  eps_idx_trainL11 <- match(train_countsL11$er_ym, L11$er_ym)
  eps_idx_trainL5 <- match(train_countsL5$er_ym, L5$er_ym)
  eps_idx_trainL6_1 <- match(train_countsL6_1$er_ym, L6_1$er_ym)
  eps_idx_trainL6_2 <- match(train_countsL6_2$er_ym, L6_2$er_ym)
  eps_idx_trainL7 <- match(train_countsL7$er_ym, L7$er_ym)
  eps_idx_trainL8_1 <- match(train_countsL8_1$er_ym, L8_1$er_ym)
  eps_idx_trainL8_2 <- match(train_countsL8_2$er_ym, L8_2$er_ym)
  eps_idx_trainL8_3 <- match(train_countsL8_3$er_ym, L8_3$er_ym)
  eps_idx_trainL9_1 <- match(train_countsL9_1$er_ym, L9_1$er_ym)
  eps_idx_trainL9_2 <- match(train_countsL9_2$er_ym, L9_2$er_ym)

  assert_that(all(diff(eps_idx_trainL10_1) == 1))
  assert_that(all(diff(eps_idx_trainL10_2) == 1))
  assert_that(all(diff(eps_idx_trainL11) == 1))
  assert_that(all(diff(eps_idx_trainL5) == 1))
  assert_that(all(diff(eps_idx_trainL6_1) == 1))
  assert_that(all(diff(eps_idx_trainL6_2) == 1))
  assert_that(all(diff(eps_idx_trainL7) == 1))
  assert_that(all(diff(eps_idx_trainL8_1) == 1))
  assert_that(all(diff(eps_idx_trainL8_2) == 1))
  assert_that(all(diff(eps_idx_trainL8_3) == 1))
  assert_that(all(diff(eps_idx_trainL9_1) == 1))
  assert_that(all(diff(eps_idx_trainL9_2) == 1))


  X_tcL10_1 <- X_L10_1[eps_idx_trainL10_1, ]
  X_tcL10_2 <- X_L10_2[eps_idx_trainL10_2, ]
  X_tcL11 <- X_L11[eps_idx_trainL11, ]
  X_tcL5 <- X_L5[eps_idx_trainL5, ]
  X_tcL6_1 <- X_L6_1[eps_idx_trainL6_1, ]
  X_tcL6_2 <- X_L6_2[eps_idx_trainL6_2, ]
  X_tcL7 <- X_L7[eps_idx_trainL7, ]
  X_tcL8_1 <- X_L8_1[eps_idx_trainL8_1, ]
  X_tcL8_2 <- X_L8_2[eps_idx_trainL8_2, ]
  X_tcL8_3 <- X_L8_3[eps_idx_trainL8_3, ]
  X_tcL9_1 <- X_L9_1[eps_idx_trainL9_1, ]
  X_tcL9_2 <- X_L9_2[eps_idx_trainL9_2, ]

  assert_that(identical(nrow(X_tcL10_1), nrow(train_countsL10_1)))
  assert_that(identical(nrow(X_tcL10_2), nrow(train_countsL10_2)))
  assert_that(identical(nrow(X_tcL11), nrow(train_countsL11)))
  assert_that(identical(nrow(X_tcL5), nrow(train_countsL5)))
  assert_that(identical(nrow(X_tcL6_1), nrow(train_countsL6_1)))
  assert_that(identical(nrow(X_tcL6_2), nrow(train_countsL6_2)))
  assert_that(identical(nrow(X_tcL7), nrow(train_countsL7)))
  assert_that(identical(nrow(X_tcL8_1), nrow(train_countsL8_1)))
  assert_that(identical(nrow(X_tcL8_2), nrow(train_countsL8_2)))
  assert_that(identical(nrow(X_tcL8_3), nrow(train_countsL8_3)))
  assert_that(identical(nrow(X_tcL9_1), nrow(train_countsL9_1)))
  assert_that(identical(nrow(X_tcL9_2), nrow(train_countsL9_2)))

  sparse_X_tcL10_1 <- extract_sparse_parts(X_tcL10_1)
  sparse_X_tcL10_2 <- extract_sparse_parts(X_tcL10_2)
  sparse_X_tcL11 <- extract_sparse_parts(X_tcL11)
  sparse_X_tcL5 <- extract_sparse_parts(X_tcL5)
  sparse_X_tcL6_1 <- extract_sparse_parts(X_tcL6_1)
  sparse_X_tcL6_2 <- extract_sparse_parts(X_tcL6_2)
  sparse_X_tcL7 <- extract_sparse_parts(X_tcL7)
  sparse_X_tcL8_1 <- extract_sparse_parts(X_tcL8_1)
  sparse_X_tcL8_2 <- extract_sparse_parts(X_tcL8_2)
  sparse_X_tcL8_3 <- extract_sparse_parts(X_tcL8_3)
  sparse_X_tcL9_1 <- extract_sparse_parts(X_tcL9_1)
  sparse_X_tcL9_2 <- extract_sparse_parts(X_tcL9_2)

  eps_idx_futureL10_1 <- setdiff(1:nrow(L10_1), eps_idx_trainL10_1)
  eps_idx_futureL10_2 <- setdiff(1:nrow(L10_2), eps_idx_trainL10_2)
  eps_idx_futureL11 <- setdiff(1:nrow(L11), eps_idx_trainL11)
  eps_idx_futureL5 <- setdiff(1:nrow(L5), eps_idx_trainL5)
  eps_idx_futureL6_1 <- setdiff(1:nrow(L6_1), eps_idx_trainL6_1)
  eps_idx_futureL6_2 <- setdiff(1:nrow(L6_2), eps_idx_trainL6_2)
  eps_idx_futureL7 <- setdiff(1:nrow(L7), eps_idx_trainL7)
  eps_idx_futureL8_1 <- setdiff(1:nrow(L8_1), eps_idx_trainL8_1)
  eps_idx_futureL8_2 <- setdiff(1:nrow(L8_2), eps_idx_trainL8_2)
  eps_idx_futureL8_3 <- setdiff(1:nrow(L8_3), eps_idx_trainL8_3)
  eps_idx_futureL9_1 <- setdiff(1:nrow(L9_1), eps_idx_trainL9_1)
  eps_idx_futureL9_2 <- setdiff(1:nrow(L9_2), eps_idx_trainL9_2)

  assert_that(all(diff(eps_idx_futureL10_1) == 1))
  assert_that(all(diff(eps_idx_futureL10_2) == 1))
  assert_that(all(diff(eps_idx_futureL11) == 1))
  assert_that(all(diff(eps_idx_futureL5) == 1))
  assert_that(all(diff(eps_idx_futureL6_1) == 1))
  assert_that(all(diff(eps_idx_futureL6_2) == 1))
  assert_that(all(diff(eps_idx_futureL7) == 1))
  assert_that(all(diff(eps_idx_futureL8_1) == 1))
  assert_that(all(diff(eps_idx_futureL8_2) == 1))
  assert_that(all(diff(eps_idx_futureL8_3) == 1))
  assert_that(all(diff(eps_idx_futureL9_1) == 1))
  assert_that(all(diff(eps_idx_futureL9_2) == 1))

  assert_that(eps_idx_trainL10_1[length(eps_idx_trainL10_1)] + 1 == eps_idx_futureL10_1[1])
  assert_that(eps_idx_trainL10_2[length(eps_idx_trainL10_2)] + 1 == eps_idx_futureL10_2[1])
  assert_that(eps_idx_trainL11[length(eps_idx_trainL11)] + 1 == eps_idx_futureL11[1])
  assert_that(eps_idx_trainL5[length(eps_idx_trainL5)] + 1 == eps_idx_futureL5[1])
  assert_that(eps_idx_trainL6_1[length(eps_idx_trainL6_1)] + 1 == eps_idx_futureL6_1[1])
  assert_that(eps_idx_trainL6_2[length(eps_idx_trainL6_2)] + 1 == eps_idx_futureL6_2[1])
  assert_that(eps_idx_trainL7[length(eps_idx_trainL7)] + 1 == eps_idx_futureL7[1])
  assert_that(eps_idx_trainL8_1[length(eps_idx_trainL8_1)] + 1 == eps_idx_futureL8_1[1])
  assert_that(eps_idx_trainL8_2[length(eps_idx_trainL8_2)] + 1 == eps_idx_futureL8_2[1])
  assert_that(eps_idx_trainL8_3[length(eps_idx_trainL8_3)] + 1 == eps_idx_futureL8_3[1])
  assert_that(eps_idx_trainL9_1[length(eps_idx_trainL9_1)] + 1 == eps_idx_futureL9_1[1])
  assert_that(eps_idx_trainL9_2[length(eps_idx_trainL9_2)] + 1 == eps_idx_futureL9_2[1])

  # design matrix for training burn areas
  # is a subset of X, based on which unique rows are in train_burns
  train_burn_covsL10_1 <- train_burnsL10_1 %>%
    distinct(er_ym, .keep_all = TRUE)
  train_burn_covsL10_2 <- train_burnsL10_2 %>%
    distinct(er_ym, .keep_all = TRUE)
  train_burn_covsL11 <- train_burnsL11 %>%
    distinct(er_ym, .keep_all = TRUE)
  train_burn_covsL5 <- train_burnsL5 %>%
    distinct(er_ym, .keep_all = TRUE)
  train_burn_covsL6_1 <- train_burnsL6_1 %>%
    distinct(er_ym, .keep_all = TRUE)
  train_burn_covsL6_2 <- train_burnsL6_2 %>%
    distinct(er_ym, .keep_all = TRUE)
  train_burn_covsL7 <- train_burnsL7 %>%
    distinct(er_ym, .keep_all = TRUE)
  train_burn_covsL8_1 <- train_burnsL8_1 %>%
    distinct(er_ym, .keep_all = TRUE)
  train_burn_covsL8_2 <- train_burnsL8_2 %>%
    distinct(er_ym, .keep_all = TRUE)
  train_burn_covsL8_3 <- train_burnsL8_3 %>%
    distinct(er_ym, .keep_all = TRUE)
  train_burn_covsL9_1 <- train_burnsL9_1 %>%
    distinct(er_ym, .keep_all = TRUE)
  train_burn_covsL9_2 <- train_burnsL9_2 %>%
    distinct(er_ym, .keep_all = TRUE)

  print(nrow(train_burn_covsL8_1))
  print(nrow(train_burnsL8_1))

  # train_burn_covs has no duplicate er_ym's: should be fewer rows than train_burns
  assert_that(nrow(train_burn_covsL10_1) < nrow(train_burnsL10_1))
  assert_that(nrow(train_burn_covsL10_2) < nrow(train_burnsL10_2))
  assert_that(nrow(train_burn_covsL11) < nrow(train_burnsL11))
  assert_that(nrow(train_burn_covsL5) < nrow(train_burnsL5))
  assert_that(nrow(train_burn_covsL6_1) < nrow(train_burnsL6_1))
  assert_that(nrow(train_burn_covsL6_2) < nrow(train_burnsL6_2))
  assert_that(nrow(train_burn_covsL7) < nrow(train_burnsL7))
  assert_that(nrow(train_burn_covsL8_1) <= nrow(train_burnsL8_1))
  assert_that(nrow(train_burn_covsL8_2) <= nrow(train_burnsL8_2))
  assert_that(nrow(train_burn_covsL8_3) <= nrow(train_burnsL8_3))
  assert_that(nrow(train_burn_covsL9_1) <= nrow(train_burnsL9_1))
  assert_that(nrow(train_burn_covsL9_2) <= nrow(train_burnsL9_2))


  tb_idxL10_1 <- match(train_burn_covsL10_1$er_ym, L10_1$er_ym)
  tb_idxL10_2 <- match(train_burn_covsL10_2$er_ym, L10_2$er_ym)
  tb_idxL11 <- match(train_burn_covsL11$er_ym, L11$er_ym)
  tb_idxL5 <- match(train_burn_covsL5$er_ym, L5$er_ym)
  tb_idxL6_1 <- match(train_burn_covsL6_1$er_ym, L6_1$er_ym)
  tb_idxL6_2 <- match(train_burn_covsL6_2$er_ym, L6_2$er_ym)
  tb_idxL7 <- match(train_burn_covsL7$er_ym, L7$er_ym)
  tb_idxL8_1 <- match(train_burn_covsL8_1$er_ym, L8_1$er_ym)
  tb_idxL8_2 <- match(train_burn_covsL8_2$er_ym, L8_2$er_ym)
  tb_idxL8_3 <- match(train_burn_covsL8_3$er_ym, L8_3$er_ym)
  tb_idxL9_1 <- match(train_burn_covsL9_1$er_ym, L9_1$er_ym)
  tb_idxL9_2 <- match(train_burn_covsL9_2$er_ym, L9_2$er_ym)

  X_tbL10_1 <- X_L10_1[tb_idxL10_1, ]
  X_tbL10_2 <- X_L10_2[tb_idxL10_2, ]
  X_tbL11 <- X_L11[tb_idxL11, ]
  X_tbL5 <- X_L5[tb_idxL5, ]
  X_tbL6_1 <- X_L6_1[tb_idxL6_1, ]
  X_tbL6_2 <- X_L6_2[tb_idxL6_2, ]
  X_tbL7 <- X_L7[tb_idxL7, ]
  X_tbL8_1 <- X_L8_1[tb_idxL8_1, ]
  X_tbL8_2 <- X_L8_2[tb_idxL8_2, ]
  X_tbL8_3 <- X_L8_3[tb_idxL8_3, ]
  X_tbL9_1 <- X_L9_1[tb_idxL9_1, ]
  X_tbL9_2 <- X_L9_2[tb_idxL9_2, ]

  assert_that(identical(nrow(X_tbL10_1), nrow(train_burn_covsL10_1)))
  assert_that(identical(nrow(X_tbL10_2), nrow(train_burn_covsL10_2)))
  assert_that(identical(nrow(X_tbL11), nrow(train_burn_covsL11)))
  assert_that(identical(nrow(X_tbL5), nrow(train_burn_covsL5)))
  assert_that(identical(nrow(X_tbL6_1), nrow(train_burn_covsL6_1)))
  assert_that(identical(nrow(X_tbL6_2), nrow(train_burn_covsL6_2)))
  assert_that(identical(nrow(X_tbL7), nrow(train_burn_covsL7)))
  assert_that(identical(nrow(X_tbL8_1), nrow(train_burn_covsL8_1)))
  assert_that(identical(nrow(X_tbL8_2), nrow(train_burn_covsL8_2)))
  assert_that(identical(nrow(X_tbL8_3), nrow(train_burn_covsL8_3)))
  assert_that(identical(nrow(X_tbL9_1), nrow(train_burn_covsL9_1)))
  assert_that(identical(nrow(X_tbL9_2), nrow(train_burn_covsL9_2)))


  sparse_X_tbL10_1 <- extract_sparse_parts(X_tbL10_1)
  sparse_X_tbL10_2 <- extract_sparse_parts(X_tbL10_2)
  sparse_X_tbL11 <- extract_sparse_parts(X_tbL11)
  sparse_X_tbL5 <- extract_sparse_parts(X_tbL5)
  sparse_X_tbL6_1 <- extract_sparse_parts(X_tbL6_1)
  sparse_X_tbL6_2 <- extract_sparse_parts(X_tbL6_2)
  sparse_X_tbL7 <- extract_sparse_parts(X_tbL7)
  sparse_X_tbL8_1 <- extract_sparse_parts(X_tbL8_1)
  sparse_X_tbL8_2 <- extract_sparse_parts(X_tbL8_2)
  sparse_X_tbL8_3 <- extract_sparse_parts(X_tbL8_3)
  sparse_X_tbL9_1 <- extract_sparse_parts(X_tbL9_1)
  sparse_X_tbL9_2 <- extract_sparse_parts(X_tbL9_2)


  # indices to match epsilon parameters for burn areas to those computed for counts
  burn_eps_idxL10_1 <- match(train_burn_covsL10_1$er_ym, train_countsL10_1$er_ym)
  burn_eps_idxL10_2 <- match(train_burn_covsL10_2$er_ym, train_countsL10_2$er_ym)
  burn_eps_idxL11 <- match(train_burn_covsL11$er_ym, train_countsL11$er_ym)
  burn_eps_idxL5 <- match(train_burn_covsL5$er_ym, train_countsL5$er_ym)
  burn_eps_idxL6_1 <- match(train_burn_covsL6_1$er_ym, train_countsL6_1$er_ym)
  burn_eps_idxL6_2 <- match(train_burn_covsL6_2$er_ym, train_countsL6_2$er_ym)
  burn_eps_idxL7 <- match(train_burn_covsL7$er_ym, train_countsL7$er_ym)
  burn_eps_idxL8_1 <- match(train_burn_covsL8_1$er_ym, train_countsL8_1$er_ym)
  burn_eps_idxL8_2 <- match(train_burn_covsL8_2$er_ym, train_countsL8_2$er_ym)
  burn_eps_idxL8_3 <- match(train_burn_covsL8_3$er_ym, train_countsL8_3$er_ym)
  burn_eps_idxL9_1 <- match(train_burn_covsL9_1$er_ym, train_countsL9_1$er_ym)
  burn_eps_idxL9_2 <- match(train_burn_covsL9_2$er_ym, train_countsL9_2$er_ym)

  assert_that(train_burn_covsL10_1$er_ym[1] == train_countsL10_1$er_ym[burn_eps_idxL10_1[1]])
  assert_that(train_burn_covsL10_2$er_ym[1] == train_countsL10_2$er_ym[burn_eps_idxL10_2[1]])
  assert_that(train_burn_covsL11$er_ym[1] == train_countsL11$er_ym[burn_eps_idxL11[1]])
  assert_that(train_burn_covsL5$er_ym[1] == train_countsL5$er_ym[burn_eps_idxL5[1]])
  assert_that(train_burn_covsL6_1$er_ym[1] == train_countsL6_1$er_ym[burn_eps_idxL6_1[1]])
  assert_that(train_burn_covsL6_2$er_ym[1] == train_countsL6_2$er_ym[burn_eps_idxL6_2[1]])
  assert_that(train_burn_covsL7$er_ym[1] == train_countsL7$er_ym[burn_eps_idxL7[1]])
  assert_that(train_burn_covsL8_1$er_ym[1] == train_countsL8_1$er_ym[burn_eps_idxL8_1[1]])
  assert_that(train_burn_covsL8_2$er_ym[1] == train_countsL8_2$er_ym[burn_eps_idxL8_2[1]])
  assert_that(train_burn_covsL8_3$er_ym[1] == train_countsL8_3$er_ym[burn_eps_idxL8_3[1]])
  assert_that(train_burn_covsL9_1$er_ym[1] == train_countsL9_1$er_ym[burn_eps_idxL9_1[1]])
  assert_that(train_burn_covsL9_2$er_ym[1] == train_countsL9_2$er_ym[burn_eps_idxL9_2[1]])

  # indices to match each fire event to a row in the design matrix for burns
  burn_idxL10_1 <- match(train_burnsL10_1$er_ym, train_burn_covsL10_1$er_ym)
  burn_idxL10_2 <- match(train_burnsL10_2$er_ym, train_burn_covsL10_2$er_ym)
  burn_idxL11 <- match(train_burnsL11$er_ym, train_burn_covsL11$er_ym)
  burn_idxL5 <- match(train_burnsL5$er_ym, train_burn_covsL5$er_ym)
  burn_idxL6_1 <- match(train_burnsL6_1$er_ym, train_burn_covsL6_1$er_ym)
  burn_idxL6_2 <- match(train_burnsL6_2$er_ym, train_burn_covsL6_2$er_ym)
  burn_idxL7 <- match(train_burnsL7$er_ym, train_burn_covsL7$er_ym)
  burn_idxL8_1 <- match(train_burnsL8_1$er_ym, train_burn_covsL8_1$er_ym)
  burn_idxL8_2 <- match(train_burnsL8_2$er_ym, train_burn_covsL8_2$er_ym)
  burn_idxL8_3 <- match(train_burnsL8_3$er_ym, train_burn_covsL8_3$er_ym)
  burn_idxL9_1 <- match(train_burnsL9_1$er_ym, train_burn_covsL9_1$er_ym)
  burn_idxL9_2 <- match(train_burnsL9_2$er_ym, train_burn_covsL9_2$er_ym)

  # check to make sure the indices were correct
  assert_that(max(burn_idxL10_1) <= nrow(L10_1))
  assert_that(max(burn_idxL10_2) <= nrow(L10_2))
  assert_that(max(burn_idxL11) <= nrow(L11))
  assert_that(max(burn_idxL5) <= nrow(L5))
  assert_that(max(burn_idxL6_1) <= nrow(L6_1))
  assert_that(max(burn_idxL6_2) <= nrow(L6_2))
  assert_that(max(burn_idxL7) <= nrow(L7))
  assert_that(max(burn_idxL8_1) <= nrow(L8_1))
  assert_that(max(burn_idxL8_2) <= nrow(L8_2))
  assert_that(max(burn_idxL8_3) <= nrow(L8_3))
  assert_that(max(burn_idxL9_1) <= nrow(L9_1))
  assert_that(max(burn_idxL9_2) <= nrow(L9_2))

  assert_that(all(train_burn_covsL10_1$US_L4NAME[burn_idxL10_1] == train_burnsL10_1$US_L4NAME))
  assert_that(all(train_burn_covsL10_2$US_L4NAME[burn_idxL10_2] == train_burnsL10_2$US_L4NAME))
  assert_that(all(train_burn_covsL11$US_L4NAME[burn_idxL11] == train_burnsL11$US_L4NAME))
  assert_that(all(train_burn_covsL5$US_L4NAME[burn_idxL5] == train_burnsL5$US_L4NAME))
  assert_that(all(train_burn_covsL6_1$US_L4NAME[burn_idxL6_1] == train_burnsL6_1$US_L4NAME))
  assert_that(all(train_burn_covsL6_2$US_L4NAME[burn_idxL6_2] == train_burnsL6_2$US_L4NAME))
  assert_that(all(train_burn_covsL7$US_L4NAME[burn_idxL7] == train_burnsL7$US_L4NAME))
  assert_that(all(train_burn_covsL8_1$US_L4NAME[burn_idxL8_1] == train_burnsL8_1$US_L4NAME))
  assert_that(all(train_burn_covsL8_2$US_L4NAME[burn_idxL8_2] == train_burnsL8_2$US_L4NAME))
  assert_that(all(train_burn_covsL8_3$US_L4NAME[burn_idxL8_3] == train_burnsL8_3$US_L4NAME))
  assert_that(all(train_burn_covsL9_1$US_L4NAME[burn_idxL9_1] == train_burnsL9_1$US_L4NAME))
  assert_that(all(train_burn_covsL9_2$US_L4NAME[burn_idxL9_2] == train_burnsL9_2$US_L4NAME))

  assert_that(all(train_burn_covsL10_1$ym[burn_idxL10_1] == train_burnsL10_1$ym))
  assert_that(all(train_burn_covsL10_2$ym[burn_idxL10_2] == train_burnsL10_2$ym))
  assert_that(all(train_burn_covsL11$ym[burn_idxL11] == train_burnsL11$ym))
  assert_that(all(train_burn_covsL5$ym[burn_idxL5] == train_burnsL5$ym))
  assert_that(all(train_burn_covsL6_1$ym[burn_idxL6_1] == train_burnsL6_1$ym))
  assert_that(all(train_burn_covsL6_2$ym[burn_idxL6_2] == train_burnsL6_2$ym))
  assert_that(all(train_burn_covsL7$ym[burn_idxL7] == train_burnsL7$ym))
  assert_that(all(train_burn_covsL8_1$ym[burn_idxL8_1] == train_burnsL8_1$ym))
  assert_that(all(train_burn_covsL8_2$ym[burn_idxL8_2] == train_burnsL8_2$ym))
  assert_that(all(train_burn_covsL8_3$ym[burn_idxL8_3] == train_burnsL8_3$ym))
  assert_that(all(train_burn_covsL9_1$ym[burn_idxL9_1] == train_burnsL9_1$ym))
  assert_that(all(train_burn_covsL9_2$ym[burn_idxL9_2] == train_burnsL9_2$ym))

  rm(X_L10_1)
  rm(X_L10_2)
  rm(X_L11)
  rm(X_L5)
  rm(X_L6_1)
  rm(X_L6_2)
  rm(X_L7)
  rm(X_L8_1)
  rm(X_L8_2)
  rm(X_L8_3)
  rm(X_L9_1)
  rm(X_L9_2)
  rm(X_tcL10_1)
  rm(X_tcL10_2)
  rm(X_tcL11)
  rm(X_tcL5)
  rm(X_tcL6_1)
  rm(X_tcL6_2)
  rm(X_tcL7)
  rm(X_tcL8_1)
  rm(X_tcL8_2)
  rm(X_tcL8_3)
  rm(X_tcL9_1)
  rm(X_tcL9_2)

  rm(X_tbL10_1)
  rm(X_tbL10_2)
  rm(X_tbL11)
  rm(X_tbL5)
  rm(X_tbL6_1)
  rm(X_tbL6_2)
  rm(X_tbL7)
  rm(X_tbL8_1)
  rm(X_tbL8_2)
  rm(X_tbL8_3)
  rm(X_tbL9_1)
  rm(X_tbL9_2)

  area_df<- area_df %>%
  left_join(er_df) %>%
  droplevels %>%
  filter(!(US_L4NAME %in% c("Northern Channel Islands", "Southern Channel Islands")))

  area_df <- na.omit(area_df)

  area_L10_1<-area_df%>%
    filter(NA_L2CODE==10.1) %>%
    filter((US_L4NAME %in% unique(L10_1$US_L4NAME)))
  area_L10_2<-area_df%>%
    filter(NA_L2CODE %in% c("10.2", "12.1","13.1")) %>%
    distinct(US_L4NAME, .keep_all = TRUE) %>%
    filter((US_L4NAME %in% unique(L10_2$US_L4NAME)))
  area_L11<-area_df%>%
    filter(NA_L1CODE==11)
  area_L5<-area_df%>%
    filter(NA_L1CODE==5)
  area_L6_1<-area_df%>%
    filter(NA_L3CODE>= "6.2.3" & NA_L3CODE<= "6.2.9")
  area_L6_2<-area_df%>%
    filter(NA_L3CODE>= "6.2.10" & NA_L3CODE<= "6.2.15") %>%
    filter((US_L4NAME %in% unique(L6_2$US_L4NAME)))
  area_L7<-area_df%>%
    filter(NA_L1CODE==7)
  area_L8_1<-area_df%>%
    filter(NA_L2CODE>= 8.1 & NA_L2CODE<= 8.2)
  area_L8_2<-area_df%>%
    filter(NA_L2CODE==8.3) %>%
    filter((US_L4NAME %in% unique(L8_2$US_L4NAME)))
  area_L8_3<-area_df%>%
    filter(NA_L2CODE %in% c("8.4", "8.5","15.4"))%>%
    distinct(US_L4NAME, .keep_all = TRUE) %>%
    filter((US_L4NAME %in% unique(L8_3$US_L4NAME)))
  area_L9_1<-area_df%>%
    filter(NA_L2CODE>= 9.2 & NA_L2CODE<= 9.3)
  area_L9_2<-area_df%>%
    filter(NA_L2CODE>= 9.4 & NA_L2CODE<= 9.6)%>%
    distinct(US_L4NAME, .keep_all = TRUE)

  assert_that(identical(unique(area_L10_1$US_L4NAME), unique(L10_1$US_L4NAME)))
  assert_that(identical(unique(area_L10_2$US_L4NAME), unique(L10_2$US_L4NAME)))
  assert_that(identical(unique(area_L11$US_L4NAME), unique(L11$US_L4NAME)))
  assert_that(identical(unique(area_L5$US_L4NAME),unique(L5$US_L4NAME)))
  assert_that(identical(unique(area_L6_1$US_L4NAME), unique(L6_1$US_L4NAME)))
  assert_that(identical(unique(area_L6_2$US_L4NAME), unique(L6_2$US_L4NAME)))
  assert_that(identical(unique(area_L7$US_L4NAME),unique(L7$US_L4NAME)))
  assert_that(identical(unique(area_L8_1$US_L4NAME), unique(L8_1$US_L4NAME)))
  assert_that(identical(unique(area_L8_2$US_L4NAME), unique(L8_2$US_L4NAME)))
  assert_that(identical(unique(area_L8_3$US_L4NAME), unique(L8_3$US_L4NAME)))
  assert_that(identical(unique(area_L9_1$US_L4NAME), unique(L9_1$US_L4NAME)))
  assert_that(identical(unique(area_L9_2$US_L4NAME), unique(L9_2$US_L4NAME)))

  assert_that(identical(levels(factor(area_L10_1$US_L4NAME)),
                        levels(factor(L10_1$US_L4NAME))))
  assert_that(identical(levels(factor(area_L10_2$US_L4NAME)),
                        levels(factor(L10_2$US_L4NAME))))
  assert_that(identical(levels(factor(area_L11$US_L4NAME)),
                        levels(factor(L11$US_L4NAME))))


  min_size <- 1e3
  # Bundle up data into a list too pass to Stan -----------------------------

  stan_L10_1 <- list(
    N = N_L10_1,
    T = T,
    p = length(colnamesXL10_1),
    
    n_count = nrow(train_countsL10_1),
    counts = train_countsL10_1$n_fire,
    
    log_area = log(area_L10_1$area * 1e-11) / 2,
    er_idx_train = as.numeric(factor(train_countsL10_1$US_L4NAME,
                                    levels = levels(factor(area_L10_1$US_L4NAME)))),
    er_idx_full = as.numeric(factor(L10_1$US_L4NAME)),
    
    n_fire = nrow(train_burnsL10_1),
    sizes = train_burnsL10_1$BurnBndAc - min_size,
    burn_idx = burn_idxL10_1,
    
    n_w = length(sparse_XL10_1$w),
    w = sparse_XL10_1$w,
    v = sparse_XL10_1$v,
    u = sparse_XL10_1$u,

    # sparse design matrix for training counts
    n_w_tc = length(sparse_X_tcL10_1$w),
    w_tc = sparse_X_tcL10_1$w,
    v_tc = sparse_X_tcL10_1$v,
    n_u_tc = length(sparse_X_tcL10_1$u),
    u_tc = sparse_X_tcL10_1$u,

    # sparse design matrix for training burns
    n_w_tb = length(sparse_X_tbL10_1$w),
    w_tb = sparse_X_tbL10_1$w,
    v_tb = sparse_X_tbL10_1$v,
    n_u_tb = length(sparse_X_tbL10_1$u),
    u_tb = sparse_X_tbL10_1$u,
    
    burn_eps_idx = burn_eps_idxL10_1,
    
    M = 1,
    slab_df = 5,
    slab_scale = 2,
    
    eps_idx_train = eps_idx_trainL10_1,
    eps_idx_future = eps_idx_futureL10_1,
    size_threshold = 0,
    
    n_holdout_c = length(holdout_cidL10_1),
    holdout_c_idx = holdout_cidL10_1,
    holdout_c = holdout_countsL10_1$n_fire,
    
    n_holdout_b = length(holdout_bidL10_1),
    holdout_b_idx = holdout_bidL10_1,
    holdout_b = holdout_burnsL10_1$BurnBndAc - min_size,
    
    min_size = min_size,
    
    n_edges = length(B_L10_1@i),
    node1 = B_L10_1@i + 1, # add one to offset zero-based index
    node2 = B_L10_1@j + 1,
    tb_idx = tb_idxL10_1, 
    cutoff_year = cutoff_year
  )

  # assert that there are no missing values in stan_d
  assert_that(!any(lapply(stan_L10_1, function(x) any(is.na(x))) %>% unlist))

  stan_L10_2 <- list(
    N = N_L10_2,
    T = T,
    p = length(colnamesXL10_2),
    
    n_count = nrow(train_countsL10_2),
    counts = train_countsL10_2$n_fire,
    
    log_area = log(area_L10_2$area * 1e-11) / 2,
    er_idx_train = as.numeric(factor(train_countsL10_2$US_L4NAME,
                                    levels = levels(factor(area_L10_2$US_L4NAME)))),
    er_idx_full = as.numeric(factor(L10_2$US_L4NAME)),
    
    n_fire = nrow(train_burnsL10_2),
    sizes = train_burnsL10_2$BurnBndAc - min_size,
    burn_idx = burn_idxL10_2,
    
    
    n_w = length(sparse_XL10_2$w),
    w = sparse_XL10_2$w,
    v = sparse_XL10_2$v,
    u = sparse_XL10_2$u,
    
    
    # sparse design matrix for training counts
    n_w_tc = length(sparse_X_tcL10_2$w),
    w_tc = sparse_X_tcL10_2$w,
    v_tc = sparse_X_tcL10_2$v,
    n_u_tc = length(sparse_X_tcL10_2$u),
    u_tc = sparse_X_tcL10_2$u,
    
    # sparse design matrix for training burns
    n_w_tb = length(sparse_X_tbL10_2$w),
    w_tb = sparse_X_tbL10_2$w,
    v_tb = sparse_X_tbL10_2$v,
    n_u_tb = length(sparse_X_tbL10_2$u),
    u_tb = sparse_X_tbL10_2$u,
    
    burn_eps_idx = burn_eps_idxL10_2,
    
    M = 1,
    slab_df = 5,
    slab_scale = 2,
    
    eps_idx_train = eps_idx_trainL10_2,
    eps_idx_future = eps_idx_futureL10_2,
    size_threshold = 0,
    
    n_holdout_c = length(holdout_cidL10_2),
    holdout_c_idx = holdout_cidL10_2,
    holdout_c = holdout_countsL10_2$n_fire,
    
    n_holdout_b = length(holdout_bidL10_2),
    holdout_b_idx = holdout_bidL10_2,
    holdout_b = holdout_burnsL10_2$BurnBndAc - min_size,
    
    min_size = min_size,
    
    n_edges = length(B_L10_2@i),
    node1 = B_L10_2@i + 1, # add one to offset zero-based index
    node2 = B_L10_2@j + 1,
    tb_idx = tb_idxL10_2, 
    cutoff_year = cutoff_year
  )
  # assert that there are no missing values in stan_d
  assert_that(!any(lapply(stan_L10_2, function(x) any(is.na(x))) %>% unlist))

  stan_L11 <- list(
    N = N_L11,
    T = T,
    p = length(colnamesXL11),
    
    n_count = nrow(train_countsL11),
    counts = train_countsL11$n_fire,
    
    log_area = log(area_L11$area * 1e-11) / 2,
    er_idx_train = as.numeric(factor(train_countsL11$US_L4NAME,
                                    levels = levels(factor(area_L11$US_L4NAME)))),
    er_idx_full = as.numeric(factor(L11$US_L4NAME)),
    
    n_fire = nrow(train_burnsL11),
    sizes = train_burnsL11$BurnBndAc - min_size,
    burn_idx = burn_idxL11,
    
    
    n_w = length(sparse_XL11$w),
    w = sparse_XL11$w,
    v = sparse_XL11$v,
    u = sparse_XL11$u,
    
    
    # sparse design matrix for training counts
    n_w_tc = length(sparse_X_tcL11$w),
    w_tc = sparse_X_tcL11$w,
    v_tc = sparse_X_tcL11$v,
    n_u_tc = length(sparse_X_tcL11$u),
    u_tc = sparse_X_tcL11$u,
    
    # sparse design matrix for training burns
    n_w_tb = length(sparse_X_tbL11$w),
    w_tb = sparse_X_tbL11$w,
    v_tb = sparse_X_tbL11$v,
    n_u_tb = length(sparse_X_tbL11$u),
    u_tb = sparse_X_tbL11$u,
    
    burn_eps_idx = burn_eps_idxL11,
    
    M = 1,
    slab_df = 5,
    slab_scale = 2,
    
    eps_idx_train = eps_idx_trainL11,
    eps_idx_future = eps_idx_futureL11,
    size_threshold = 0,
    
    n_holdout_c = length(holdout_cidL11),
    holdout_c_idx = holdout_cidL11,
    holdout_c = holdout_countsL11$n_fire,
    
    n_holdout_b = length(holdout_bidL11),
    holdout_b_idx = holdout_bidL11,
    holdout_b = holdout_burnsL11$BurnBndAc - min_size,
    
    min_size = min_size,
    
    n_edges = length(B_L11@i),
    node1 = B_L11@i + 1, # add one to offset zero-based index
    node2 = B_L11@j + 1,
    tb_idx = tb_idxL11, 
    cutoff_year = cutoff_year
  )
  # assert that there are no missing values in stan_d
  assert_that(!any(lapply(stan_L11, function(x) any(is.na(x))) %>% unlist))


  stan_L5 <- list(
    N = N_L5,
    T = T,
    p = length(colnamesXL5),
    
    n_count = nrow(train_countsL5),
    counts = train_countsL5$n_fire,
    
    log_area = log(area_L5$area * 1e-11) / 2,
    er_idx_train = as.numeric(factor(train_countsL5$US_L4NAME,
                                    levels = levels(factor(area_L5$US_L4NAME)))),
    er_idx_full = as.numeric(factor(L5$US_L4NAME)),
    
    n_fire = nrow(train_burnsL5),
    sizes = train_burnsL5$BurnBndAc - min_size,
    burn_idx = burn_idxL5,
    
    n_w = length(sparse_XL5$w),
    w = sparse_XL5$w,
    v = sparse_XL5$v,
    u = sparse_XL5$u,
    
    # sparse design matrix for training counts
    n_w_tc = length(sparse_X_tcL5$w),
    w_tc = sparse_X_tcL5$w,
    v_tc = sparse_X_tcL5$v,
    n_u_tc = length(sparse_X_tcL5$u),
    u_tc = sparse_X_tcL5$u,
    
    # sparse design matrix for training burns
    n_w_tb = length(sparse_X_tbL5$w),
    w_tb = sparse_X_tbL5$w,
    v_tb = sparse_X_tbL5$v,
    n_u_tb = length(sparse_X_tbL5$u),
    u_tb = sparse_X_tbL5$u,
    
    burn_eps_idx = burn_eps_idxL5,
    
    M = 1,
    slab_df = 5,
    slab_scale = 2,
    
    eps_idx_train = eps_idx_trainL5,
    eps_idx_future = eps_idx_futureL5,
    size_threshold = 0,
    
    n_holdout_c = length(holdout_cidL5),
    holdout_c_idx = holdout_cidL5,
    holdout_c = holdout_countsL5$n_fire,
    
    n_holdout_b = length(holdout_bidL5),
    holdout_b_idx = holdout_bidL5,
    holdout_b = holdout_burnsL5$BurnBndAc - min_size,
    
    min_size = min_size,
    
    n_edges = length(B_L5@i),
    node1 = B_L5@i + 1, # add one to offset zero-based index
    node2 = B_L5@j + 1,
    tb_idx = tb_idxL5, 
    cutoff_year = cutoff_year
  )
  # assert that there are no missing values in stan_d
  assert_that(!any(lapply(stan_L5, function(x) any(is.na(x))) %>% unlist))

  stan_L6_1 <- list(
    N = N_L6_1,
    T = T,
    p = length(colnamesXL6_1),
    
    n_count = nrow(train_countsL6_1),
    counts = train_countsL6_1$n_fire,
    
    log_area = log(area_L6_1$area * 1e-11) / 2,
    er_idx_train = as.numeric(factor(train_countsL6_1$US_L4NAME,
                                    levels = levels(factor(area_L6_1$US_L4NAME)))),
    er_idx_full = as.numeric(factor(L6_1$US_L4NAME)),
    
    n_fire = nrow(train_burnsL6_1),
    sizes = train_burnsL6_1$BurnBndAc - min_size,
    burn_idx = burn_idxL6_1,
    
    n_w = length(sparse_XL6_1$w),
    w = sparse_XL6_1$w,
    v = sparse_XL6_1$v,
    u = sparse_XL6_1$u,
    
    # sparse design matrix for training counts
    n_w_tc = length(sparse_X_tcL6_1$w),
    w_tc = sparse_X_tcL6_1$w,
    v_tc = sparse_X_tcL6_1$v,
    n_u_tc = length(sparse_X_tcL6_1$u),
    u_tc = sparse_X_tcL6_1$u,
    
    # sparse design matrix for training burns
    n_w_tb = length(sparse_X_tbL6_1$w),
    w_tb = sparse_X_tbL6_1$w,
    v_tb = sparse_X_tbL6_1$v,
    n_u_tb = length(sparse_X_tbL6_1$u),
    u_tb = sparse_X_tbL6_1$u,
    
    burn_eps_idx = burn_eps_idxL6_1,
    
    M = 1,
    slab_df = 5,
    slab_scale = 2,
    
    eps_idx_train = eps_idx_trainL6_1,
    eps_idx_future = eps_idx_futureL6_1,
    size_threshold = 0,
    
    n_holdout_c = length(holdout_cidL6_1),
    holdout_c_idx = holdout_cidL6_1,
    holdout_c = holdout_countsL6_1$n_fire,
    
    n_holdout_b = length(holdout_bidL6_1),
    holdout_b_idx = holdout_bidL6_1,
    holdout_b = holdout_burnsL6_1$BurnBndAc - min_size,
    
    min_size = min_size,
    
    n_edges = length(B_L6_1@i),
    node1 = B_L6_1@i + 1, # add one to offset zero-based index
    node2 = B_L6_1@j + 1,
    tb_idx = tb_idxL6_1, 
    cutoff_year = cutoff_year
  )

  # assert that there are no missing values in stan_d
  assert_that(!any(lapply(stan_L6_1, function(x) any(is.na(x))) %>% unlist))

  stan_L6_2 <- list(
    N = N_L6_2,
    T = T,
    p = length(colnamesXL6_2),
    
    n_count = nrow(train_countsL6_2),
    counts = train_countsL6_2$n_fire,
    
    log_area = log(area_L6_2$area * 1e-11) / 2,
    er_idx_train = as.numeric(factor(train_countsL6_2$US_L4NAME,
                                    levels = levels(factor(area_L6_2$US_L4NAME)))),
    er_idx_full = as.numeric(factor(L6_2$US_L4NAME)),
    
    n_fire = nrow(train_burnsL6_2),
    sizes = train_burnsL6_2$BurnBndAc - min_size,
    burn_idx = burn_idxL6_2,
    
    
    n_w = length(sparse_XL6_2$w),
    w = sparse_XL6_2$w,
    v = sparse_XL6_2$v,
    u = sparse_XL6_2$u,
    
    
    # sparse design matrix for training counts
    n_w_tc = length(sparse_X_tcL6_2$w),
    w_tc = sparse_X_tcL6_2$w,
    v_tc = sparse_X_tcL6_2$v,
    n_u_tc = length(sparse_X_tcL6_2$u),
    u_tc = sparse_X_tcL6_2$u,
    
    # sparse design matrix for training burns
    n_w_tb = length(sparse_X_tbL6_2$w),
    w_tb = sparse_X_tbL6_2$w,
    v_tb = sparse_X_tbL6_2$v,
    n_u_tb = length(sparse_X_tbL6_2$u),
    u_tb = sparse_X_tbL6_2$u,
    
    burn_eps_idx = burn_eps_idxL6_2,
    
    M = 1,
    slab_df = 5,
    slab_scale = 2,
    
    eps_idx_train = eps_idx_trainL6_2,
    eps_idx_future = eps_idx_futureL6_2,
    size_threshold = 0,
    
    n_holdout_c = length(holdout_cidL6_2),
    holdout_c_idx = holdout_cidL6_2,
    holdout_c = holdout_countsL6_2$n_fire,
    
    n_holdout_b = length(holdout_bidL6_2),
    holdout_b_idx = holdout_bidL6_2,
    holdout_b = holdout_burnsL6_2$BurnBndAc - min_size,
    
    min_size = min_size,
    
    n_edges = length(B_L6_2@i),
    node1 = B_L6_2@i + 1, # add one to offset zero-based index
    node2 = B_L6_2@j + 1,
    tb_idx = tb_idxL6_2, 
    cutoff_year = cutoff_year
  )

  # assert that there are no missing values in stan_d
  assert_that(!any(lapply(stan_L6_2, function(x) any(is.na(x))) %>% unlist))

  # Bundle up data into a list too pass to Stan -----------------------------
  min_size <- 1e3
  stan_L7 <- list(
    N = N_L7,
    T = T,
    p = length(colnamesXL7),
    
    n_count = nrow(train_countsL7),
    counts = train_countsL7$n_fire,
    
    log_area = log(area_L7$area * 1e-11) / 2,
    er_idx_train = as.numeric(factor(train_countsL7$US_L4NAME,
                                    levels = levels(factor(area_L7$US_L4NAME)))),
    er_idx_full = as.numeric(factor(L7$US_L4NAME)),
    
    n_fire = nrow(train_burnsL7),
    sizes = train_burnsL7$BurnBndAc - min_size,
    burn_idx = burn_idxL7,
    
    n_w = length(sparse_XL7$w),
    w = sparse_XL7$w,
    v = sparse_XL7$v,
    u = sparse_XL7$u,
    
    # sparse design matrix for training counts
    n_w_tc = length(sparse_X_tcL7$w),
    w_tc = sparse_X_tcL7$w,
    v_tc = sparse_X_tcL7$v,
    n_u_tc = length(sparse_X_tcL7$u),
    u_tc = sparse_X_tcL7$u,
    
    # sparse design matrix for training burns
    n_w_tb = length(sparse_X_tbL7$w),
    w_tb = sparse_X_tbL7$w,
    v_tb = sparse_X_tbL7$v,
    n_u_tb = length(sparse_X_tbL7$u),
    u_tb = sparse_X_tbL7$u,
    
    burn_eps_idx = burn_eps_idxL7,
    
    M = 1,
    slab_df = 5,
    slab_scale = 2,
    
    eps_idx_train = eps_idx_trainL7,
    eps_idx_future = eps_idx_futureL7,
    size_threshold = 0,
    
    n_holdout_c = length(holdout_cidL7),
    holdout_c_idx = holdout_cidL7,
    holdout_c = holdout_countsL7$n_fire,
    
    n_holdout_b = length(holdout_bidL7),
    holdout_b_idx = holdout_bidL7,
    holdout_b = holdout_burnsL7$BurnBndAc - min_size,
    
    min_size = min_size,
    
    n_edges = length(B_L7@i),
    node1 = B_L7@i + 1, # add one to offset zero-based index
    node2 = B_L7@j + 1,
    tb_idx = tb_idxL7, 
    cutoff_year = cutoff_year
  )

  # assert that there are no missing values in stan_d
  assert_that(!any(lapply(stan_L7, function(x) any(is.na(x))) %>% unlist))


  stan_L8_1 <- list(
    N = N_L8_1,
    T = T,
    p = length(colnamesXL8_1),
    
    n_count = nrow(train_countsL8_1),
    counts = train_countsL8_1$n_fire,
    
    log_area = log(area_L8_1$area * 1e-11) / 2,
    er_idx_train = as.numeric(factor(train_countsL8_1$US_L4NAME,
                                    levels = levels(factor(area_L8_1$US_L4NAME)))),
    er_idx_full = as.numeric(factor(L8_1$US_L4NAME)),
    
    n_fire = nrow(train_burnsL8_1),
    sizes = train_burnsL8_1$BurnBndAc - min_size,
    burn_idx = burn_idxL8_1,
    
    n_w = length(sparse_XL8_1$w),
    w = sparse_XL8_1$w,
    v = sparse_XL8_1$v,
    u = sparse_XL8_1$u,
    
    # sparse design matrix for training counts
    n_w_tc = length(sparse_X_tcL8_1$w),
    w_tc = sparse_X_tcL8_1$w,
    v_tc = sparse_X_tcL8_1$v,
    n_u_tc = length(sparse_X_tcL8_1$u),
    u_tc = sparse_X_tcL8_1$u,
    
    # sparse design matrix for training burns
    n_w_tb = length(sparse_X_tbL8_1$w),
    w_tb = sparse_X_tbL8_1$w,
    v_tb = sparse_X_tbL8_1$v,
    n_u_tb = length(sparse_X_tbL8_1$u),
    u_tb = sparse_X_tbL8_1$u,
    
    burn_eps_idx = burn_eps_idxL8_1,
    
    M = 1,
    slab_df = 5,
    slab_scale = 2,
    
    eps_idx_train = eps_idx_trainL8_1,
    eps_idx_future = eps_idx_futureL8_1,
    size_threshold = 0,
    
    n_holdout_c = length(holdout_cidL8_1),
    holdout_c_idx = holdout_cidL8_1,
    holdout_c = holdout_countsL8_1$n_fire,
    
    n_holdout_b = length(holdout_bidL8_1),
    holdout_b_idx = holdout_bidL8_1,
    holdout_b = holdout_burnsL8_1$BurnBndAc - min_size,
    
    min_size = min_size,
    
    n_edges = length(B_L8_1@i),
    node1 = B_L8_1@i + 1, # add one to offset zero-based index
    node2 = B_L8_1@j + 1,
    tb_idx = tb_idxL8_1, 
    cutoff_year = cutoff_year
  )

  # assert that there are no missing values in stan_d
  assert_that(!any(lapply(stan_L8_1, function(x) any(is.na(x))) %>% unlist))

  stan_L8_2 <- list(
    N = N_L8_2,
    T = T,
    p = length(colnamesXL8_2),
    
    n_count = nrow(train_countsL8_2),
    counts = train_countsL8_2$n_fire,
    
    log_area = log(area_L8_2$area * 1e-11) / 2,
    er_idx_train = as.numeric(factor(train_countsL8_2$US_L4NAME,
                                    levels = levels(factor(area_L8_2$US_L4NAME)))),
    er_idx_full = as.numeric(factor(L8_2$US_L4NAME)),
    
    n_fire = nrow(train_burnsL8_2),
    sizes = train_burnsL8_2$BurnBndAc - min_size,
    burn_idx = burn_idxL8_2,
    
    
    n_w = length(sparse_XL8_2$w),
    w = sparse_XL8_2$w,
    v = sparse_XL8_2$v,
    u = sparse_XL8_2$u,
    
    
    # sparse design matrix for training counts
    n_w_tc = length(sparse_X_tcL8_2$w),
    w_tc = sparse_X_tcL8_2$w,
    v_tc = sparse_X_tcL8_2$v,
    n_u_tc = length(sparse_X_tcL8_2$u),
    u_tc = sparse_X_tcL8_2$u,
    
    # sparse design matrix for training burns
    n_w_tb = length(sparse_X_tbL8_2$w),
    w_tb = sparse_X_tbL8_2$w,
    v_tb = sparse_X_tbL8_2$v,
    n_u_tb = length(sparse_X_tbL8_2$u),
    u_tb = sparse_X_tbL8_2$u,
    
    burn_eps_idx = burn_eps_idxL8_2,
    
    M = 1,
    slab_df = 5,
    slab_scale = 2,
    
    eps_idx_train = eps_idx_trainL8_2,
    eps_idx_future = eps_idx_futureL8_2,
    size_threshold = 0,
    
    n_holdout_c = length(holdout_cidL8_2),
    holdout_c_idx = holdout_cidL8_2,
    holdout_c = holdout_countsL8_2$n_fire,
    
    n_holdout_b = length(holdout_bidL8_2),
    holdout_b_idx = holdout_bidL8_2,
    holdout_b = holdout_burnsL8_2$BurnBndAc - min_size,
    
    min_size = min_size,
    
    n_edges = length(B_L8_2@i),
    node1 = B_L8_2@i + 1, # add one to offset zero-based index
    node2 = B_L8_2@j + 1,
    tb_idx = tb_idxL8_2, 
    cutoff_year = cutoff_year
  )

  # assert that there are no missing values in stan_d
  assert_that(!any(lapply(stan_L8_2, function(x) any(is.na(x))) %>% unlist))


  stan_L8_3 <- list(
    N = N_L8_3,
    T = T,
    p = length(colnamesXL8_3),
    
    n_count = nrow(train_countsL8_3),
    counts = train_countsL8_3$n_fire,
    
    log_area = log(area_L8_3$area * 1e-11) / 2,
    er_idx_train = as.numeric(factor(train_countsL8_3$US_L4NAME,
                                    levels = levels(factor(area_L8_3$US_L4NAME)))),
    er_idx_full = as.numeric(factor(L8_3$US_L4NAME)),
    
    n_fire = nrow(train_burnsL8_3),
    sizes = train_burnsL8_3$BurnBndAc - min_size,
    burn_idx = burn_idxL8_3,
    
    
    n_w = length(sparse_XL8_3$w),
    w = sparse_XL8_3$w,
    v = sparse_XL8_3$v,
    u = sparse_XL8_3$u,
    
    
    # sparse design matrix for training counts
    n_w_tc = length(sparse_X_tcL8_3$w),
    w_tc = sparse_X_tcL8_3$w,
    v_tc = sparse_X_tcL8_3$v,
    n_u_tc = length(sparse_X_tcL8_3$u),
    u_tc = sparse_X_tcL8_3$u,
    
    # sparse design matrix for training burns
    n_w_tb = length(sparse_X_tbL8_3$w),
    w_tb = sparse_X_tbL8_3$w,
    v_tb = sparse_X_tbL8_3$v,
    n_u_tb = length(sparse_X_tbL8_3$u),
    u_tb = sparse_X_tbL8_3$u,
    
    burn_eps_idx = burn_eps_idxL8_3,
    
    M = 1,
    slab_df = 5,
    slab_scale = 2,
    
    eps_idx_train = eps_idx_trainL8_3,
    eps_idx_future = eps_idx_futureL8_3,
    size_threshold = 0,
    
    n_holdout_c = length(holdout_cidL8_3),
    holdout_c_idx = holdout_cidL8_3,
    holdout_c = holdout_countsL8_3$n_fire,
    
    n_holdout_b = length(holdout_bidL8_3),
    holdout_b_idx = holdout_bidL8_3,
    holdout_b = holdout_burnsL8_3$BurnBndAc - min_size,
    
    min_size = min_size,
    
    n_edges = length(B_L8_3@i),
    node1 = B_L8_3@i + 1, # add one to offset zero-based index
    node2 = B_L8_3@j + 1,
    tb_idx = tb_idxL8_3, 
    cutoff_year = cutoff_year
  )

  # assert that there are no missing values in stan_d
  assert_that(!any(lapply(stan_L8_3, function(x) any(is.na(x))) %>% unlist))


  stan_L9_1 <- list(
    N = N_L9_1,
    T = T,
    p = length(colnamesXL9_1),
    
    n_count = nrow(train_countsL9_1),
    counts = train_countsL9_1$n_fire,
    
    log_area = log(area_L9_1$area * 1e-11) / 2,
    er_idx_train = as.numeric(factor(train_countsL9_1$US_L4NAME,
                                    levels = levels(factor(area_L9_1$US_L4NAME)))),
    er_idx_full = as.numeric(factor(L9_1$US_L4NAME)),
    
    n_fire = nrow(train_burnsL9_1),
    sizes = train_burnsL9_1$BurnBndAc - min_size,
    burn_idx = burn_idxL9_1,
    
    n_w = length(sparse_XL9_1$w),
    w = sparse_XL9_1$w,
    v = sparse_XL9_1$v,
    u = sparse_XL9_1$u,
    
    # sparse design matrix for training counts
    n_w_tc = length(sparse_X_tcL9_1$w),
    w_tc = sparse_X_tcL9_1$w,
    v_tc = sparse_X_tcL9_1$v,
    n_u_tc = length(sparse_X_tcL9_1$u),
    u_tc = sparse_X_tcL9_1$u,
    
    # sparse design matrix for training burns
    n_w_tb = length(sparse_X_tbL9_1$w),
    w_tb = sparse_X_tbL9_1$w,
    v_tb = sparse_X_tbL9_1$v,
    n_u_tb = length(sparse_X_tbL9_1$u),
    u_tb = sparse_X_tbL9_1$u,
    
    burn_eps_idx = burn_eps_idxL9_1,
    
    M = 1,
    slab_df = 5,
    slab_scale = 2,
    
    eps_idx_train = eps_idx_trainL9_1,
    eps_idx_future = eps_idx_futureL9_1,
    size_threshold = 0,
    
    n_holdout_c = length(holdout_cidL9_1),
    holdout_c_idx = holdout_cidL9_1,
    holdout_c = holdout_countsL9_1$n_fire,
    
    n_holdout_b = length(holdout_bidL9_1),
    holdout_b_idx = holdout_bidL9_1,
    holdout_b = holdout_burnsL9_1$BurnBndAc - min_size,
    
    min_size = min_size,
    
    n_edges = length(B_L9_1@i),
    node1 = B_L9_1@i + 1, # add one to offset zero-based index
    node2 = B_L9_1@j + 1,
    tb_idx = tb_idxL9_1, 
    cutoff_year = cutoff_year
  )

  # assert that there are no missing values in stan_d
  assert_that(!any(lapply(stan_L9_1, function(x) any(is.na(x))) %>% unlist))

  stan_L9_2 <- list(
    N = N_L9_2,
    T = T,
    p = length(colnamesXL9_2),
    
    n_count = nrow(train_countsL9_2),
    counts = train_countsL9_2$n_fire,
    
    log_area = log(area_L9_2$area * 1e-11) / 2,
    er_idx_train = as.numeric(factor(train_countsL9_2$US_L4NAME,
                                    levels = levels(factor(area_L9_2$US_L4NAME)))),
    er_idx_full = as.numeric(factor(L9_2$US_L4NAME)),
    
    n_fire = nrow(train_burnsL9_2),
    sizes = train_burnsL9_2$BurnBndAc - min_size,
    burn_idx = burn_idxL9_2,
    
    
    n_w = length(sparse_XL9_2$w),
    w = sparse_XL9_2$w,
    v = sparse_XL9_2$v,
    u = sparse_XL9_2$u,
    
    
    # sparse design matrix for training counts
    n_w_tc = length(sparse_X_tcL9_2$w),
    w_tc = sparse_X_tcL9_2$w,
    v_tc = sparse_X_tcL9_2$v,
    n_u_tc = length(sparse_X_tcL9_2$u),
    u_tc = sparse_X_tcL9_2$u,
    
    # sparse design matrix for training burns
    n_w_tb = length(sparse_X_tbL9_2$w),
    w_tb = sparse_X_tbL9_2$w,
    v_tb = sparse_X_tbL9_2$v,
    n_u_tb = length(sparse_X_tbL9_2$u),
    u_tb = sparse_X_tbL9_2$u,
    
    burn_eps_idx = burn_eps_idxL9_2,
    
    M = 1,
    slab_df = 5,
    slab_scale = 2,
    
    eps_idx_train = eps_idx_trainL9_2,
    eps_idx_future = eps_idx_futureL9_2,
    size_threshold = 0,
    
    n_holdout_c = length(holdout_cidL9_2),
    holdout_c_idx = holdout_cidL9_2,
    holdout_c = holdout_countsL9_2$n_fire,
    
    n_holdout_b = length(holdout_bidL9_2),
    holdout_b_idx = holdout_bidL9_2,
    holdout_b = holdout_burnsL9_2$BurnBndAc - min_size,
    
    min_size = min_size,
    
    n_edges = length(B_L9_2@i),
    node1 = B_L9_2@i + 1, # add one to offset zero-based index
    node2 = B_L9_2@j + 1,
    tb_idx = tb_idxL9_2, 
    cutoff_year = cutoff_year
  )

  # assert that there are no missing values in stan_d
  assert_that(!any(lapply(stan_L9_2, function(x) any(is.na(x))) %>% unlist))


  zi_dL10_1 <- stan_L10_1
  zi_dL10_2 <- stan_L10_2
  zi_dL11 <- stan_L11
  zi_dL5 <- stan_L5
  zi_dL6_1 <- stan_L6_1
  zi_dL6_2 <- stan_L6_2
  zi_dL7 <- stan_L7
  zi_dL8_1 <- stan_L8_1
  zi_dL8_2 <- stan_L8_2
  zi_dL8_3 <- stan_L8_3
  zi_dL9_1 <- stan_L9_1
  zi_dL9_2 <- stan_L9_2

  zi_dL10_1$M <- 2
  zi_dL10_2$M <- 2
  zi_dL11$M <- 2
  zi_dL5$M <- 2
  zi_dL6_1$M <- 2
  zi_dL6_2$M <- 2
  zi_dL7$M <- 2
  zi_dL8_1$M <- 2
  zi_dL8_2$M <- 2
  zi_dL8_3$M <- 2
  zi_dL9_1$M <- 2
  zi_dL9_2$M <- 2


  ## constant across subsets
  write_rds(cutoff_year, file.path(stan_dir, 'cutoff_year.rds'))
  write_rds(ecoregions, file.path(stan_dir, 'ecoregions.rds'))
  write_rds(vars, file.path(stan_dir, 'vars.rds'))
  write_rds(mtbs, file.path(stan_dir, 'mtbs.rds'))

  ### each subset has its own version
  write_rds(zi_dL10_1, file.path(stan_dir, 'zi_dL10_1.rds'))
  write_rds(stan_L10_1, file.path(stan_dir, 'stan_dL10_1.rds'))
  write_rds(L10_1, file.path(stan_dir, 'st_covs_L10_1.rds'))
  write_rds(train_countsL10_1, file.path(stan_dir, 'train_countsL10_1.rds'))
  write_rds(holdout_countsL10_1, file.path(stan_dir, 'holdout_countsL10_1.rds'))
  write_rds(train_burnsL10_1, file.path(stan_dir, 'train_burnsL10_1.rds'))
  write_rds(holdout_burnsL10_1, file.path(stan_dir, 'holdout_burnsL10_1.rds'))
  write_rds(colnamesXL10_1, file.path(stan_dir, 'colnamesXL10_1.rds'))

  ### each subset has its own version
  write_rds(zi_dL10_2, file.path(stan_dir, 'zi_dL10_2.rds'))
  write_rds(stan_L10_2, path = file.path(stan_dir, 'stan_dL10_2.rds'))
  write_rds(L10_2, file.path(stan_dir, 'st_covs_L10_2.rds'))
  write_rds(train_countsL10_2, file.path(stan_dir, 'train_countsL10_2.rds'))
  write_rds(holdout_countsL10_2, file.path(stan_dir, 'holdout_countsL10_2.rds'))
  write_rds(train_burnsL10_2, file.path(stan_dir, 'train_burnsL10_2.rds'))
  write_rds(holdout_burnsL10_2, file.path(stan_dir, 'holdout_burnsL10_2.rds'))
  write_rds(colnamesXL10_2, file.path(stan_dir, 'colnamesXL10_2.rds'))

  ### each subset has its own version
  write_rds(zi_dL11, file.path(stan_dir, 'zi_dL11.rds'))
  write_rds(stan_L11, path = file.path(stan_dir, 'stan_dL11.rds'))
  write_rds(L11, file.path(stan_dir, 'st_covs_L11.rds'))
  write_rds(train_countsL11, file.path(stan_dir, 'train_countsL11.rds'))
  write_rds(holdout_countsL11, file.path(stan_dir, 'holdout_countsL11.rds'))
  write_rds(train_burnsL11, file.path(stan_dir, 'train_burnsL11.rds'))
  write_rds(holdout_burnsL11, file.path(stan_dir, 'holdout_burnsL11.rds'))
  write_rds(colnamesXL11, file.path(stan_dir, 'colnamesXL11.rds'))

  ### each subset has its own version
  write_rds(zi_dL5, file.path(stan_dir, 'zi_dL5.rds'))
  write_rds(stan_L5, file.path(stan_dir, 'stan_dL5.rds'))
  write_rds(L5, file.path(stan_dir, 'st_covs_L5.rds'))
  write_rds(train_countsL5, file.path(stan_dir, 'train_countsL5.rds'))
  write_rds(holdout_countsL5, file.path(stan_dir, 'holdout_countsL5.rds'))
  write_rds(train_burnsL5, file.path(stan_dir, 'train_burnsL5.rds'))
  write_rds(holdout_burnsL5, file.path(stan_dir, 'holdout_burnsL5.rds'))
  write_rds(colnamesXL5, file.path(stan_dir, 'colnamesXL5.rds'))

  ### each subset has its own version
  write_rds(zi_dL6_1, file.path(stan_dir, 'zi_dL6_1.rds'))
  write_rds(stan_L6_1, file.path(stan_dir, 'stan_dL6_1.rds'))
  write_rds(L6_1, file.path(stan_dir, 'st_covs_L6_1.rds'))
  write_rds(train_countsL6_1, file.path(stan_dir, 'train_countsL6_1.rds'))
  write_rds(holdout_countsL6_1, file.path(stan_dir, 'holdout_countsL6_1.rds'))
  write_rds(train_burnsL6_1, file.path(stan_dir, 'train_burnsL6_1.rds'))
  write_rds(holdout_burnsL6_1, file.path(stan_dir, 'holdout_burnsL6_1.rds'))
  write_rds(colnamesXL6_1, file.path(stan_dir, 'colnamesXL6_1.rds'))

  ### each subset has its own version
  write_rds(zi_dL6_2, file.path(stan_dir, 'zi_dL6_2.rds'))
  write_rds(stan_L6_2, file.path(stan_dir, 'stan_dL6_2.rds'))
  write_rds(L6_2, file.path(stan_dir, 'st_covs_L6_2.rds'))
  write_rds(train_countsL6_2, file.path(stan_dir, 'train_countsL6_2.rds'))
  write_rds(holdout_countsL6_2, file.path(stan_dir, 'holdout_countsL6_2.rds'))
  write_rds(train_burnsL6_2, file.path(stan_dir, 'train_burnsL6_2.rds'))
  write_rds(holdout_burnsL6_2, file.path(stan_dir, 'holdout_burnsL6_2.rds'))
  write_rds(colnamesXL6_2, file.path(stan_dir, 'colnamesXL6_2.rds'))

  ### each subset has its own version
  write_rds(zi_dL7, file.path(stan_dir, 'zi_dL7.rds'))
  write_rds(stan_L7, file.path(stan_dir, 'stan_dL7.rds'))
  write_rds(L7, file.path(stan_dir, 'st_covs_L7.rds'))
  write_rds(train_countsL7, file.path(stan_dir, 'train_countsL7.rds'))
  write_rds(holdout_countsL7, file.path(stan_dir, 'holdout_countsL7.rds'))
  write_rds(train_burnsL7, file.path(stan_dir, 'train_burnsL7.rds'))
  write_rds(holdout_burnsL7, file.path(stan_dir, 'holdout_burnsL7.rds'))
  write_rds(colnamesXL7, file.path(stan_dir, 'colnamesXL7.rds'))


  ### each subset has its own version
  write_rds(zi_dL8_1, file.path(stan_dir, 'zi_dL8_1.rds'))
  write_rds(stan_L8_1, file.path(stan_dir, 'stan_dL8_1.rds'))
  write_rds(L8_1, file.path(stan_dir, 'st_covs_L8_1.rds'))
  write_rds(train_countsL8_1, file.path(stan_dir, 'train_countsL8_1.rds'))
  write_rds(holdout_countsL8_1, file.path(stan_dir, 'holdout_countsL8_1.rds'))
  write_rds(train_burnsL8_1, file.path(stan_dir, 'train_burnsL8_1.rds'))
  write_rds(holdout_burnsL8_1, file.path(stan_dir, 'holdout_burnsL8_1.rds'))
  write_rds(colnamesXL8_1, file.path(stan_dir, 'colnamesXL8_1.rds'))

  ### each subset has its own version
  write_rds(zi_dL8_2, file.path(stan_dir, 'zi_dL8_2.rds'))
  write_rds(stan_L8_2, file.path(stan_dir, 'stan_dL8_2.rds'))
  write_rds(L8_2, file.path(stan_dir, 'st_covs_L8_2.rds'))
  write_rds(train_countsL8_2, file.path(stan_dir, 'train_countsL8_2.rds'))
  write_rds(holdout_countsL8_2, file.path(stan_dir, 'holdout_countsL8_2.rds'))
  write_rds(train_burnsL8_2, file.path(stan_dir, 'train_burnsL8_2.rds'))
  write_rds(holdout_burnsL8_2, file.path(stan_dir, 'holdout_burnsL8_2.rds'))
  write_rds(colnamesXL8_2, file.path(stan_dir, 'colnamesXL8_2.rds'))

  ### each subset has its own version
  write_rds(zi_dL8_3, file.path(stan_dir, 'zi_dL8_3.rds'))
  write_rds(stan_L8_3, file.path(stan_dir, 'stan_dL8_3.rds'))
  write_rds(L8_3, file.path(stan_dir, 'st_covs_L8_3.rds'))
  write_rds(train_countsL8_3, file.path(stan_dir, 'train_countsL8_3.rds'))
  write_rds(holdout_countsL8_3, file.path(stan_dir, 'holdout_countsL8_3.rds'))
  write_rds(train_burnsL8_3, file.path(stan_dir, 'train_burnsL8_3.rds'))
  write_rds(holdout_burnsL8_3, file.path(stan_dir, 'holdout_burnsL8_3.rds'))
  write_rds(colnamesXL8_3, file.path(stan_dir, 'colnamesXL8_3.rds'))

  ### each subset has its own version
  write_rds(zi_dL9_1, file.path(stan_dir, 'zi_dL9_1.rds'))
  write_rds(stan_L9_1, file.path(stan_dir, 'stan_dL9_1.rds'))
  write_rds(L9_1, file.path(stan_dir, 'st_covs_L9_1.rds'))
  write_rds(train_countsL9_1, file.path(stan_dir, 'train_countsL9_1.rds'))
  write_rds(holdout_countsL9_1, file.path(stan_dir, 'holdout_countsL9_1.rds'))
  write_rds(train_burnsL9_1, file.path(stan_dir, 'train_burnsL9_1.rds'))
  write_rds(holdout_burnsL9_1, file.path(stan_dir, 'holdout_burnsL9_1.rds'))
  write_rds(colnamesXL9_1, file.path(stan_dir, 'colnamesXL9_1.rds'))

  ### each subset has its own version
  write_rds(zi_dL9_2, file.path(stan_dir, 'zi_dL9_2.rds'))
  write_rds(stan_L9_2, file.path(stan_dir, 'stan_dL9_2.rds'))
  write_rds(L9_2, file.path(stan_dir, 'st_covs_L9_2.rds'))
  write_rds(train_countsL9_2, file.path(stan_dir, 'train_countsL9_2.rds'))
  write_rds(holdout_countsL9_2, file.path(stan_dir, 'holdout_countsL9_2.rds'))
  write_rds(train_burnsL9_2, file.path(stan_dir, 'train_burnsL9_2.rds'))
  write_rds(holdout_burnsL9_2, file.path(stan_dir, 'holdout_burnsL9_2.rds'))
  write_rds(colnamesXL9_2, file.path(stan_dir, 'colnamesXL9_2.rds'))
}

models <- c("CanESM2", "CNRM", "CSIRO", "HadGEM2-CC", "HadGEM2-ES", "IPSL", "MIROC5", "MRI")

for (model in models) {
  make_stan(model)
}
