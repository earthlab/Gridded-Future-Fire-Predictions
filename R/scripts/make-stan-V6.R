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


subregion_constraints <- list(
    L5 = list(c("5"), c(""), "NA_L1CODE"),
    L6_1 = list(c("6.2.3", "6.2.4", "6.2.4", "6.2.5", "6.2.6", "6.2.7", "6.2.8", "6.2.9"), c(""), "NA_L3CODE"),
    L6_2 = list(c("6.2.10", "6.2.11", "6.2.12", "6.2.13", "6.2.14", "6.2.15"), c("Semiarid Foothills"), "NA_L3CODE"),
    L7 = list(c("7"), c(""), "NA_L1CODE"),
    L8_1 = list(c("8.1", "8.2"), c(""), "NA_L2CODE"),
    L8_2 = list(c("8.3"), c("Floodplains and Low Terraces", "Sand Hills"), "NA_L2CODE"),
    L8_3 = list(c("8.4", "8.5", "15.4"), c("Grand Prairie", "Floodplains and Low Terraces"), "NA_L2CODE"),
    L9_1 = list(c("9.2", "9.3"), c(""), "NA_L2CODE"),
    L9_2 = list(c("9.4", "9.5", "9.6"), c(""), "NA_L2CODE"),
    L10_1 = list(c("10.1"), c("Partly Forested Mountains", "Pluvial Lake Basins"), "NA_L2CODE"),
    L10_2 = list(c("10.2", "12.1", "13.1"), c("Lava Malpais", "Rio Grande Floodplain"), "NA_L2CODE"),
    L11 = list(c("11"), c("Northern Channel Islands", "Southern Channel Islands"), "NA_L1CODE")
)


ecoregions <- st_make_valid(st_read(file.path(raw_data_dir, "ecoregions_L4", 'us_eco_l4_no_st.shp')))
ecoregions <- st_transform(ecoregions, crs = "EPSG:4326")


ecoregion_df <- as(ecoregions, "Spatial") %>%
  data.frame
  
er_df <- dplyr::distinct(data.frame(ecoregions),
                         US_L4NAME,US_L3NAME, NA_L2NAME, NA_L1NAME,
                         NA_L3CODE,NA_L2CODE, NA_L1CODE) %>%
  as_tibble

# get areas for each L3 ecoregion
area_df <- ecoregion_df %>%
  as.data.frame %>%
  as_tibble %>%
  group_by(US_L4NAME) %>%
  summarize(area = sum(Shape_Area))

area_df<- area_df[-c(529,762),]

area_df <- area_df %>%
      left_join(er_df) %>%
      droplevels %>%
      filter(!(US_L4NAME %in% c("Northern Channel Islands", "Southern Channel Islands")))

area_df <- na.omit(area_df)

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

cutoff_year <- 2020

train_burns <- mtbs %>%
  filter(year > 1989 & year < cutoff_year) %>%
  left_join(er_df)
train_burns<- train_burns[-c(5391,5789),]  


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
    
    holdout_burns <-  mtbs %>%
      filter(year >= cutoff_year) %>%
      left_join(st_covs)%>%
      arrange(ym, US_L4NAME)
    holdout_burns<- holdout_burns[-510,]

  for (subregion_name in names(subregion_constraints)) {
    subregion_values <- subregion_constraints[[subregion_name]]

    # Access the three values for the current subregion
    subregion_codes <- subregion_values[[1]]
    subregion_exclusions <- subregion_values[[2]]
    subregion_filter <- subregion_values[[3]]
    
    # Print or process the values as needed
    eco <- ecoregions %>%
      filter(!!sym(subregion_filter) %in% subregion_codes)

    # generate spatial neighbors
    nb <- poly2nb(as(eco, 'Spatial'))
    nb_agg <- aggregate(nb, eco$US_L4NAME)

    # generate neighborhood data for car prior
    listw <- nb2listw(nb_agg, style = 'B', zero.policy = TRUE)

    # B is suitable for building N, N_edges, node1, and node2
    # following http://mc-stan.org/users/documentation/case-studies/icar_stan.html
    B <- as(listw, 'symmetricMatrix')

    print(length(unique(st_covs$US_L4NAME)))

    L <- st_covs %>%
      filter(!!sym(subregion_filter) %in% subregion_codes) %>%
      filter(!(US_L4NAME %in% subregion_exclusions))

      print(length(unique(L$US_L4NAME)))

    L$id <- 1:nrow(L)

    # Create training sets for each subset, including years from 1990 to cutoff_year
    train_counts_L <- L %>%
      filter(year < cutoff_year) 

    train_burns_L <- train_burns %>%
      filter(!!sym(subregion_filter) %in% subregion_codes) %>%
      filter(!(US_L4NAME %in% subregion_exclusions)) %>%
      left_join(train_counts_L) %>%
      filter (!is.na(pr))

    holdout_counts_L <- L %>%
    filter(year >= cutoff_year)

    holdout_cid_L <- match(holdout_counts_L$er_ym, L$er_ym)

    holdout_burns_L <- holdout_burns %>%
      filter(!!sym(subregion_filter) %in% subregion_codes) %>%
      filter(!(US_L4NAME %in% subregion_exclusions))
    
    holdout_bid_L <- match(holdout_burns_L$er_ym, holdout_counts_L$er_ym)
    
    N_L <- length(unique(L$US_L4NAME))
    T <- length(unique(L$ym))
    assert_that(identical(nrow(L), N_L * T))
    vars <- c('log_housing_density', 'was', 'pr', 'prev_12mo_precip', 'tasmax', 'rhsmin')
    df_each <- 5
    X_bs <- list()
    X_bs_df <- list()
    for (i in seq_along(vars)) {
      X_bs[[i]] <- bs(L[[vars[i]]], df = df_each, intercept = TRUE)
      
      X_bs_df[[i]] <- X_bs[[i]] %>%
        as_tibble
      names(X_bs_df[[i]]) <- paste('bs', vars[[i]], 1:df_each, sep = '_')
    }
  
    X_bs_L <- bind_cols(X_bs_df)
    assert_that(!any(is.na(X_bs_L)))

    # Create design matrices --------------------------------------------------
    l4_terms <- paste0('US_L4NAME * ', names(X_bs_L)) %>%
      paste(collapse = ' + ')
    l3_terms <- paste0('US_L3NAME * ', names(X_bs_df)) %>%
      paste(collapse = ' + ')
    l2_terms <- paste0('NA_L2NAME * ', names(X_bs_df)) %>%
      paste(collapse = ' + ')
    l1_terms <- paste0('NA_L1NAME * ', names(X_bs_df)) %>%
      paste(collapse = ' + ')

    L <- L %>%
    bind_cols(lapply(X_bs_L, c)) %>%
    as_tibble

    X_L <- model.matrix(as.formula(paste('~ 0 + ',
                                    l4_terms,
                                    sep = ' + ')),
                    data = L)
      
    sparse_XL <- extract_sparse_parts(X_L)
    colnamesXL <- colnames(X_L)

    # design matrix for training counts
    # is a subset of the rows of X, based on which rows show up in train_counts
    eps_idx_train_L <- match(train_counts_L$er_ym, L$er_ym)
    assert_that(all(diff(eps_idx_train_L) == 1))

    X_tc_L <- X_L[eps_idx_train_L, ]
    assert_that(identical(nrow(X_tc_L), nrow(train_counts_L)))

    sparse_X_tc_L <- extract_sparse_parts(X_tc_L)

    eps_idx_future_L <- setdiff(1:nrow(L), eps_idx_train_L)

    assert_that(all(diff(eps_idx_future_L) == 1))
    assert_that(eps_idx_train_L[length(eps_idx_train_L)] + 1 == eps_idx_future_L[1])

    train_burn_covs_L <- train_burns_L %>%
      distinct(er_ym, .keep_all = TRUE)
    
    assert_that(nrow(train_burn_covs_L) <= nrow(train_burns_L))

    tb_idx_L <- match(train_burn_covs_L$er_ym, L$er_ym)

    X_tb_L <- X_L[tb_idx_L, ]
    assert_that(identical(nrow(X_tb_L), nrow(train_burn_covs_L)))

    sparse_X_tb_L <- extract_sparse_parts(X_tb_L)

    # indices to match epsilon parameters for burn areas to those computed for counts
    burn_eps_idx_L <- match(train_burn_covs_L$er_ym, train_counts_L$er_ym)
    assert_that(train_burn_covs_L$er_ym[1] == train_counts_L$er_ym[burn_eps_idx_L[1]])

    # indices to match each fire event to a row in the design matrix for burns
    burn_idx_L <- match(train_burns_L$er_ym, train_burn_covs_L$er_ym)

    # check to make sure the indices were correct
    assert_that(max(burn_idx_L) <= nrow(L))
    assert_that(all(train_burn_covs_L$US_L4NAME[burn_idx_L] == train_burns_L$US_L4NAME))
    assert_that(all(train_burn_covs_L$ym[burn_idx_L] == train_burns_L$ym))

    rm(X_L)
    rm(X_tc_L)
    rm(X_tb_L)

    area_L <- area_df %>%
      filter(!!sym(subregion_filter) %in% subregion_codes) %>%
      filter((US_L4NAME %in% unique(L$US_L4NAME))) %>% 
      distinct(US_L4NAME, .keep_all = TRUE)
    
    assert_that(identical(unique(area_L$US_L4NAME), unique(L$US_L4NAME)))
    assert_that(identical(levels(factor(area_L$US_L4NAME)), levels(factor(L$US_L4NAME))))

    min_size <- 1e3
    # Bundle up data into a list too pass to Stan -----------------------------

    stan_L <- list(
      N = N_L,
      T = T,
      p = length(colnamesXL),
      
      n_count = nrow(train_counts_L),
      counts = train_counts_L$n_fire,
      
      log_area = log(area_L$area * 1e-11) / 2,
      er_idx_train = as.numeric(factor(train_counts_L$US_L4NAME,
                                      levels = levels(factor(area_L$US_L4NAME)))),
      er_idx_full = as.numeric(factor(L$US_L4NAME)),
      
      n_fire = nrow(train_burns_L),
      sizes = train_burns_L$BurnBndAc - min_size,
      burn_idx = burn_idx_L,
      
      n_w = length(sparse_XL$w),
      w = sparse_XL$w,
      v = sparse_XL$v,
      u = sparse_XL$u,

      # sparse design matrix for training counts
      n_w_tc = length(sparse_X_tc_L$w),
      w_tc = sparse_X_tc_L$w,
      v_tc = sparse_X_tc_L$v,
      n_u_tc = length(sparse_X_tc_L$u),
      u_tc = sparse_X_tc_L$u,

      # sparse design matrix for training burns
      n_w_tb = length(sparse_X_tb_L$w),
      w_tb = sparse_X_tb_L$w,
      v_tb = sparse_X_tb_L$v,
      n_u_tb = length(sparse_X_tb_L$u),
      u_tb = sparse_X_tb_L$u,
      
      burn_eps_idx = burn_eps_idx_L,
      
      M = 1,
      slab_df = 5,
      slab_scale = 2,
      
      eps_idx_train = eps_idx_train_L,
      eps_idx_future = eps_idx_future_L,
      size_threshold = 0,
      
      n_holdout_c = length(holdout_cid_L),
      holdout_c_idx = holdout_cid_L,
      holdout_c = holdout_counts_L$n_fire,
      
      n_holdout_b = length(holdout_bid_L),
      holdout_b_idx = holdout_bid_L,
      holdout_b = holdout_burns_L$BurnBndAc - min_size,
      
      min_size = min_size,
      
      n_edges = length(B@i),
      node1 = B@i + 1, # add one to offset zero-based index
      node2 = B@j + 1,
      tb_idx = tb_idx_L, 
      cutoff_year = cutoff_year
    )

    # assert that there are no missing values in stan_d
    assert_that(!any(lapply(stan_L, function(x) any(is.na(x))) %>% unlist))

    zi_dL <- stan_L
    zi_dL$M <- 2

    write_rds(zi_dL, file.path(stan_dir, paste0('zi_d', subregion_name, '.rds')))
    write_rds(stan_L, file.path(stan_dir, paste0('stan_d', subregion_name, '.rds')))
    write_rds(L, file.path(stan_dir, paste0('st_covs_', subregion_name, '.rds')))
    write_rds(train_counts_L, file.path(stan_dir, paste0('train_counts', subregion_name, '.rds')))
    write_rds(holdout_counts_L, file.path(stan_dir, paste0('holdout_counts', subregion_name, '.rds')))
    write_rds(train_burns_L, file.path(stan_dir, paste0('train_burns', subregion_name, '.rds')))
    write_rds(holdout_burns_L, file.path(stan_dir, paste0('holdout_burns', subregion_name, '.rds')))
    write_rds(colnamesXL, file.path(stan_dir, paste0('colnamesX', subregion_name, '.rds')))

  }

  write_rds(cutoff_year, file.path(stan_dir, 'cutoff_year.rds'))
  write_rds(ecoregions, file.path(stan_dir, 'ecoregions.rds'))
  write_rds(vars, file.path(stan_dir, 'vars.rds'))
  write_rds(mtbs, file.path(stan_dir, 'mtbs.rds'))

}

models <- c("CanESM2", "CNRM", "CSIRO", "HadGEM2-CC", "HadGEM2-ES", "IPSL", "MIROC5", "MRI")

for (model in models) {
  make_stan(model)
}
