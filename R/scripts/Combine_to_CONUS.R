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


#### CanESM2
L10_1 <- read_rds('data/processed/CanESM2/L10_1/AnnualL10_1.rds')
L10_2 <- read_rds('data/processed/CanESM2/L10_2/AnnualL10_2.rds')
L11 <- read_rds('data/processed/CanESM2/L11/AnnualL11.rds')
L5 <- read_rds('data/processed/CanESM2/L5/AnnualL5.rds')
L6_1 <- read_rds('data/processed/CanESM2/L6_1/AnnualL6_1.rds')
L6_2 <- read_rds('data/processed/CanESM2/L6_2/AnnualL6_2.rds')
L7 <- read_rds('data/processed/CanESM2/L7/AnnualL7.rds')
L8_1 <- read_rds('data/processed/CanESM2/L8_1/AnnualL8_1.rds')
L8_2 <- read_rds('data/processed/CanESM2/L8_2/AnnualL8_2.rds')
L8_3 <- read_rds('data/processed/CanESM2/L8_3/AnnualL8_3.rds')
L9_1 <- read_rds('data/processed/CanESM2/L9_1/AnnualL9_1.rds')
L9_2 <- read_rds('data/processed/CanESM2/L9_2/AnnualL9_2.rds')

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

#### CNRM
L10_1 <- read_rds('data/processed/CNRM/L10_1/AnnualL10_1.rds')
L10_2 <- read_rds('data/processed/CNRM/L10_2/AnnualL10_2.rds')
L11 <- read_rds('data/processed/CNRM/L11/AnnualL11.rds')
L5 <- read_rds('data/processed/CNRM/L5/AnnualL5.rds')
L6_1 <- read_rds('data/processed/CNRM/L6_1/AnnualL6_1.rds')
L6_2 <- read_rds('data/processed/CNRM/L6_2/AnnualL6_2.rds')
L7 <- read_rds('data/processed/CNRM/L7/AnnualL7.rds')
L8_1 <- read_rds('data/processed/CNRM/L8_1/AnnualL8_1.rds')
L8_2 <- read_rds('data/processed/CNRM/L8_2/AnnualL8_2.rds')
L8_3 <- read_rds('data/processed/CNRM/L8_3/AnnualL8_3.rds')
L9_1 <- read_rds('data/processed/CNRM/L9_1/AnnualL9_1.rds')
L9_2 <- read_rds('data/processed/CNRM/L9_2/AnnualL9_2.rds')

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
CONUS_CNRM<- full_join(Comb10,L9_2)

#### CSIRO
L10_1 <- read_rds('data/processed/CSIRO/L10_1/AnnualL10_1.rds')
L10_2 <- read_rds('data/processed/CSIRO/L10_2/AnnualL10_2.rds')
L11 <- read_rds('data/processed/CSIRO/L11/AnnualL11.rds')
L5 <- read_rds('data/processed/CSIRO/L5/AnnualL5.rds')
L6_1 <- read_rds('data/processed/CSIRO/L6_1/AnnualL6_1.rds')
L6_2 <- read_rds('data/processed/CSIRO/L6_2/AnnualL6_2.rds')
L7 <- read_rds('data/processed/CSIRO/L7/AnnualL7.rds')
L8_1 <- read_rds('data/processed/CSIRO/L8_1/AnnualL8_1.rds')
L8_2 <- read_rds('data/processed/CSIRO/L8_2/AnnualL8_2.rds')
L8_3 <- read_rds('data/processed/CSIRO/L8_3/AnnualL8_3.rds')
L9_1 <- read_rds('data/processed/CSIRO/L9_1/AnnualL9_1.rds')
L9_2 <- read_rds('data/processed/CSIRO/L9_2/AnnualL9_2.rds')

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
CONUS_CSIRO<- full_join(Comb10,L9_2)

#### HadGEM2_CC
L10_1 <- read_rds('data/processed/HadGEM2_CC/L10_1/AnnualL10_1.rds')
L10_2 <- read_rds('data/processed/HadGEM2_CC/L10_2/AnnualL10_2.rds')
L11 <- read_rds('data/processed/HadGEM2_CC/L11/AnnualL11.rds')
L12 <- read_rds('data/processed/HadGEM2_CC/L12/AnnualL12.rds')
L13 <- read_rds('data/processed/HadGEM2_CC/L13/AnnualL13.rds')
L15 <- read_rds('data/processed/HadGEM2_CC/L15/AnnualL15.rds')
L5 <- read_rds('data/processed/HadGEM2_CC/L5/AnnualL5.rds')
L6_1 <- read_rds('data/processed/HadGEM2_CC/L6_1/AnnualL6_1.rds')
L6_2 <- read_rds('data/processed/HadGEM2_CC/L6_2/AnnualL6_2.rds')
L7 <- read_rds('data/processed/HadGEM2_CC/L7/AnnualL7.rds')
L8_1 <- read_rds('data/processed/HadGEM2_CC/L8_1/AnnualL8_1.rds')
L8_2 <- read_rds('data/processed/HadGEM2_CC/L8_2/AnnualL8_2.rds')
L8_3 <- read_rds('data/processed/HadGEM2_CC/L8_3/AnnualL8_3.rds')
L9_1 <- read_rds('data/processed/HadGEM2_CC/L9_1/AnnualL9_1.rds')
L9_2 <- read_rds('data/processed/HadGEM2_CC/L9_2/AnnualL9_2.rds')

Comb1<- full_join(L10_1,L10_2)
Comb2<- full_join(Comb1,L11)
Comb3<- full_join(Comb2,L12)
Comb4<- full_join(Comb3,L13)
Comb5<- full_join(Comb4,L15)
Comb6<- full_join(Comb5,L5)
Comb7<- full_join(Comb6,L6_1)
Comb8<- full_join(Comb7,L6_2)
Comb9<- full_join(Comb8,L7)
Comb10<- full_join(Comb9,L8_1)
Comb11<- full_join(Comb10,L8_2)
Comb12<- full_join(Comb11,L8_3)
Comb13<- full_join(Comb12,L9_1)
CONUS_HadCC<- full_join(Comb13,L9_2)

#### HadGEM2_ES
L10_1 <- read_rds('data/processed/HadGEM2_ES/L10_1/AnnualL10_1.rds')
L10_2 <- read_rds('data/processed/HadGEM2_ES/L10_2/AnnualL10_2.rds')
L11 <- read_rds('data/processed/HadGEM2_ES/L11/AnnualL11.rds')
L12 <- read_rds('data/processed/HadGEM2_ES/L12/AnnualL12.rds')
L13 <- read_rds('data/processed/HadGEM2_ES/L13/AnnualL13.rds')
L15 <- read_rds('data/processed/HadGEM2_ES/L15/AnnualL15.rds')
L5 <- read_rds('data/processed/HadGEM2_ES/L5/AnnualL5.rds')
L6_1 <- read_rds('data/processed/HadGEM2_ES/L6_1/AnnualL6_1.rds')
L6_2 <- read_rds('data/processed/HadGEM2_ES/L6_2/AnnualL6_2.rds')
L7 <- read_rds('data/processed/HadGEM2_ES/L7/AnnualL7.rds')
L8_1 <- read_rds('data/processed/HadGEM2_ES/L8_1/AnnualL8_1.rds')
L8_2 <- read_rds('data/processed/HadGEM2_ES/L8_2/AnnualL8_2.rds')
L8_3 <- read_rds('data/processed/HadGEM2_ES/L8_3/AnnualL8_3.rds')
L9_1 <- read_rds('data/processed/HadGEM2_ES/L9_1/AnnualL9_1.rds')
L9_2 <- read_rds('data/processed/HadGEM2_ES/L9_2/AnnualL9_2.rds')

Comb1<- full_join(L10_1,L10_2)
Comb2<- full_join(Comb1,L11)
Comb3<- full_join(Comb2,L12)
Comb4<- full_join(Comb3,L13)
Comb5<- full_join(Comb4,L15)
Comb6<- full_join(Comb5,L5)
Comb7<- full_join(Comb6,L6_1)
Comb8<- full_join(Comb7,L6_2)
Comb9<- full_join(Comb8,L7)
Comb10<- full_join(Comb9,L8_1)
Comb11<- full_join(Comb10,L8_2)
Comb12<- full_join(Comb11,L8_3)
Comb13<- full_join(Comb12,L9_1)
CONUS_HadES<- full_join(Comb13,L9_2)

#### IPSL
L10_1 <- read_rds('data/processed/IPSL/L10_1/AnnualL10_1.rds')
L10_2 <- read_rds('data/processed/IPSL/L10_2/AnnualL10_2.rds')
L11 <- read_rds('data/processed/IPSL/L11/AnnualL11.rds')
L12 <- read_rds('data/processed/IPSL/L12/AnnualL12.rds')
L13 <- read_rds('data/processed/IPSL/L13/AnnualL13.rds')
L15 <- read_rds('data/processed/IPSL/L15/AnnualL15.rds')
L5 <- read_rds('data/processed/IPSL/L5/AnnualL5.rds')
L6_1 <- read_rds('data/processed/IPSL/L6_1/AnnualL6_1.rds')
L6_2 <- read_rds('data/processed/IPSL/L6_2/AnnualL6_2.rds')
L7 <- read_rds('data/processed/IPSL/L7/AnnualL7.rds')
L8_1 <- read_rds('data/processed/IPSL/L8_1/AnnualL8_1.rds')
L8_2 <- read_rds('data/processed/IPSL/L8_2/AnnualL8_2.rds')
L8_3 <- read_rds('data/processed/IPSL/L8_3/AnnualL8_3.rds')
L9_1 <- read_rds('data/processed/IPSL/L9_1/AnnualL9_1.rds')
L9_2 <- read_rds('data/processed/IPSL/L9_2/AnnualL9_2.rds')

Comb1<- full_join(L10_1,L10_2)
Comb2<- full_join(Comb1,L11)
Comb3<- full_join(Comb2,L12)
Comb4<- full_join(Comb3,L13)
Comb5<- full_join(Comb4,L15)
Comb6<- full_join(Comb5,L5)
Comb7<- full_join(Comb6,L6_1)
Comb8<- full_join(Comb7,L6_2)
Comb9<- full_join(Comb8,L7)
Comb10<- full_join(Comb9,L8_1)
Comb11<- full_join(Comb10,L8_2)
Comb12<- full_join(Comb11,L8_3)
Comb13<- full_join(Comb12,L9_1)
CONUS_IPSL<- full_join(Comb13,L9_2)

#### MIROC
L10_1 <- read_rds('data/processed/MIROC/L10_1/AnnualL10_1.rds')
L10_2 <- read_rds('data/processed/MIROC/L10_2/AnnualL10_2.rds')
L11 <- read_rds('data/processed/MIROC/L11/AnnualL11.rds')
L12 <- read_rds('data/processed/MIROC/L12/AnnualL12.rds')
L13 <- read_rds('data/processed/MIROC/L13/AnnualL13.rds')
L15 <- read_rds('data/processed/MIROC/L15/AnnualL15.rds')
L5 <- read_rds('data/processed/MIROC/L5/AnnualL5.rds')
L6_1 <- read_rds('data/processed/MIROC/L6_1/AnnualL6_1.rds')
L6_2 <- read_rds('data/processed/MIROC/L6_2/AnnualL6_2.rds')
L7 <- read_rds('data/processed/MIROC/L7/AnnualL7.rds')
L8_1 <- read_rds('data/processed/MIROC/L8_1/AnnualL8_1.rds')
L8_2 <- read_rds('data/processed/MIROC/L8_2/AnnualL8_2.rds')
L8_3 <- read_rds('data/processed/MIROC/L8_3/AnnualL8_3.rds')
L9_1 <- read_rds('data/processed/MIROC/L9_1/AnnualL9_1.rds')
L9_2 <- read_rds('data/processed/MIROC/L9_2/AnnualL9_2.rds')

Comb1<- full_join(L10_1,L10_2)
Comb2<- full_join(Comb1,L11)
Comb3<- full_join(Comb2,L12)
Comb4<- full_join(Comb3,L13)
Comb5<- full_join(Comb4,L15)
Comb6<- full_join(Comb5,L5)
Comb7<- full_join(Comb6,L6_1)
Comb8<- full_join(Comb7,L6_2)
Comb9<- full_join(Comb8,L7)
Comb10<- full_join(Comb9,L8_1)
Comb11<- full_join(Comb10,L8_2)
Comb12<- full_join(Comb11,L8_3)
Comb13<- full_join(Comb12,L9_1)
CONUS_MIROC<- full_join(Comb13,L9_2)

#### MRI
L10_1 <- read_rds('data/processed/MRI/L10_1/AnnualL10_1.rds')
L10_2 <- read_rds('data/processed/MRI/L10_2/AnnualL10_2.rds')
L11 <- read_rds('data/processed/MRI/L11/AnnualL11.rds')
L12 <- read_rds('data/processed/MRI/L12/AnnualL12.rds')
L13 <- read_rds('data/processed/MRI/L13/AnnualL13.rds')
L15 <- read_rds('data/processed/MRI/L15/AnnualL15.rds')
L5 <- read_rds('data/processed/MRI/L5/AnnualL5.rds')
L6_1 <- read_rds('data/processed/MRI/L6_1/AnnualL6_1.rds')
L6_2 <- read_rds('data/processed/MRI/L6_2/AnnualL6_2.rds')
L7 <- read_rds('data/processed/MRI/L7/AnnualL7.rds')
L8_1 <- read_rds('data/processed/MRI/L8_1/AnnualL8_1.rds')
L8_2 <- read_rds('data/processed/MRI/L8_2/AnnualL8_2.rds')
L8_3 <- read_rds('data/processed/MRI/L8_3/AnnualL8_3.rds')
L9_1 <- read_rds('data/processed/MRI/L9_1/AnnualL9_1.rds')
L9_2 <- read_rds('data/processed/MRI/L9_2/AnnualL9_2.rds')

Comb1<- full_join(L10_1,L10_2)
Comb2<- full_join(Comb1,L11)
Comb3<- full_join(Comb2,L12)
Comb4<- full_join(Comb3,L13)
Comb5<- full_join(Comb4,L15)
Comb6<- full_join(Comb5,L5)
Comb7<- full_join(Comb6,L6_1)
Comb8<- full_join(Comb7,L6_2)
Comb9<- full_join(Comb8,L7)
Comb10<- full_join(Comb9,L8_1)
Comb11<- full_join(Comb10,L8_2)
Comb12<- full_join(Comb11,L8_3)
Comb13<- full_join(Comb12,L9_1)
CONUS_MRI<- full_join(Comb13,L9_2)

### Ensemble
Ensb_Area <-data.frame(CONUS_Can$mean_total_BA,CONUS_CNRM$mean_total_BA,
                       CONUS_CSIRO$mean_total_BA,CONUS_HadCC$mean_total_BA,
                       CONUS_HadES$mean_total_BA, CONUS_IPSL$mean_total_BA,
                       CONUS_MIROC$mean_total_BA, CONUS_MRI$mean_total_BA)
Ensb_NFire <-data.frame(CONUS_Can$mean_total_n_event,CONUS_CNRM$mean_total_n_event,
                        CONUS_CSIRO$mean_total_n_event, CONUS_HadCC$mean_total_n_event,
                        CONUS_HadES$mean_total_n_event, CONUS_IPSL$mean_total_n_event,
                        CONUS_MIROC$mean_total_n_event, CONUS_MRI$mean_total_n_event)

En_Area<-rowMeans(Ensb_Area)
En_NFire<-rowMeans(Ensb_NFire)


US_L4NAME<-CONUS_Can$US_L4NAME
Year<- CONUS_Can$year
Ensemble<-data.frame(US_L4NAME,Year, En_Area, En_NFire)

write_rds(Ensemble, "Ensemble.rds")

## Make Yearly files
Ens_2023<- Ensemble%>%
  filter(Year == 2023)
Ens_2024<- Ensemble%>%
  filter(Year == 2024)
Ens_2025<- Ensemble%>%
  filter(Year == 2025)
Ens_2026<- Ensemble%>%
  filter(Year == 2026)
Ens_2027<- Ensemble%>%
  filter(Year == 2027)
Ens_2028<- Ensemble%>%
  filter(Year == 2028)
Ens_2029<- Ensemble%>%
  filter(Year == 2029)
Ens_2030<- Ensemble%>%
  filter(Year == 2030)
Ens_2031<- Ensemble%>%
  filter(Year == 2031)

ecoregions <- read_rds('data/processed/HadGEM2_CC/ecoregions.rds')
Fires_23<- sp::merge(ecoregions, Ens_2023, by ='US_L4NAME',duplicateGeoms = T)

## create 4km grid
grid2 = st_as_stars(st_bbox(Fires_23), dx = 10000, dy = 10000)
grid2 = st_as_sf(grid2)
grid2 = grid2[Fires_23, ]

## 2023
Grid_2023<- grid2 %>%
  mutate(grid_id = row_number()) %>% # create unique ID
  st_join(Fires_23) %>% # join the dataset
  group_by(grid_id) # group by the grid id

write_rds(Grid_2023, "Ens_2023.rds")

## 2024
Fires_24<- sp::merge(ecoregions, Ens_2024, by ='US_L4NAME',duplicateGeoms = T)

Grid_2024<- grid2 %>%
  mutate(grid_id = row_number()) %>% # create unique ID
  st_join(Fires_24) %>% # join the dataset
  group_by(grid_id) # group by the grid id

write_rds(Grid_2024, "Ens_2024.rds")

## 2025
Fires_25<- sp::merge(ecoregions, Ens_2025, by ='US_L4NAME',duplicateGeoms = T)

Grid_2025<- grid2 %>%
  mutate(grid_id = row_number()) %>% # create unique ID
  st_join(Fires_25) %>% # join the dataset
  group_by(grid_id) # group by the grid id

write_rds(Grid_2025, "Ens_2025.rds")

## 2026
Fires_26<- sp::merge(ecoregions, Ens_2026, by ='US_L4NAME',duplicateGeoms = T)

Grid_2026<- grid2 %>%
  mutate(grid_id = row_number()) %>% # create unique ID
  st_join(Fires_26) %>% # join the dataset
  group_by(grid_id) # group by the grid id

write_rds(Grid_2026, "Ens_2026.rds")

## 2027
Fires_27<- sp::merge(ecoregions, Ens_2027, by ='US_L4NAME',duplicateGeoms = T)

Grid_2027<- grid2 %>%
  mutate(grid_id = row_number()) %>% # create unique ID
  st_join(Fires_27) %>% # join the dataset
  group_by(grid_id) # group by the grid id

write_rds(Grid_2027, "Ens_2027.rds")

## 2028
Fires_28<- sp::merge(ecoregions, Ens_2028, by ='US_L4NAME',duplicateGeoms = T)

Grid_2028<- grid2 %>%
  mutate(grid_id = row_number()) %>% # create unique ID
  st_join(Fires_28) %>% # join the dataset
  group_by(grid_id) # group by the grid id

write_rds(Grid_2028, "Ens_2028.rds")

## 2029
Fires_29<- sp::merge(ecoregions, Ens_2029, by ='US_L4NAME',duplicateGeoms = T)

Grid_2029<- grid2 %>%
  mutate(grid_id = row_number()) %>% # create unique ID
  st_join(Fires_29) %>% # join the dataset
  group_by(grid_id) # group by the grid id

write_rds(Grid_2029, "Ens_2029.rds")

## 2030
Fires_30<- sp::merge(ecoregions, Ens_2030, by ='US_L4NAME',duplicateGeoms = T)

Grid_2030<- grid2 %>%
  mutate(grid_id = row_number()) %>% # create unique ID
  st_join(Fires_30) %>% # join the dataset
  group_by(grid_id) # group by the grid id

write_rds(Grid_2030, "Ens_2030.rds")

## 2031
Fires_31<- sp::merge(ecoregions, Ens_2031, by ='US_L4NAME',duplicateGeoms = T)

Grid_2031<- grid2 %>%
  mutate(grid_id = row_number()) %>% # create unique ID
  st_join(Fires_31) %>% # join the dataset
  group_by(grid_id) # group by the grid id

write_rds(Grid_2031, "Ens_2031.rds")

## 2032
Fires_32<- sp::merge(ecoregions, Ens_2032, by ='US_L4NAME',duplicateGeoms = T)

Grid_2032<- grid2 %>%
  mutate(grid_id = row_number()) %>% # create unique ID
  st_join(Fires_32) %>% # join the dataset
  group_by(grid_id) # group by the grid id

write_rds(Grid_2032, "Ens_2032.rds")

## 2033
Fires_33<- sp::merge(ecoregions, Ens_2033, by ='US_L4NAME',duplicateGeoms = T)

Grid_2033<- grid2 %>%
  mutate(grid_id = row_number()) %>% # create unique ID
  st_join(Fires_33) %>% # join the dataset
  group_by(grid_id) # group by the grid id

write_rds(Grid_2033, "Ens_2033.rds")

## 2034
Fires_34<- sp::merge(ecoregions, Ens_2034, by ='US_L4NAME',duplicateGeoms = T)

Grid_2034<- grid2 %>%
  mutate(grid_id = row_number()) %>% # create unique ID
  st_join(Fires_34) %>% # join the dataset
  group_by(grid_id) # group by the grid id

write_rds(Grid_2034, "Ens_2034.rds")

## 2035
Fires_35<- sp::merge(ecoregions, Ens_2035, by ='US_L4NAME',duplicateGeoms = T)

Grid_2035<- grid2 %>%
  mutate(grid_id = row_number()) %>% # create unique ID
  st_join(Fires_35) %>% # join the dataset
  group_by(grid_id) # group by the grid id

write_rds(Grid_2035, "Ens_2035.rds")

## 2036
Fires_36<- sp::merge(ecoregions, Ens_2036, by ='US_L4NAME',duplicateGeoms = T)

Grid_2036<- grid2 %>%
  mutate(grid_id = row_number()) %>% # create unique ID
  st_join(Fires_36) %>% # join the dataset
  group_by(grid_id) # group by the grid id

write_rds(Grid_2036, "Ens_2036.rds")

## 2037
Fires_37<- sp::merge(ecoregions, Ens_2037, by ='US_L4NAME',duplicateGeoms = T)

Grid_2037<- grid2 %>%
  mutate(grid_id = row_number()) %>% # create unique ID
  st_join(Fires_37) %>% # join the dataset
  group_by(grid_id) # group by the grid id

write_rds(Grid_2037, "Ens_2037.rds")

## 2038
Fires_38<- sp::merge(ecoregions, Ens_2038, by ='US_L4NAME',duplicateGeoms = T)

Grid_2038<- grid2 %>%
  mutate(grid_id = row_number()) %>% # create unique ID
  st_join(Fires_38) %>% # join the dataset
  group_by(grid_id) # group by the grid id

write_rds(Grid_2038, "Ens_2038.rds")

## 2039
Fires_39<- sp::merge(ecoregions, Ens_2039, by ='US_L4NAME',duplicateGeoms = T)

Grid_2039<- grid2 %>%
  mutate(grid_id = row_number()) %>% # create unique ID
  st_join(Fires_39) %>% # join the dataset
  group_by(grid_id) # group by the grid id

write_rds(Grid_2039, "Ens_2039.rds")

## 2040
Fires_40<- sp::merge(ecoregions, Ens_2040, by ='US_L4NAME',duplicateGeoms = T)

Grid_2040<- grid2 %>%
  mutate(grid_id = row_number()) %>% # create unique ID
  st_join(Fires_40) %>% # join the dataset
  group_by(grid_id) # group by the grid id

write_rds(Grid_2040, "Ens_2040.rds")

## 2041
Fires_41<- sp::merge(ecoregions, Ens_2041, by ='US_L4NAME',duplicateGeoms = T)

Grid_2041<- grid2 %>%
  mutate(grid_id = row_number()) %>% # create unique ID
  st_join(Fires_41) %>% # join the dataset
  group_by(grid_id) # group by the grid id

write_rds(Grid_2041, "Ens_2041.rds")

## 2042
Fires_42<- sp::merge(ecoregions, Ens_2042, by ='US_L4NAME',duplicateGeoms = T)

Grid_2042<- grid2 %>%
  mutate(grid_id = row_number()) %>% # create unique ID
  st_join(Fires_42) %>% # join the dataset
  group_by(grid_id) # group by the grid id

write_rds(Grid_2042, "Ens_2042.rds")

## 2043
Fires_43<- sp::merge(ecoregions, Ens_2043, by ='US_L4NAME',duplicateGeoms = T)

Grid_2043<- grid2 %>%
  mutate(grid_id = row_number()) %>% # create unique ID
  st_join(Fires_43) %>% # join the dataset
  group_by(grid_id) # group by the grid id

write_rds(Grid_2043, "Ens_2043.rds")

## 2044
Fires_44<- sp::merge(ecoregions, Ens_2044, by ='US_L4NAME',duplicateGeoms = T)

Grid_2044<- grid2 %>%
  mutate(grid_id = row_number()) %>% # create unique ID
  st_join(Fires_44) %>% # join the dataset
  group_by(grid_id) # group by the grid id

write_rds(Grid_2044, "Ens_2044.rds")

## 2045
Fires_45<- sp::merge(ecoregions, Ens_2045, by ='US_L4NAME',duplicateGeoms = T)

Grid_2045<- grid2 %>%
  mutate(grid_id = row_number()) %>% # create unique ID
  st_join(Fires_45) %>% # join the dataset
  group_by(grid_id) # group by the grid id

write_rds(Grid_2045, "Ens_2045.rds")

## 2046
Fires_46<- sp::merge(ecoregions, Ens_2046, by ='US_L4NAME',duplicateGeoms = T)

Grid_2046<- grid2 %>%
  mutate(grid_id = row_number()) %>% # create unique ID
  st_join(Fires_46) %>% # join the dataset
  group_by(grid_id) # group by the grid id

write_rds(Grid_2046, "Ens_2046.rds")

## 2047
Fires_47<- sp::merge(ecoregions, Ens_2047, by ='US_L4NAME',duplicateGeoms = T)

Grid_2047<- grid2 %>%
  mutate(grid_id = row_number()) %>% # create unique ID
  st_join(Fires_47) %>% # join the dataset
  group_by(grid_id) # group by the grid id

write_rds(Grid_2047, "Ens_2047.rds")

## 2048
Fires_48<- sp::merge(ecoregions, Ens_2048, by ='US_L4NAME',duplicateGeoms = T)

Grid_2048<- grid2 %>%
  mutate(grid_id = row_number()) %>% # create unique ID
  st_join(Fires_48) %>% # join the dataset
  group_by(grid_id) # group by the grid id

write_rds(Grid_2048, "Ens_2048.rds")

## 2049
Fires_49<- sp::merge(ecoregions, Ens_2049, by ='US_L4NAME',duplicateGeoms = T)

Grid_2049<- grid2 %>%
  mutate(grid_id = row_number()) %>% # create unique ID
  st_join(Fires_49) %>% # join the dataset
  group_by(grid_id) # group by the grid id

write_rds(Grid_2049, "Ens_2049.rds")

## 2050
Fires_50<- sp::merge(ecoregions, Ens_2050, by ='US_L4NAME',duplicateGeoms = T)

Grid_2050<- grid2 %>%
  mutate(grid_id = row_number()) %>% # create unique ID
  st_join(Fires_50) %>% # join the dataset
  group_by(grid_id) # group by the grid id

write_rds(Grid_2050, "Ens_2050.rds")

## 2051
Fires_51<- sp::merge(ecoregions, Ens_2051, by ='US_L4NAME',duplicateGeoms = T)

Grid_2051<- grid2 %>%
  mutate(grid_id = row_number()) %>% # create unique ID
  st_join(Fires_51) %>% # join the dataset
  group_by(grid_id) # group by the grid id

write_rds(Grid_2051, "Ens_2051.rds")

## 2052
Fires_52<- sp::merge(ecoregions, Ens_2052, by ='US_L4NAME',duplicateGeoms = T)

Grid_2052<- grid2 %>%
  mutate(grid_id = row_number()) %>% # create unique ID
  st_join(Fires_52) %>% # join the dataset
  group_by(grid_id) # group by the grid id

write_rds(Grid_2052, "Ens_2052.rds")

## 2053
Fires_53<- sp::merge(ecoregions, Ens_2053, by ='US_L4NAME',duplicateGeoms = T)

Grid_2053<- grid2 %>%
  mutate(grid_id = row_number()) %>% # create unique ID
  st_join(Fires_53) %>% # join the dataset
  group_by(grid_id) # group by the grid id

write_rds(Grid_2053, "Ens_2053.rds")

## 2054
Fires_54<- sp::merge(ecoregions, Ens_2054, by ='US_L4NAME',duplicateGeoms = T)

Grid_2054<- grid2 %>%
  mutate(grid_id = row_number()) %>% # create unique ID
  st_join(Fires_54) %>% # join the dataset
  group_by(grid_id) # group by the grid id

write_rds(Grid_2054, "Ens_2054.rds")

## 2055
Fires_55<- sp::merge(ecoregions, Ens_2055, by ='US_L4NAME',duplicateGeoms = T)

Grid_2055<- grid2 %>%
  mutate(grid_id = row_number()) %>% # create unique ID
  st_join(Fires_55) %>% # join the dataset
  group_by(grid_id) # group by the grid id

write_rds(Grid_2055, "Ens_2055.rds")

## 2056
Fires_56<- sp::merge(ecoregions, Ens_2056, by ='US_L4NAME',duplicateGeoms = T)

Grid_2056<- grid2 %>%
  mutate(grid_id = row_number()) %>% # create unique ID
  st_join(Fires_56) %>% # join the dataset
  group_by(grid_id) # group by the grid id

write_rds(Grid_2056, "Ens_2056.rds")

## 2057
Fires_57<- sp::merge(ecoregions, Ens_2057, by ='US_L4NAME',duplicateGeoms = T)

Grid_2057<- grid2 %>%
  mutate(grid_id = row_number()) %>% # create unique ID
  st_join(Fires_57) %>% # join the dataset
  group_by(grid_id) # group by the grid id

write_rds(Grid_2057, "Ens_2057.rds")

## 2058
Fires_58<- sp::merge(ecoregions, Ens_2058, by ='US_L4NAME',duplicateGeoms = T)

Grid_2058<- grid2 %>%
  mutate(grid_id = row_number()) %>% # create unique ID
  st_join(Fires_58) %>% # join the dataset
  group_by(grid_id) # group by the grid id

write_rds(Grid_2058, "Ens_2058.rds")

## 2059
Fires_59<- sp::merge(ecoregions, Ens_2059, by ='US_L4NAME',duplicateGeoms = T)

Grid_2059<- grid2 %>%
  mutate(grid_id = row_number()) %>% # create unique ID
  st_join(Fires_59) %>% # join the dataset
  group_by(grid_id) # group by the grid id

write_rds(Grid_2059, "Ens_2059.rds")

## 2060
Fires_60<- sp::merge(ecoregions, Ens_2060, by ='US_L4NAME',duplicateGeoms = T)

Grid_2060<- grid2 %>%
  mutate(grid_id = row_number()) %>% # create unique ID
  st_join(Fires_60) %>% # join the dataset
  group_by(grid_id) # group by the grid id

write_rds(Grid_2060, "Ens_2060.rds")
