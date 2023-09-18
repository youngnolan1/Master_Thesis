#-------------------------------------------------------------------------------
# Script Name: radius_distance_approach.R
# Author: NYZ
# Description: for each household, identify distance to nearest site. Add vars
#              to "ruralmaster.csv".
#-------------------------------------------------------------------------------

#Libraries
library(tidyverse)
library(readstata13)
library(RANN)
library(sp)
library(rgdal)

#Data
setwd("C:/Users/young/OneDrive/Documents/MASTERPROJECT/data/raw_data/ihs5_household")
hh_geo5 <- read.dta13("householdgeovariables_ihs5.dta")
hh_consumpagg5 <- read.dta13("ihs5_consumption_aggregate.dta")
setwd("C:/Users/young/OneDrive/Documents/MASTERPROJECT/data/processed_data")
unique_sites <- read.csv("unique_sites.csv")


#Clean
hh_geo5 <- left_join(hh_geo5, hh_consumpagg5, by = "case_id")
ihs5_districts <- c("Chikwawa", "Phalombe", "Nsanje", "Zomba")

hh_geo5 <- hh_geo5 %>% 
  rename(long = ea_lon_mod) %>% 
  rename(lat = ea_lat_mod) %>% 
  filter(district %in% ihs5_districts) %>% 
  select(case_id, long, lat) %>% 
  na.omit(hh_geo5) %>% 
  arrange(case_id)

hhids_5 <- as.list(hh_geo5$case_id)

unique_sites <- unique_sites[,-1]
unique_sites <- distinct(unique_sites, site_id, .keep_all = TRUE)
unique_sites <- unique_sites %>% rename(long = lon)


#Convert the sites lat/long coordinates to UTM
sites2 <- unique_sites
coordinates(sites2) <- c("long", "lat")
proj4string(sites2) <- CRS("+proj=longlat +datum=WGS84")

sites.xy <- spTransform(sites2, CRS(paste0("+proj=utm +zone=36", "ellps=WGS84")))

#Convert the household lat/long coordinates to UTM
hh2 <- hh_geo5
coordinates(hh2) <- c("long", "lat")
proj4string(hh2) <- CRS("+proj=longlat +datum=WGS84")

hh.xy <- spTransform(hh2, CRS(paste0("+proj=utm +zone=36", "ellps=WGS84")))

## find the nearest neighbor in sites.xy@coords for each hh.xy@coords
res <- nn2(sites.xy@coords, hh.xy@coords, 1)

## res$nn.dist is a vector of the distance to the nearest sites.xy@coords for each hh.xy@coords
## res$nn.idx is a vector of indices to sites.xy of the nearest sites.xy@coords for each hh.xy@coords
hh_geo5$distance_to_nearest <- res$nn.dists
hh_geo5$site_id_of_nearest <- unique_sites[res$nn.idx,"site_id"]

#---------------------------------------------------
#Merge everything into IHS5 2020 master frame
setwd("C:/Users/young/OneDrive/Documents/MASTERPROJECT/data/processed_data")
distance_master <- read.csv("ruralmaster.csv")

distance_master <- distance_master %>% 
  select(-c(X,lat, lon)) %>% 
  mutate(case_id = (as.character(case_id))) %>% 
  filter(year2020 == 1) %>% 
  left_join(hh_geo5, by = "case_id")

#Perception healthcare
distance_master <- distance_master %>% 
  mutate(perception_healthcare = case_when(perception_healthcare == "It was less than adequate for household needs" ~ 3,
                                           perception_healthcare == "It was just adequate for household needs" ~ 2,
                                           perception_healthcare == "It was more than adequate for household needs" ~ 1))

#---------------------------------------------------
#Add site vars
all_rounds_site <- rename(all_rounds_site, site_id_of_nearest = site_id)
distance_master <- left_join(distance_master, all_rounds_site, by = "site_id_of_nearest")

r1_and_r2_site <- r1_and_r2_site %>% 
  rename(site_id_of_nearest = site_id) %>% 
  select(-c(site_type, site_accessibility, site_number_of_IDPs, site_is_health_center))

distance_hygiene <- left_join(distance_master, r1_and_r2_site, by = "site_id_of_nearest")
distance_hygiene <- distance_hygiene %>% 
  filter(site_id_of_nearest %notin% exclude)
