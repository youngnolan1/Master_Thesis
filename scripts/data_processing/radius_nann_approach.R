#-------------------------------------------------------------------------------
# Script Name: radius_nann_approach.R
# Author: NYZ
# Description: for each household, identify whether it falls within X km radius
#              of an IDP site using nearest neighbours. Add vars to 
#              "ruralmaster.csv".
#-------------------------------------------------------------------------------

#Libraries
library(tidyverse)
library(readstata13)
library(RANN)
library(sp)
library(rgdal)

#Data
setwd("C:/Users/young/OneDrive/Documents/MASTERPROJECT/data/processed_data")
hh_all <- read.csv("ruralmaster.csv")
unique_sites <- read.csv("unique_sites.csv")


#Clean
hh_all <- hh_all %>% 
  select(case_id, lat, long) %>% 
  rename(long = long) %>% 
  mutate(case_id = as.character(case_id))

unique_sites <- unique_sites[,-1]
unique_sites <- distinct(unique_sites, site_id, .keep_all = TRUE)
unique_sites <- unique_sites %>% rename(long = lon)


#Convert the sites lat/long coordinates to UTM
sites2 <- unique_sites
coordinates(sites2) <- c("long", "lat")
proj4string(sites2) <- CRS("+proj=longlat +datum=WGS84")

sites.xy <- spTransform(sites2, CRS(paste0("+proj=utm +zone=36", "ellps=WGS84")))

#Convert the household lat/long coordinates to UTM
hh2 <- hh_all
coordinates(hh2) <- c("long", "lat")
proj4string(hh2) <- CRS("+proj=longlat +datum=WGS84")

hh.xy <- spTransform(hh2, CRS(paste0("+proj=utm +zone=36", "ellps=WGS84")))

## find the nearest neighbor in sites.xy@coords for each hh.xy@coords
res <- nn2(sites.xy@coords, hh.xy@coords, 1)

## res$nn.dist is a vector of the distance to the nearest sites.xy@coords for each hh.xy@coords
## res$nn.idx is a vector of indices to sites.xy of the nearest sites.xy@coords for each hh.xy@coords
hh_all$binary_treat_nann_5km <- res$nn.dists <= 5000
hh_all$site_id_of_nearest_5km <-  ifelse(res$nn.dists <= 5000, unique_sites[res$nn.idx,"site_id"], NA)

hh_all <- hh_all %>% 
  select(case_id, binary_treat_nann_5km, site_id_of_nearest_5km)

#---------------------------------------------------
#Merge everything into ruralmaster
setwd("C:/Users/young/OneDrive/Documents/MASTERPROJECT/data/processed_data")
ruralmaster <- read.csv("ruralmaster.csv")

ruralmaster <- ruralmaster %>% 
  mutate(case_id = (as.character(case_id))) %>% 
  left_join(hh_all, by = "case_id")



#Save names of households which fall within radius of Round 3 sites
hh_rnd3_exclude <- hh_all %>% 
  filter(binary_treat_nann_5km == TRUE)

hh_rnd3_exclude <- as.list(hh_rnd3_exclude$case_id)
