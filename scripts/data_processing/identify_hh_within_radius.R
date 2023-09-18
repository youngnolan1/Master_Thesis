#-------------------------------------------------------------------------------
# Script Name: identify_hh_within_radius.R
# Author: NYZ
# Description: for each household, identify whether it falls within X km radius
#              of an IDP site. Yields "site_treatment_df.csv".
#-------------------------------------------------------------------------------

#Libraries
library(sf)
library(tidyverse)
library(readstata13)
library(mapview)

#Load data
setwd("C:/Users/young/OneDrive/Documents/MASTERPROJECT/data/raw_data")
hh_geo3 <- read.dta13("householdgeovariables_ihs3.dta")
hh_geo4 <- read.dta13("householdgeovariables_ihs4.dta")
hh_geo5 <- read.dta13("householdgeovariables_ihs5.dta")
unique_sites <- read.csv("unique_sites.csv")

hh_consumpagg3 <- read.dta13("ihs3_mod_a_filt.dta")
hh_consumpagg4 <- read.dta13("ihs4_consumption_aggregate.dta")
hh_consumpagg5 <- read.dta13("ihs5_consumption_aggregate.dta")

sofar_hh4 <- read.csv("sofar_hh4.csv")
sofar_hh4 <- sofar_hh4[,-c(1:2)]
sofar_hh4$case_id <- as.character(sofar_hh4$case_id)

#Clean
hh_geo3 <- left_join(hh_geo3, hh_consumpagg3, by = "case_id")
ihs3_districts <- c("Chikwawa", "Phalombe", "Nsanje", "Zomba City", "Zomba")

hh_geo4 <- left_join(hh_geo4, hh_consumpagg4, by = "case_id")
ihs4_districts <- c("Chikwawa", "Phalombe", "Nsanje", "Zomba City", "Zomba Non-City")

hh_geo5 <- left_join(hh_geo5, hh_consumpagg5, by = "case_id")
ihs5_districts <- c("Chikwawa", "Phalombe", "Nsanje", "Zomba")

hh_geo3 <- hh_geo3 %>% 
  rename(lon = lon_modified) %>% 
  rename(lat = lat_modified) %>% 
  filter(hh_a01 %in% ihs3_districts) %>% 
  select(case_id, lon, lat) %>% 
  na.omit(hh_geo3) %>% 
  arrange(case_id)

hh_geo4 <- hh_geo4 %>% 
  rename(lon = lon_modified) %>% 
  rename(lat = lat_modified) %>% 
  filter(district %in% ihs4_districts) %>% 
  select(case_id, lon, lat) %>% 
  na.omit(hh_geo4) %>% 
  arrange(case_id)

hh_geo5 <- hh_geo5 %>% 
  rename(lon = ea_lon_mod) %>% 
  rename(lat = ea_lat_mod) %>% 
  filter(district %in% ihs5_districts) %>% 
  select(case_id, lon, lat) %>% 
  na.omit(hh_geo5) %>% 
  arrange(case_id)

hhids_3 <- as.list(hh_geo3$case_id)
hhids_4 <- as.list(hh_geo4$case_id)
hhids_5 <- as.list(hh_geo5$case_id)

unique_sites <- unique_sites[,-1]
unique_sites <- distinct(unique_sites, site_id, .keep_all = TRUE)

#Change data to sf form
stores_ihs3 <- st_as_sf(hh_geo3, coords = c("lon", "lat"), crs = 4326)
stores_ihs4 <- st_as_sf(hh_geo4, coords = c("lon", "lat"), crs = 4326)
stores_ihs5 <- st_as_sf(hh_geo5, coords = c("lon", "lat"), crs = 4326)

stores_sites <- st_as_sf(unique_sites, coords = c("lon", "lat"), crs = 4326)


#UTM zone
stores_ihs3_utm <- st_transform(stores_ihs3, "+proj=utm +zone=36")
stores_ihs4_utm <- st_transform(stores_ihs4, "+proj=utm +zone=36")
stores_ihs5_utm <- st_transform(stores_ihs5, "+proj=utm +zone=36")

stores_sites_utm <- st_transform(stores_sites, "+proj=utm +zone=36")


#Site radius
site_radius <- st_buffer(stores_sites_utm, 5000)

stores_ihs3_utm <- st_cast(stores_ihs3_utm, "POINT")
stores_ihs4_utm <- st_cast(stores_ihs4_utm, "POINT")
stores_ihs5_utm <- st_cast(stores_ihs5_utm, "POINT")

contains3 <- as.data.frame(st_contains(site_radius, stores_ihs3_utm, sparse = FALSE))
contains4 <- as.data.frame(st_contains(site_radius, stores_ihs4_utm, sparse = FALSE))
contains5 <- as.data.frame(st_contains(site_radius, stores_ihs5_utm, sparse = FALSE))

#Find radiuses which contained households in both 2016 and 2020
sites <- as.list(unique_sites$site_id)

contains4$site_id <- sites
contains5$site_id <- sites

id_contains4 <- which(apply(contains4, 1, any))
actually_contains4 <- contains4[id_contains4, ]

id_contains5 <- which(apply(contains5, 1, any))
actually_contains5 <- contains5[id_contains5, ]

sites_with_hh_5 <- as.list(actually_contains5$site_id)

common_radius <- actually_contains4 %>% 
  filter(site_id %in% sites_with_hh_5)


#List of sites which contain households in both 2016 and 2020
treatment_sites <- as.list(common_radius$site_id)


#Identify sites which contain households in 2020 but not in 2016
`%notin%` <- Negate("%in%")

contains_in_5_but_not_4 <- actually_contains5 %>% 
  filter(site_id %notin% treatment_sites)

sites_to_exclude <- as.list(contains_in_5_but_not_4$site_id)


#Change column and row names to household and site IDs respectively
names(contains3)[] <- hhids_3
row.names(contains3)[] <- sites

contains4 <- contains4[,-1917]
names(contains4)[] <- hhids_4
row.names(contains4)[] <- sites

contains5 <- contains5[,-1408]
names(contains5)[] <- hhids_5
row.names(contains5)[] <- sites

#Transpose
contains3 <- as.data.frame(t(contains3))

contains4 <- as.data.frame(t(contains4))

contains5 <- as.data.frame(t(contains5))

#Replace TRUE/FALSE with 1/0
contains3 <- contains3*1
contains4 <- contains4*1
contains5 <- contains5*1

#Create continuous treatment variable (just sum all 1s for each household!)
contains3$continuous_treat <- rowSums(contains3[,])

contains4$continuous_treat <-rowSums(contains4[,])

contains5$continuous_treat <-rowSums(contains5[,])

#Create dummy treatment variable and drop site columns
contains3 <- contains3 %>% 
  mutate(binary_treat = case_when(continuous_treat == 0 ~ 0,
                                  continuous_treat > 0 ~ 1)) %>% 
  select(continuous_treat, binary_treat)


contains4 <- contains4 %>% 
  mutate(binary_treat = case_when(continuous_treat == 0 ~ 0,
                                  continuous_treat > 0 ~ 1)) %>% 
  select(continuous_treat, binary_treat)


contains5 <- contains5 %>% 
  mutate(binary_treat = case_when(continuous_treat == 0 ~ 0,
                                  continuous_treat > 0 ~ 1)) %>% 
  select(continuous_treat, binary_treat)

#Add site_id column for merging
contains3 <- data.frame(case_id = row.names(contains3), contains3)
contains3 <- contains3 %>% 
  as.data.frame(row.names = 1:nrow(.))

contains4 <- data.frame(case_id = row.names(contains4), contains4)

contains5 <- data.frame(case_id = row.names(contains5), contains5)

#Rowbind them to have one big dataframe
site_treatment_df <- rbind(contains4, contains5)
site_treatment_df <- site_treatment_df %>% 
  as.data.frame(row.names = 1:nrow(.))

#Rowbind with contains3
site_treatment_df <- read.csv("site_treatment_df.csv")
site_treatment_df <- site_treatment_df[,-1]
site_treatment_df <- rbind(site_treatment_df, contains3)


#Write to csv
setwd("C:/Users/young/OneDrive/Documents/MASTERPROJECT/data/processed_data")
write.csv(site_treatment_df, "site_treatment_df.csv")
