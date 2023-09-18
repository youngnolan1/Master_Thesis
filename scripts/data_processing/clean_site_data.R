#-------------------------------------------------------------------------------
# Script Name: clean_site_data.R
# Author: NYZ
# Description: first it reads and cleans raw IDP site data, yielding "site_full.csv".
#              then it merges with ruralmaster, yielding updated "ruralmaster.csv".
#-------------------------------------------------------------------------------

#Load libraries
library(readxl)
library(tidyverse)

#Set working directory
setwd("C:/Users/young/OneDrive/Documents/MASTERPROJECT/data/raw_data")

#Load data
site_round1 <- read_xlsx("site_round1.xlsx")
site_round2 <- read_xlsx("site_round2.xlsx")
site_round3 <- read_xlsx("site_round3.xlsx")

#Identify sites falling in each round
sites1 <- as.data.frame(unique(site_round1$`1.1.c.1 Site ID (SSID)`))
sites2 <- as.data.frame(unique(site_round2$`Site ID (SSID)`))
sites3 <- as.data.frame(unique(site_round3$SSID))

sites1 <- sites1 %>% 
  rename(site_id = "unique(site_round1$`1.1.c.1 Site ID (SSID)`)")

sites2 <- sites2 %>% 
  rename(site_id = "unique(site_round2$`Site ID (SSID)`)")

sites3 <- sites3 %>% 
  rename(site_id = "unique(site_round3$SSID)")

allsites <- rbind.data.frame(sites1, sites2, sites3)

allsites <- allsites %>% 
  distinct()

#Just keep unique sites for each round
`%notin%` <- Negate("%in%")

sites1 <- as.list(sites1$site_id)

site_round2 <- site_round2 %>% 
  subset(`Site ID (SSID)` %notin% sites1)

sites2 <- as.list(site_round2$`Site ID (SSID)`)

sites12 <- c(sites1, sites2)

site_round3 <- site_round3 %>% 
  subset(SSID %notin% sites12)

#Change column names
site_round1 <- site_round1 %>% 
  rename(site_id = `1.1.c.1 Site ID (SSID)`) %>% 
  rename(lon = `1.1.f.1 Longitude (Manual GPS)`) %>% 
  rename(lat = `1.1.f.2 Latitude (Manual GPS)`) %>% 
  select(site_id, lon, lat)

site_round2 <- site_round2 %>% 
  rename(site_id = `Site ID (SSID)`) %>% 
  rename(lon = `Longitude (Manual GPS)`) %>% 
  rename(lat = `Latitude (Manual GPS)`) %>% 
  select(site_id, lon, lat)

site_round3 <- site_round3 %>% 
  rename(site_id = SSID) %>%
  rename(lon = Longitude) %>% 
  rename(lat = Latitude) %>% 
  select(site_id, lon, lat)

#Merge three rounds
site_full <- rbind.data.frame(site_round1, site_round2, site_round3)

#Remove duplicates
site_full <- site_full %>% 
  distinct() %>% 
  filter(site_id != "CH_ 017")

#Write to csv
setwd("C:/Users/young/OneDrive/Documents/MASTERPROJECT/data/processed_data")
#write.csv(site_full, "site_full.csv")
#-----------------------------------------------------------
#Site-specific variables

#Load data
setwd("C:/Users/young/OneDrive/Documents/MASTERPROJECT/data/raw_data")
r1_site <- read_xlsx("r1_site.xlsx")
r2_site <- read_xlsx("r2_site.xlsx")
r3_site <- read_xlsx("r3_site.xlsx")

#Round 1
r1_site <- r1_site %>% 
  rename(site_id = "1.1.c.1 Site ID (SSID)") %>% 
  rename(site_type = "1.3.b.1 Site Type") %>% 
  rename(site_accessibility = "Is the location physically accessible?") %>% 
  rename(site_number_of_IDPs = "2.1.b.1 Total number of IDP individuals") %>% 
  rename(site_garbage_visible = "How many people live in areas where dumped garbage is frequently visible?") %>% 
  rename(site_defecation_visible = "How many people live in areas where open defecation is frequently visible?") %>% 
  rename(site_enough_soap = "How many IDPs have enough soap?") %>% 
  rename(site_bathing_facilities = "How many IDPs have access to functioning bathing/shower facilities?") %>% 
  rename(site_enough_water_to_clean = "How many IDPs have enough water to cook, bath, do laundry and personal hygiene?") %>% 
  rename(site_sleep_outdoors = "3.1.a.1 How many households in the site sleep outdoors?") %>% 
  rename(site_distance_health_facility = "How long does it take to reach the nearest health facility from the site?") %>% 
  select(site_id, site_type, site_accessibility, site_number_of_IDPs, site_garbage_visible, site_defecation_visible,
         site_enough_soap, site_bathing_facilities, site_enough_water_to_clean, site_sleep_outdoors,
         site_distance_health_facility)

r1_site <- r1_site %>% 
  mutate(site_is_health_center = case_when(site_type == "Health center" ~ 1,
                                      site_type != "Health center" ~ 0)) %>%
  mutate(site_accessibility = case_when(site_accessibility == "Yes" ~ 1,
                                        site_accessibility != "Yes" ~ 0)) %>% 
  mutate(site_garbage_visible = case_when(site_garbage_visible == "Everyone (around 100%)" ~ 5,
                                          site_garbage_visible == "Most (around 75%)" ~ 4,
                                          site_garbage_visible == "About half (around 50%)" ~ 3,
                                          site_garbage_visible == "A few (around 25%)" ~ 2,
                                          site_garbage_visible == "Nobody (around 0%)" ~ 1)) %>% 
  mutate(site_defecation_visible = case_when(site_defecation_visible == "Everyone (around 100%)" ~ 5,
                                             site_defecation_visible == "Most (around 75%)" ~ 4,
                                             site_defecation_visible == "About half (around 50%)" ~ 3,
                                             site_defecation_visible == "A few (around 25%)" ~ 2,
                                             site_defecation_visible == "Nobody (around 0%)" ~ 1)) %>% 
  mutate(site_enough_soap = case_when(site_enough_soap == "Everyone (around 100%)" ~ 1,
                                      site_enough_soap == "Most (around 75%)" ~ 2,
                                      site_enough_soap == "About half (around 50%)" ~ 3,
                                      site_enough_soap == "A few (around 25%)" ~ 4,
                                      site_enough_soap == "Nobody (around 0%)" ~ 5)) %>% 
  mutate(site_bathing_facilities = case_when(site_bathing_facilities == "Everyone (around 100%)" ~ 1,
                                             site_bathing_facilities == "Most (around 50%)" ~ 2,
                                             site_bathing_facilities == "About half (around 50%)" ~ 3,
                                             site_bathing_facilities == "A few (around 25%)" ~ 4,
                                             site_bathing_facilities == "Nobody (around 0%)" ~ 5)) %>% 
  mutate(site_enough_water_to_clean = case_when(site_enough_water_to_clean == "Everyone (around 100%)" ~ 1,
                                                site_enough_water_to_clean == "Most (around 50%)" ~ 2,
                                                site_enough_water_to_clean == "About half (around 50%)" ~ 3,
                                                site_enough_water_to_clean == "A few (around 25%)" ~ 4,
                                                site_enough_water_to_clean == "Nobody (around 0%)" ~ 5)) %>% 
  mutate(site_sleep_outdoors = case_when(site_sleep_outdoors == "Everyone (around 100%)" ~ 5,
                                         site_sleep_outdoors == "Most (around 75%)" ~ 4,
                                         site_sleep_outdoors == "About half (around 50%)" ~ 3,
                                         site_sleep_outdoors == "A few (around 25%)" ~ 2,
                                         site_sleep_outdoors == "Nobody (around 0%)" ~ 1)) %>% 
  mutate(site_distance_health_facility = case_when(site_distance_health_facility == "There is no reachable health facility" ~ 3,
                                         site_distance_health_facility == "More than 30 minutes" ~ 2,
                                         site_distance_health_facility == "Up to 30 minutes" ~ 1))
  
#Round 2
r2_site["Site Type"][r2_site["Site Type"] == "Health centre"] <- "Health center"

r2_site <- r2_site %>% 
  rename(site_id = "Site ID (SSID)") %>% 
  rename(site_type = "Site Type") %>% 
  rename(site_accessibility = "Is the location physically accessible") %>% 
  rename(site_number_of_IDPs = "Total number of IDP individuals") %>% 
  rename(site_garbage_visible = "How many people live in areas where dumped garbage is frequently visible") %>% 
  rename(site_defecation_visible = "How many people live in areas where open defecation is frequently visible") %>% 
  rename(site_enough_soap = "How many IDPs have enough soap") %>% 
  rename(site_bathing_facilities = "How many IDPs have access to functioning bathing shower facilities") %>% 
  rename(site_enough_water_to_clean = "How many IDPs have enough water to cook bath do laundry and personal hygiene") %>% 
  rename(site_sleep_outdoors = "How many households in the site sleep outdoors") %>% 
  rename(site_distance_health_facility = "How long does it take to reach the nearest health facility from the site") %>% 
  select(site_id, site_type, site_accessibility, site_number_of_IDPs, site_garbage_visible, site_defecation_visible,
         site_enough_soap, site_bathing_facilities, site_enough_water_to_clean, site_sleep_outdoors,
         site_distance_health_facility)

r2_site <- r2_site %>% 
  mutate(site_is_health_center = case_when(site_type == "Health center" ~ 1,
                                      site_type != "Health center" ~ 0)) %>% 
  mutate(site_accessibility = 1) %>% 
  mutate(site_garbage_visible = case_when(site_garbage_visible == "Everyone (around 100%)" ~ 5,
                                          site_garbage_visible == "Most (around 75%)" ~ 4,
                                          site_garbage_visible == "About half (around 50%)" ~ 3,
                                          site_garbage_visible == "A few (around 25%)" ~ 2,
                                          site_garbage_visible == "Nobody (around 0%)" ~ 1)) %>% 
  mutate(site_defecation_visible = case_when(site_defecation_visible == "Everyone (around 100%)" ~ 5,
                                             site_defecation_visible == "Most (around 75%)" ~ 4,
                                             site_defecation_visible == "About half (around 50%)" ~ 3,
                                             site_defecation_visible == "A few (around 25%)" ~ 2,
                                             site_defecation_visible == "Nobody (around 0%)" ~ 1)) %>% 
  mutate(site_enough_soap = case_when(site_enough_soap == "Everyone (around 100%)" ~ 1,
                                      site_enough_soap == "Most (around 75%)" ~ 2,
                                      site_enough_soap == "About half (around 50%)" ~ 3,
                                      site_enough_soap == "A few (around 25%)" ~ 4,
                                      site_enough_soap == "Nobody (around 0%)" ~ 5)) %>% 
  mutate(site_bathing_facilities = case_when(site_bathing_facilities == "Everyone (around 100%)" ~ 1,
                                             site_bathing_facilities == "Most (around 75%)" ~ 2,
                                             site_bathing_facilities == "About half (around 50%)" ~ 3,
                                             site_bathing_facilities == "A few (around 25%)" ~ 4,
                                             site_bathing_facilities == "Nobody (around 0%)" ~ 5)) %>% 
  mutate(site_enough_water_to_clean = case_when(site_enough_water_to_clean == "Everyone (around 100%)" ~ 1,
                                                site_enough_water_to_clean == "Most (around 75%)" ~ 2,
                                                site_enough_water_to_clean == "About half (around 50%)" ~ 3,
                                                site_enough_water_to_clean == "A few (around 25%)" ~ 4,
                                                site_enough_water_to_clean == "Nobody (around 0%)" ~ 5)) %>% 
  mutate(site_sleep_outdoors = case_when(site_sleep_outdoors == "Everyone (around 100%)" ~ 5,
                                         site_sleep_outdoors == "Most (around 75%)" ~ 4,
                                         site_sleep_outdoors == "About half (around 50%)" ~ 3,
                                         site_sleep_outdoors == "A few (around 25%)" ~ 2,
                                         site_sleep_outdoors == "Nobody (around 0%)" ~ 1)) %>% 
  mutate(site_distance_health_facility = case_when(site_distance_health_facility == "There is no reachable health facility" ~ 3,
                                                   site_distance_health_facility == "More than 30 minutes" ~ 2,
                                                   site_distance_health_facility == "Up to 30 minutes" ~ 1))
  
r2_site["site_id"][r2_site["site_id"] == "CH_ 017"] <- "CH_017"


#Merge with ruralmaster (excluding round 3 sites and households)
ruralmaster <- read.csv("ruralmaster.csv")

ruralmaster <- ruralmaster[,-1]

ruralmaster_no_rnd3 <- ruralmaster %>% 
  filter(case_id %notin% hh_rnd3_exclude)

r1_site_unique <- as.list(r1_site$site_id)

r2_site <- r2_site %>% 
  subset(site_id %notin% r1_site_unique)

r1_and_r2_site <- rbind(r1_site, r2_site)

r1_and_r2_site <- r1_and_r2_site %>%
  distinct(site_id, .keep_all = TRUE)

ruralmaster_no_rnd3 <- ruralmaster_no_rnd3 %>% 
  rename(site_id = "site_id_of_nearest")

ruralmaster_no_rnd3 <- left_join(ruralmaster_no_rnd3, r1_and_r2_site, by = "site_id")


#Including round 3 (no hygiene questions)
r3_site <- r3_site %>% 
  rename(site_id = "SSID") %>% 
  rename(site_type = "Use of the site before the displacement") %>% 
  rename(site_accessibility = "Accessibility to Site") %>% 
  rename(site_number_of_IDPs = "Total Number of IDP Individuals") %>% 
  select(site_id, site_type, site_accessibility, site_number_of_IDPs)

r3_site <- r3_site %>% 
  mutate(site_is_health_center = case_when(site_type == "Health center" ~ 1,
                                           site_type != "Health center" ~ 0)) %>%
  mutate(site_accessibility = 1)

unique_r1_and_r2 <- as.list(r1_and_r2_site$site_id_of_nearest)

r3_site <- r3_site %>% 
  subset(site_id %notin% unique_r1_and_r2)

r1_and_r2_site_no_hygiene <- r1_and_r2_site %>% 
  select(site_id, site_type, site_is_health_center, site_accessibility, site_number_of_IDPs)

all_rounds_site <- rbind(r1_and_r2_site_no_hygiene, r3_site)

ruralmaster <- ruralmaster %>% 
  rename(site_id = "site_id_of_nearest")

ruralmaster <- left_join(ruralmaster, all_rounds_site, by = "site_id")

ruralmaster <- ruralmaster %>% 
  mutate(perception_healthcare = case_when(perception_healthcare == "It was less than adequate for household needs" ~ 3,
                                           perception_healthcare == "It was just adequate for household needs" ~ 2,
                                           perception_healthcare == "It was more than adequate for household needs" ~ 1,
                                           perception_healthcare == 3 ~ 1,
                                           perception_healthcare == 2 ~ 2,
                                           perception_healthcare == 1 ~ 3))

ruralmaster_no_rnd3 <- ruralmaster_no_rnd3 %>% 
  mutate(perception_healthcare = case_when(perception_healthcare == "It was less than adequate for household needs" ~ 3,
                                           perception_healthcare == "It was just adequate for household needs" ~ 2,
                                           perception_healthcare == "It was more than adequate for household needs" ~ 1,
                                           perception_healthcare == 3 ~ 1,
                                           perception_healthcare == 2 ~ 2,
                                           perception_healthcare == 1 ~ 3))

#Write to csv
setwd("C:/Users/young/OneDrive/Documents/MASTERPROJECT/data/processed_data")
#write.csv(ruralmaster, "ruralmaster.csv")
#write.csv(ruralmaster_no_rnd3, "ruralmaster_no_rnd3.csv")
