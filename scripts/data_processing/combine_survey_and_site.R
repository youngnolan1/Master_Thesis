#-------------------------------------------------------------------------------
# Script Name: combine_survey_and_site.R
# Author: NYZ
# Description: combine processed survey and site data into "ruralmaster.csv"
#-------------------------------------------------------------------------------


#Libraries
library(tidyverse)

#Data
setwd("C:/Users/young/OneDrive/Documents/MASTERPROJECT/data/processed_data")
survey_4and5 <- read.csv("survey_4and5.csv")
site_treatment_df <- read.csv("site_treatment_df.csv")

#Merge
hh_and_site <- left_join(site_treatment_df, survey_4and5, by = case_id)

#Re-order columns
hh_and_site <- hh_and_site[,c(2, 1, 3:59)]

#Remove lack of resources columns
hh_and_site <- hh_and_site %>% 
  select(-c(starts_with("lack")))

#Remove MOD_W questions
hh_and_site <- hh_and_site %>% 
  select(-c(ends_with("2yrs")))

#Assign values to perceptions columns
hh_and_site <- hh_and_site %>% 
  mutate(perception_food = case_when(perception_food == "It was less than adequate for household needs" ~ 1,
                                     perception_food == "It was just adequate for household needs" ~ 2,
                                     perception_food == "It was more than adequate for household needs" ~ 3)) %>% 
  mutate(perception_housing = case_when(perception_housing == "It was less than adequate for household needs" ~ 1,
                                        perception_housing == "It was just adequate for household needs" ~ 2,
                                        perception_housing == "It was more than adequate for household needs" ~ 3)) %>% 
  mutate(perception_clothing = case_when(perception_clothing == "It was less than adequate for household needs" ~ 1,
                                         perception_clothing == "It was just adequate for household needs" ~ 2,
                                         perception_clothing == "It was more than adequate for household needs" ~ 3)) %>% 
  mutate(perception_healthcare = case_when(perception_healthcare == "It was less than adequate for household needs" ~ 1,
                                           perception_healthcare == "It was just adequate for household needs" ~ 2,
                                           perception_healthcare == "It was more than adequate for household needs" ~ 3))

#Give urban = 1, rural = 0
master_4and5_only <- master_4and5_only %>% 
  mutate(urban = case_when(urban == "urban" ~ 1,
                           urban == "rural" ~ 0,
                           urban %in% NA ~ 0.5))

master_345 <- master_345 %>% 
  mutate(urban = case_when(urban == "urban" ~ 1,
                           urban == "rural" ~ 0,
                           urban == 1 ~ 1,
                           urban == 0 ~ 0))



#Save
write.csv(hh_and_site, "hh_and_site.csv")

#Read back in
hh_and_site <- read.csv("hh_and_site.csv")
hh_and_site$case_id <- as.character(hh_and_site$case_id)

#rowbind ill_2wks
mod_d <- rbind(mod_d4, mod_d5, mod_d3)

hh_and_site <- left_join(hh_and_site, mod_d, by = "case_id")

hh_and_site <- hh_and_site %>% 
  select(-ill_indiv)


#Join with IHS3
master_4and5_only <- read.csv("master_4and5_only.csv")
master_4and5_only <- master_4and5_only[,-c(1:2)]
master_4and5_only <- master_4and5_only %>% 
  rename(health_consump = rexp_cat06) %>% 
  rename(drugs_consump = rexp_cat061) %>% 
  rename(outpatient_consump = rexp_cat062) %>% 
  rename(hospitalization_consump = rexp_cat063)
write.csv(master_4and5_only, "master_4and5_only.csv")
  

sofar_hh3 <- read.csv("sofar_hh3.csv")
sofar_hh3 <- sofar_hh3[,-1]
sofar_hh3$case_id <- as.character(sofar_hh3$case_id)

master_345 <- master_4and5_only

master_345 <- master_345 %>% 
  select(-c(poor, upoor, real_annual_consump_pc, popdensity))

sofar_hh3 <- left_join(contains3, sofar_hh3, by = "case_id")

master_345 <- rbind(master_345, sofar_hh3)

write.csv(master_345, "master_345.csv")


#Read back in
master_345 <- read.csv("master_345.csv")
master_345 <- master_345[,-1]

master_4and5_only <- read.csv("master_4and5_only.csv")
master_4and5_only <- master_4and5_only[,-1]

#Add year_cont column so that it takes 0, 1, 2 for 2010, 2016, 2020 respectively
master_4and5_only <- master_4and5_only %>% 
  mutate(year_cont = case_when(year2020 == 0 ~ 2016,
                               year2020 == 1 ~ 2020))

sofar_hh3$year_cont <- 2010


#Incorporate health and mortality variables added later
mod_w3 <- mod_w3 %>% distinct(case_id, .keep_all = TRUE)
mod_w4 <- mod_w4 %>% distinct(case_id, .keep_all = TRUE)
mod_w5 <- mod_w5 %>% distinct(case_id, .keep_all = TRUE)
mod_d3 <- mod_d3 %>% distinct(case_id, .keep_all = TRUE)
mod_d4 <- mod_d4 %>% distinct(case_id, .keep_all = TRUE)
mod_d5 <- mod_d5 %>% distinct(case_id, .keep_all = TRUE)

mod_w3 <- mod_w3 %>% select(-death_2yrs, -cause_death)
mod_w4 <- mod_w4 %>% select(-death_2yrs, -cause_death)
mod_w5 <- mod_w5 %>% select(-death_2yrs, -cause_death)

mod_w3$illness_cause_1 <- as.character(mod_w3$illness_cause_1)
mod_w4$illness_cause_1 <- as.character(mod_w4$illness_cause_1)
mod_w5$illness_cause_1 <- as.character(mod_w5$illness_cause_1)

modw <- rbind(mod_w3, mod_w4, mod_w5)

modd <- rbind(mod_d3, mod_d4, mod_d5)

ruralmaster <- filter(master_345, urban == 0)

ruralmaster <- ruralmaster %>% 
  left_join(modw, by = "case_id") %>% 
  left_join(modd, by = "case_id")

ruralmaster <- ruralmaster %>% 
  distinct(case_id, .keep_all = TRUE)

#Write to csv
write.csv(ruralmaster, "ruralmaster.csv")
