#-------------------------------------------------------------------------------
# Script Name: clean_survey_data.R
# Author: NYZ
# Description: reads and cleans IHS household survey data, yielding "sofar_hh"
#              and round number. These are then combined into "ruralmaster.csv",
#              to which HH size and educ are added.
#-------------------------------------------------------------------------------

#Load libraries
library(tidyverse)
library(readstata13)

#-----------------------------------IHS4---------------------------------------#
setwd("C:/Users/young/OneDrive/Documents/MASTERPROJECT/data/raw_data/ihs4_household")
mod_b4 <- read.dta13("hh_mod_b.dta")
mod_c4 <- read.dta13("hh_mod_c.dta")
mod_d4 <- read.dta13("hh_mod_d.dta")
mod_e4 <- read.dta13("hh_mod_e.dta")
#mod_f4 <- read.dta13("hh_mod_f.dta")
mod_g24 <- read.dta13("hh_mod_g2.dta")
mod_h4 <- read.dta13("hh_mod_h.dta")
#mod_i14 <- read.dta13("hh_mod_i1.dta")
#mod_i24 <- read.dta13("hh_mod_i2.dta")
#mod_j4 <- read.dta13("hh_mod_j.dta")
mod_n14 <- read.dta13("hh_mod_n1.dta")
mod_t4 <- read.dta13("hh_mod_t.dta")
mod_u4 <- read.dta13("hh_mod_u.dta")
mod_w4 <- read.dta13("hh_mod_w.dta")
setwd("C:/Users/young/OneDrive/Documents/MASTERPROJECT/data/raw/data")
consumpagg_4 <- read.dta13("ihs4_consumption_aggregate.dta")
geovars_4 <- read.dta13("householdgeovariables_ihs4.dta")
#ag_mod_d <- read.dta13("ag_mod_d.dta")

#MOD_B
#Religion - just take pid=1 from each household, as assume most households' individuals practice same religion
mod_religion <- mod_b4 %>%
  filter(pid == 1) %>% 
  select("case_id", "hh_b23") %>% 
  rename("religion" = "hh_b23")

#Number of days that ate in past week
mod_daysate <- mod_b4 %>% 
  select("case_id", "hh_b08") %>% 
  group_by(case_id) %>% 
  summarise(days_ate = mean(hh_b08))


#MOD_C
mod_c4 <- mod_c4 %>% 
  select("case_id", "hh_c20") %>% 
  mutate(withdraw_indiv = case_when(hh_c20 == "yes" ~ 1,
                                    hh_c20 == "no" ~ 0,
                                    hh_c20 %in% NA ~ 0)
  ) %>% 
  select("case_id", "withdraw_indiv") %>% 
  group_by(case_id) %>% 
  mutate(withdraw_school1 = any(withdraw_indiv == 1)) %>% 
  ungroup() %>% 
  distinct(case_id, .keep_all = TRUE) %>% 
  mutate(withdraw_school = case_when(withdraw_school1 == TRUE ~ 1,
                                     withdraw_school1 == FALSE ~ 0)) %>% 
  select("case_id", "withdraw_school")


#MOD_D
ill_injury_exclude4 <- c("fracture", "wound", "pregnancy",
                         "DENTAL PROBLEM", "burn")

`%notin%` <- Negate(`%in%`)

ill_2wks <- mod_d4 %>% 
  select("case_id", "hh_d04", "hh_d05a", "hh_d05b") %>% 
  mutate(ill_indiv = case_when((hh_d04 == "yes" & (hh_d05a %notin% ill_injury_exclude4 | hh_d05b %notin% ill_injury_exclude4)) ~ 1,
                               hh_d04 == "no" ~ 0,
                               hh_d04 %in% NA ~ 0)
  ) %>% 
  select("case_id", "ill_indiv") %>% 
  group_by(case_id) %>% 
  mutate(ill_2wks = any(ill_indiv == 1)) %>% 
  ungroup() %>% 
  distinct(case_id, .keep_all = TRUE) %>% 
  mutate(ill_2wks = case_when(ill_2wks == "TRUE" ~ 1,
                              ill_2wks == "FALSE" ~ 0)) %>% 
  select("case_id", "ill_2wks")

mod_d4 <- mod_d4 %>% 
  select(case_id, hh_d08, hh_d09, hh_d10, hh_d13, hh_d14, hh_d17) %>% 
  rename(stop_activities_past_week = hh_d08) %>% 
  rename(anyone_else_stop_activities = hh_d09) %>% 
  rename(spending_4wks_illnesses = hh_d10) %>% 
  rename(hosp_12mnths = hh_d13) %>% 
  rename(cost_hosp = hh_d14) %>% 
  rename(borrow_sell_fund_treatment = hh_d17)

mod_d4 <- mod_d4 %>% 
  group_by(case_id) %>% 
  mutate(stop_activities_past_week = sum(stop_activities_past_week)) %>% 
  mutate(anyone_else_stop_activities = sum(anyone_else_stop_activities)) %>% 
  mutate(spending_4wks_illnesses = sum(spending_4wks_illnesses)) %>% 
  mutate(hosp_12mnths = case_when(hosp_12mnths == "yes" ~ 1,
                                  hosp_12mnths == "no" ~ 0)) %>% 
  mutate(cost_hosp = sum(cost_hosp)) %>% 
  mutate(borrow_sell_fund_treatment = case_when(borrow_sell_fund_treatment == "yes" ~ 1,
                                                borrow_sell_fund_treatment == "no" ~ 0)) %>% 
  distinct(case_id, .keep_all = TRUE)

#MOD_E
mod_e4 <- mod_e4 %>% 
  select("case_id", "hh_e07a") %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  group_by(case_id) %>% 
  summarise(agri_hours_worked = sum(hh_e07a))


#MOD_G2
mod_g24 <- mod_g24 %>% 
  select(-"hhid") %>% 
  rename("cereal_consump" = "hh_g08a") %>% 
  rename("roots_consump" = "hh_g08b") %>% 
  rename("nuts_consump" = "hh_g08c") %>% 
  rename("vegetables_consump" = "hh_g08d") %>% 
  rename("meatfish_consump" = "hh_g08e") %>% 
  rename("fruit_consump" = "hh_g08f") %>% 
  rename("milk_consump" = "hh_g08g") %>% 
  rename("fats_consump" = "hh_g08h") %>% 
  rename("sugar_consump" = "hh_g08i") %>% 
  rename("spices_consump" = "hh_g08j")


#MOD_H
mod_h4 <- mod_h4 %>% 
  mutate(cant_feed_fam = case_when(hh_h04 == "yes" ~ 1,
                                   hh_h04 == "no" ~ 0,
                                   hh_h04 %in% NA ~ 0)) %>% 
  select("case_id", "cant_feed_fam")


#MOD_N1
mod_n14 <- mod_n14 %>% 
  select(case_id, hh_n01) %>%
  mutate(own_nonagri_business = case_when(hh_n01 == "yes" ~ 1,
                                          hh_n01 == "no" ~ 0,
                                          hh_n01 %in% NA ~ 0)) %>% 
  select(case_id, own_nonagri_business)
  
#MOD_T
mod_t4 <- mod_t4 %>% 
  select(case_id, hh_t01, hh_t02, hh_t03, hh_t04, hh_t13, hh_t14, hh_t15,
         hh_t16, hh_t17, hh_t18, hh_t19, hh_t20) %>% 
  rename(perception_food = hh_t01) %>% 
  rename(perception_housing = hh_t02) %>% 
  rename(perception_clothing = hh_t03) %>% 
  rename(perception_healthcare = hh_t04) %>% 
  rename(lack_enough_food = hh_t13) %>% 
  rename(lack_healthy_food = hh_t14) %>% 
  rename(lack_kinds_food = hh_t15) %>% 
  rename(lack_skip_meal = hh_t16) %>% 
  rename(lack_ate_less = hh_t17) %>% 
  rename(lack_ran_out_food = hh_t18) %>%
  rename(lack_hungry_but_didnt_eat = hh_t19) %>% 
  rename(lack_no_eat_whole_day = hh_t20)


#MOD_U
#Control for whether household directly impacted by flood
mod_u4 <- mod_u4 %>% 
  filter(hh_u0a == "Floods") %>% 
  group_by(case_id) %>% 
  mutate(floods = case_when(hh_u01 == "Yes" ~ 1,
                           hh_u01 == "No" ~ 0)) %>% 
  select(case_id, floods)

#MOD_W
mod_w4 <- mod_w4 %>% 
  select(case_id, hh_w01, hh_w09, hh_w11a, hh_w11b, hh_w14, hh_w15) %>% 
  rename(death_2yrs = hh_w01) %>% 
  rename(cause_death = hh_w09) %>% 
  rename(illness_cause_1 = hh_w11a) %>% 
  rename(illness_cause_2 = hh_w11b) %>% 
  rename(loss_after_death = hh_w14) %>% 
  rename(loss_value = hh_w15)

mod_w4 <- mod_w4 %>% 
  mutate(died_from_illness = case_when(cause_death == "illness" ~ 1,
                                       cause_death %in% c("OLD AGE", "OTHER CAUSE", NA) ~ 0)) %>% 
  mutate(loss_after_death = case_when(loss_after_death == "yes" ~ 1,
                                      loss_after_death == "no" ~ 0,
                                      loss_after_death %in% NA ~ 0))

#consump_agg
consumpagg_4 <- consumpagg_4 %>% 
  select(case_id, urban, district, sdate, smonth, syear, poor, upoor,
         rexpagg, rexpaggpc, rexp_cat06, rexp_cat061, rexp_cat062, rexp_cat063) %>% 
  rename(real_annual_consump = rexpagg) %>% 
  rename(real_annual_consump_pc = rexpaggpc) %>% 
  rename(health_consump = rexp_cat06) %>%
  rename(drugs_consump = rexp_cat061) %>%
  rename(outpatient_consump = rexp_cat062) %>%
  rename(hospitalization_consump = rexp_cat063)


#geovariables
geovars_4 <- geovars_4 %>% 
  select(case_id, lat_modified, lon_modified, dist_road, dist_popcenter, 
         popdns_range, twi_mwi, af_bio_1, af_bio_12) %>% 
  rename(lat = lat_modified) %>% 
  rename(lon = lon_modified) %>% 
  rename(wetness_index = twi_mwi) %>% 
  rename(annual_mean_temp = af_bio_1) %>% 
  rename(annual_precipitation = af_bio_12) %>% 
  mutate(popdensity = case_when(popdns_range == "50-100" ~ 75,
                                popdns_range == "100-200" ~ 150,
                                popdns_range == "0-50" ~ 25,
                                popdns_range == "2000-3000" ~ 2500,
                                popdns_range == "300-400" ~ 350,
                                popdns_range == "3000-5000" ~ 4000,
                                popdns_range == "200-300" ~ 250,
                                popdns_range == "1000-2000" ~ 1500,
                                popdns_range == "" ~ 100,
                                popdns_range == "500-1000" ~ 750,
                                popdns_range == "400-500" ~ 450,
                                popdns_range == ">5000" ~ 7000)) %>% 
  select(-popdns_range)

#---------------------------------------------------------------#
#Import csv back in each time I work
so_far <- read.csv("so_far.csv")
so_far$case_id <- as.character(so_far$case_id)

so_far <- so_far %>% 
  left_join(mod_n14, by = "case_id") %>% 
  left_join(mod_t4, by = "case_id") %>% 
  left_join(mod_u4, by = "case_id") %>% 
  left_join(consumpagg_4, by = "case_id") %>% 
  left_join(geovars_4, by = "case_id")
  
#Write to csv
setwd("C:/Users/young/OneDrive/Documents/MASTERPROJECT/data/processed_data")
#write.csv(so_far, "sofar_hh4.csv")


#-----------------------------------IHS5---------------------------------------#
setwd("C:/Users/young/OneDrive/Documents/MASTERPROJECT/data/raw_data/ihs5_household")
mod_b5 <- read.dta13("HH_MOD_B.dta")
mod_c5 <- read.dta13("HH_MOD_C.dta")
mod_d5 <- read.dta13("HH_MOD_D.dta")
mod_e5 <- read.dta13("HH_MOD_E.dta")
#mod_f4 <- read.dta13("hh_mod_f.dta")
mod_g25 <- read.dta13("HH_MOD_G2.dta")
mod_h5 <- read.dta13("HH_MOD_H.dta")
#mod_i14 <- read.dta13("hh_mod_i1.dta")
#mod_i24 <- read.dta13("hh_mod_i2.dta")
#mod_j4 <- read.dta13("hh_mod_j.dta")
mod_n15 <- read.dta13("HH_MOD_N1.dta")
mod_t5 <- read.dta13("HH_MOD_T.dta")
mod_u5 <- read.dta13("HH_MOD_U.dta")
mod_w5 <- read.dta13("HH_MOD_W.dta")
consumpagg_5 <- read.dta13("ihs5_consumption_aggregate.dta")
geovars_5 <- read.dta13("householdgeovariables_ihs5.dta")
#ag_mod_d <- read.dta13("ag_mod_d.dta")

#MOD_B
#Religion - just take pid=1 from each household, as assume most households' individuals practice same religion
mod_religion <- mod_b5 %>%
  filter(PID == 1) %>% 
  select("case_id", "hh_b23") %>% 
  rename("religion" = "hh_b23")

#Number of days that ate in past week
mod_daysate <- mod_b5 %>% 
  select("case_id", "hh_b08") %>% 
  group_by(case_id) %>% 
  summarise(days_ate = mean(hh_b08))


#MOD_C
mod_c5 <- mod_c5 %>% 
  select("case_id", "hh_c20") %>% 
  mutate(withdraw_indiv = case_when(hh_c20 == "YES" ~ 1,
                                    hh_c20 == "NO" ~ 0,
                                    hh_c20 %in% NA ~ 0)
  ) %>% 
  select("case_id", "withdraw_indiv") %>% 
  group_by(case_id) %>% 
  mutate(withdraw_school1 = any(withdraw_indiv == 1)) %>% 
  ungroup() %>% 
  distinct(case_id, .keep_all = TRUE) %>% 
  mutate(withdraw_school = case_when(withdraw_school1 == TRUE ~ 1,
                                     withdraw_school1 == FALSE ~ 0)) %>% 
  select("case_id", "withdraw_school")


#MOD_D
ill_injury_exclude5 <- c(24, 25, 26, 27, 28, 29, 30, 31)


`%notin%` <- Negate(`%in%`)

mod_d5 <- mod_d5 %>% 
  select("case_id", "hh_d04", "hh_d05a", "hh_d05b") %>% 
  mutate(ill_indiv = case_when((hh_d04 == "YES" & (hh_d05a %notin% ill_injury_exclude5 | hh_d05b %notin% ill_injury_exclude5)) ~ 1,
                               hh_d04 == "NO" ~ 0,
                               hh_d04 %in% NA ~ 0)
  ) %>% 
  select("case_id", "ill_indiv") %>% 
  group_by(case_id) %>% 
  mutate(ill_2wks = any(ill_indiv == 1)) %>% 
  ungroup() %>% 
  distinct(case_id, .keep_all = TRUE) %>% 
  mutate(ill_2wks = case_when(ill_2wks == "TRUE" ~ 1,
                              ill_2wks == "FALSE" ~ 0)) %>% 
  select("case_id", "ill_2wks")

mod_d5 <- mod_d5 %>% 
  select(case_id, hh_d08, hh_d09, hh_d10, hh_d13, hh_d14, hh_d17) %>% 
  rename(stop_activities_past_week = hh_d08) %>% 
  rename(anyone_else_stop_activities = hh_d09) %>% 
  rename(spending_4wks_illnesses = hh_d10) %>% 
  rename(hosp_12mnths = hh_d13) %>% 
  rename(cost_hosp = hh_d14) %>% 
  rename(borrow_sell_fund_treatment = hh_d17)

mod_d5 <- mod_d5 %>% 
  group_by(case_id) %>% 
  mutate(stop_activities_past_week = sum(stop_activities_past_week)) %>% 
  mutate(anyone_else_stop_activities = sum(anyone_else_stop_activities)) %>% 
  mutate(spending_4wks_illnesses = sum(spending_4wks_illnesses)) %>% 
  mutate(hosp_12mnths = case_when(hosp_12mnths == "YES" ~ 1,
                                  hosp_12mnths == "NO" ~ 0)) %>% 
  mutate(cost_hosp = sum(cost_hosp)) %>% 
  mutate(borrow_sell_fund_treatment = case_when(borrow_sell_fund_treatment == "YES" ~ 1,
                                                borrow_sell_fund_treatment == "NO" ~ 0)) %>% 
  distinct(case_id, .keep_all = TRUE)


#MOD_E
mod_e5 <- mod_e5 %>% 
  select("case_id", "hh_e07a") %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  group_by(case_id) %>% 
  summarise(agri_hours_worked = sum(hh_e07a))


#MOD_G2
mod_g25 <- mod_g25 %>% 
  select(-"HHID") %>% 
  rename("cereal_consump" = "hh_g08a") %>% 
  rename("roots_consump" = "hh_g08b") %>% 
  rename("nuts_consump" = "hh_g08c") %>% 
  rename("vegetables_consump" = "hh_g08d") %>% 
  rename("meatfish_consump" = "hh_g08e") %>% 
  rename("fruit_consump" = "hh_g08f") %>% 
  rename("milk_consump" = "hh_g08g") %>% 
  rename("fats_consump" = "hh_g08h") %>% 
  rename("sugar_consump" = "hh_g08i") %>% 
  rename("spices_consump" = "hh_g08j")


#MOD_H
mod_h5 <- mod_h5 %>% 
  mutate(cant_feed_fam = case_when(hh_h04 == "YES" ~ 1,
                                   hh_h04 == "NO" ~ 0,
                                   hh_h04 %in% NA ~ 0)) %>% 
  select("case_id", "cant_feed_fam")


#MOD_N1
mod_n15 <- mod_n15 %>% 
  select(case_id, hh_n01) %>%
  mutate(own_nonagri_business = case_when(hh_n01 == "YES" ~ 1,
                                          hh_n01 == "NO" ~ 0,
                                          hh_n01 %in% NA ~ 0)) %>% 
  select(case_id, own_nonagri_business)

#MOD_T
mod_t5 <- mod_t5 %>% 
  select(case_id, hh_t01, hh_t02, hh_t03, hh_t04, hh_t13, hh_t14, hh_t15,
         hh_t16, hh_t17, hh_t18, hh_t19, hh_t20) %>% 
  rename(perception_food = hh_t01) %>% 
  rename(perception_housing = hh_t02) %>% 
  rename(perception_clothing = hh_t03) %>% 
  rename(perception_healthcare = hh_t04) %>% 
  rename(lack_enough_food = hh_t13) %>% 
  rename(lack_healthy_food = hh_t14) %>% 
  rename(lack_kinds_food = hh_t15) %>% 
  rename(lack_skip_meal = hh_t16) %>% 
  rename(lack_ate_less = hh_t17) %>% 
  rename(lack_ran_out_food = hh_t18) %>%
  rename(lack_hungry_but_didnt_eat = hh_t19) %>% 
  rename(lack_no_eat_whole_day = hh_t20)


#MOD_U
#Control for whether household directly impacted by flood
mod_u5 <- mod_u5 %>% 
  filter(hh_u0a == "Floods") %>% 
  group_by(case_id) %>% 
  mutate(floods = case_when(hh_u01 == "Yes" ~ 1,
                            hh_u01 == "No" ~ 0)) %>% 
  select(case_id, floods)

#MOD_W
mod_w5 <- mod_w5 %>% 
  select(case_id, hh_w01, hh_w09, hh_w11a, hh_w11b, hh_w14, hh_w15) %>% 
  rename(death_2yrs = hh_w01) %>% 
  rename(cause_death = hh_w09) %>% 
  rename(illness_cause_1 = hh_w11a) %>% 
  rename(illness_cause_2 = hh_w11b) %>% 
  rename(loss_after_death = hh_w14) %>% 
  rename(loss_value = hh_w15)

mod_w5 <- mod_w5 %>% 
  mutate(died_from_illness = case_when(cause_death == "ILLNESS" ~ 1,
                                       cause_death %in% c("OLD AGE", "OTHER CAUSE", NA) ~ 0)) %>% 
  mutate(loss_after_death = case_when(loss_after_death == "YES" ~ 1,
                                      loss_after_death == "NO" ~ 0,
                                      loss_after_death %in% NA ~ 0))


#consump_agg
consumpagg_5 <- consumpagg_5 %>% 
  select(case_id, urban, district, sdate, smonth, syear, poor, upoor,
         rexpagg, rexpaggpc, rexp_cat06, rexp_cat061, rexp_cat062, rexp_cat063) %>% 
  rename(real_annual_consump = rexpagg) %>% 
  rename(real_annual_consump_pc = rexpaggpc) %>% 
  rename(health_consump = rexp_cat06) %>%
  rename(drugs_consump = rexp_cat061) %>%
  rename(outpatient_consump = rexp_cat062) %>%
  rename(hospitalization_consump = rexp_cat063)


#geovariables
geovars_5 <- geovars_5 %>% 
  select(case_id, ea_lat_mod, ea_lon_mod, dist_road, dist_popcenter, 
         popdensity, twi_mwi, af_bio_1_x, af_bio_12_x) %>% 
  rename(lat = ea_lat_mod) %>% 
  rename(lon = ea_lon_mod) %>% 
  rename(wetness_index = twi_mwi) %>% 
  rename(annual_mean_temp = af_bio_1_x) %>% 
  rename(annual_precipitation = af_bio_12_x)

#Merge all IHS5
sofar_hh5 <- mod_religion %>% 
  left_join(mod_daysate, by = "case_id") %>% 
  left_join(mod_c5, by = "case_id") %>% 
  left_join(mod_d5, by = "case_id") %>% 
  left_join(mod_e5, by = "case_id") %>% 
  left_join(mod_g25, by = "case_id") %>%
  left_join(mod_h5, by = "case_id") %>%
  left_join(mod_n15, by = "case_id") %>%
  left_join(mod_t5, by = "case_id") %>%
  left_join(mod_u5, by = "case_id") %>%
  left_join(consumpagg_5, by = "case_id") %>% 
  left_join(geovars_5, by = "case_id")

sofar_hh4$year2020 <- 0
sofar_hh5$year2020 <- 1

#Write to csv
setwd("C:/Users/young/OneDrive/Documents/MASTERPROJECT/data/processed_data")
# write.csv(sofar_hh4, "sofar_hh4.csv")
# write.csv(sofar_hh5, "sofar_hh5.csv")


#---------------------------------COMBINING------------------------------------#
#Row bind
survey_4and5 <- rbind(sofar_hh4, sofar_hh5)

#Make sure all variables coded in same way for both years
survey_4and5$religion <- tolower(survey_4and5$religion)

survey_4and5$lack_enough_food <- tolower(survey_4and5$lack_enough_food)
survey_4and5$lack_healthy_food <- tolower(survey_4and5$lack_healthy_food)
survey_4and5$lack_kinds_food <- tolower(survey_4and5$lack_kinds_food)
survey_4and5$lack_skip_meal <- tolower(survey_4and5$lack_skip_meal)
survey_4and5$lack_ate_less <- tolower(survey_4and5$lack_ate_less)
survey_4and5$lack_ran_out_food <- tolower(survey_4and5$lack_ran_out_food)
survey_4and5$lack_hungry_but_didnt_eat <- tolower(survey_4and5$lack_hungry_but_didnt_eat)
survey_4and5$lack_no_eat_whole_day <- tolower(survey_4and5$lack_no_eat_whole_day)

survey_4and5$any_member_die_2yrs <- tolower(survey_4and5$any_member_die_2yrs)
survey_4and5$cause_of_death_2yrs <- tolower(survey_4and5$cause_of_death_2yrs)
survey_4and5$illness_cause1_2yrs <- tolower(survey_4and5$illness_cause1_2yrs)
survey_4and5$illness_cause2_2yrs <- tolower(survey_4and5$illness_cause2_2yrs)

survey_4and5$urban <- tolower(survey_4and5$urban)

survey_4and5 <- survey_4and5 %>% 
  mutate(poor = case_when(poor == "Poor" ~ 1,
                          poor == "Non-poor" ~ 0,
                          poor == "1" ~ 1,
                          poor == "0" ~ 0))

survey_4and5 <- survey_4and5 %>% 
  mutate(upoor = case_when(upoor == "Ultra-poor" ~ 1,
                          upoor == "Non-ultra-poor" ~ 0,
                          upoor == "1" ~ 1,
                          upoor == "0" ~ 0))

#Write to csv
setwd("C:/Users/young/OneDrive/Documents/MASTERPROJECT/data/processed_data")
write.csv(survey_4and5, "survey_4and5.csv")


#-----------------------------------IHS3---------------------------------------#
setwd("C:/Users/young/OneDrive/Documents/MASTERPROJECT/ihs3_household")
mod_a3 <- read.dta13("hh_mod_a_filt.dta")
mod_b3 <- read.dta13("hh_mod_b.dta")
mod_c3 <- read.dta13("hh_mod_c.dta")
mod_d3 <- read.dta13("hh_mod_d.dta")
mod_e3 <- read.dta13("hh_mod_e.dta")
#mod_f3 <- read.dta13("hh_mod_f.dta")
mod_g23 <- read.dta13("hh_mod_g2.dta")
mod_h3 <- read.dta13("hh_mod_h.dta")
#mod_i13 <- read.dta13("hh_mod_i1.dta")
#mod_i23 <- read.dta13("hh_mod_i2.dta")
#mod_j3 <- read.dta13("hh_mod_j.dta")
mod_n13 <- read.dta13("hh_mod_n1.dta")
mod_t3 <- read.dta13("hh_mod_t.dta")
mod_u3 <- read.dta13("hh_mod_u.dta")
mod_w3 <- read.dta13("hh_mod_w.dta")
consumpagg_3 <- read.dta13("ihs3_consumption_aggregate.dta")
geovars_3 <- read.dta13("householdgeovariables_ihs3.dta")
#ag_mod_d <- read.dta13("ag_mod_d.dta")

#MOD_A
mod_a3 <- mod_a3 %>% 
  select(case_id, reside, hh_a01, hh_a23a_1, hh_a23b_1, hh_a23c_1) %>% 
  rename(urban = reside) %>% 
  rename(district = hh_a01) %>% 
  rename(sdate = hh_a23a_1) %>% 
  rename(smonth = hh_a23b_1) %>% 
  rename(syear = hh_a23c_1)

#MOD_B
#Religion - just take pid=1 from each household, as assume most households' individuals practice same religion
mod_religion <- mod_b3 %>%
  filter(id_code == 1) %>% 
  select("case_id", "hh_b23") %>% 
  rename("religion" = "hh_b23")

#Number of days that ate in past week
mod_daysate <- mod_b3 %>% 
  select("case_id", "hh_b08") %>% 
  group_by(case_id) %>% 
  summarise(days_ate = mean(hh_b08))


#MOD_C
mod_c3 <- mod_c3 %>% 
  select("case_id", "hh_c20") %>% 
  mutate(withdraw_indiv = case_when(hh_c20 == "Yes" ~ 1,
                                    hh_c20 == "No" ~ 0,
                                    hh_c20 %in% NA ~ 0)
  ) %>% 
  select("case_id", "withdraw_indiv") %>% 
  group_by(case_id) %>% 
  mutate(withdraw_school1 = any(withdraw_indiv == 1)) %>% 
  ungroup() %>% 
  distinct(case_id, .keep_all = TRUE) %>% 
  mutate(withdraw_school = case_when(withdraw_school1 == TRUE ~ 1,
                                     withdraw_school1 == FALSE ~ 0)) %>% 
  select("case_id", "withdraw_school")


#MOD_D
ill_injury_exclude3 <- c(24, 25, 26, 27, 28)
  
`%notin%` <- Negate(`%in%`)

mod_d3 <- mod_d3 %>% 
  select("case_id", "hh_d04", "hh_d05a", "hh_d05b") %>% 
  mutate(ill_indiv = case_when((hh_d04 == "Yes" & (hh_d05a %notin% ill_injury_exclude3 | hh_d05b %notin% ill_injury_exclude3)) ~ 1,
                               hh_d04 == "No" ~ 0,
                               hh_d04 %in% NA ~ 0)
  ) %>% 
  select("case_id", "ill_indiv") %>% 
  group_by(case_id) %>% 
  mutate(ill_2wks = any(ill_indiv == 1)) %>% 
  ungroup() %>% 
  distinct(case_id, .keep_all = TRUE) %>% 
  mutate(ill_2wks = case_when(ill_2wks == "TRUE" ~ 1,
                              ill_2wks == "FALSE" ~ 0)) %>% 
  select("case_id", "ill_2wks")


mod_d3 <- mod_d3 %>% 
  select(case_id, hh_d08, hh_d09, hh_d10, hh_d13, hh_d14, hh_d17) %>% 
  rename(stop_activities_past_week = hh_d08) %>% 
  rename(anyone_else_stop_activities = hh_d09) %>% 
  rename(spending_4wks_illnesses = hh_d10) %>% 
  rename(hosp_12mnths = hh_d13) %>% 
  rename(cost_hosp = hh_d14) %>% 
  rename(borrow_sell_fund_treatment = hh_d17)

mod_d3 <- mod_d3 %>% 
  group_by(case_id) %>% 
  mutate(stop_activities_past_week = sum(stop_activities_past_week)) %>% 
  mutate(anyone_else_stop_activities = sum(anyone_else_stop_activities)) %>% 
  mutate(spending_4wks_illnesses = sum(spending_4wks_illnesses)) %>% 
  mutate(hosp_12mnths = sum(hosp_12mnths)) %>% 
  mutate(cost_hosp = sum(cost_hosp)) %>% 
  mutate(borrow_sell_fund_treatment = sum(borrow_sell_fund_treatment)) %>% 
  distinct(case_id, .keep_all = TRUE)


#MOD_E
mod_e3 <- mod_e3 %>% 
  select("case_id", "hh_e07") %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  group_by(case_id) %>% 
  summarise(agri_hours_worked = sum(hh_e07))


#MOD_G2
mod_g23 <- mod_g23 %>% 
  select(-c("visit", "ea_id", "hh_g08a")) %>%
  pivot_wider(names_from = hh_g08b, values_from = hh_g08c) %>% 
  rename("cereal_consump" = "Cereals, Grains and Cereal Products") %>% 
  rename("roots_consump" = "Root, Tubers and Plantains") %>% 
  rename("nuts_consump" = "Nuts and Pulses") %>% 
  rename("vegetables_consump" = "Vegetables") %>% 
  rename("meatfish_consump" = "Meat, Fish and Animal Products") %>% 
  rename("fruit_consump" = "Fruits") %>% 
  rename("milk_consump" = "Milk/Milk Products") %>% 
  rename("fats_consump" = "Fats/Oil") %>% 
  rename("sugar_consump" = "Sugar/Sugar Products/Honey") %>% 
  rename("spices_consump" = "Spices/Condiments")


#MOD_H
mod_h3 <- mod_h3 %>% 
  mutate(cant_feed_fam = case_when(hh_h04 == "Yes" ~ 1,
                                   hh_h04 == "No" ~ 0,
                                   hh_h04 %in% NA ~ 0)) %>% 
  select("case_id", "cant_feed_fam")


#MOD_N1
mod_n13 <- mod_n13 %>% 
  select(case_id, hh_n01) %>%
  mutate(own_nonagri_business = case_when(hh_n01 == "yes" ~ 1,
                                          hh_n01 == "no" ~ 0,
                                          hh_n01 %in% NA ~ 0)) %>% 
  select(case_id, own_nonagri_business)

#MOD_T
mod_t3 <- mod_t3 %>% 
  mutate(perception_food = case_when(hh_t01 == "It was less than adequate for household needs" ~ 1,
                                     hh_t01 == "It was just adequate for household needs" ~ 2,
                                     hh_t01 == "It was more than adequate for household needs" ~ 3)) %>% 
  mutate(perception_housing = case_when(hh_t02 == "It was less than adequate for household needs" ~ 1,
                                        hh_t02 == "It was just adequate for household needs" ~ 2,
                                        hh_t02 == "It was more than adequate for household needs" ~ 3)) %>% 
  mutate(perception_clothing = case_when(hh_t01 == "It was less than adequate for household needs" ~ 1,
                                         hh_t01 == "It was just adequate for household needs" ~ 2,
                                         hh_t01 == "It was more than adequate for household needs" ~ 3)) %>% 
  mutate(perception_healthcare = case_when(hh_t01 == "It was less than adequate for household needs" ~ 1,
                                           hh_t01 == "It was just adequate for household needs" ~ 2,
                                           hh_t01 == "It was more than adequate for household needs" ~ 3)) %>% 
  select(case_id, perception_food, perception_housing, perception_clothing,
         perception_healthcare)
           

#MOD_U
#Control for whether household directly impacted by flood
mod_u3 <- mod_u3 %>% 
  filter(hh_u0a == "Floods/Landslides") %>% 
  group_by(case_id) %>% 
  mutate(floods = case_when(hh_u01 == "Yes" ~ 1,
                            hh_u01 == "No" ~ 0)) %>% 
  select(case_id, floods)

#MOD_W
mod_w3 <- mod_w3 %>% 
  select(case_id, hh_w01, hh_w09, hh_w11a, hh_w11b, hh_w14, hh_w15) %>% 
  rename(death_2yrs = hh_w01) %>% 
  rename(cause_death = hh_w09) %>% 
  rename(illness_cause_1 = hh_w11a) %>% 
  rename(illness_cause_2 = hh_w11b) %>% 
  rename(loss_after_death = hh_w14) %>% 
  rename(loss_value = hh_w15)

mod_w3 <- mod_w3 %>% 
  mutate(died_from_illness = case_when(cause_death == "Illness" ~ 1,
                                       cause_death %in% c("Old Age", "Other cause", NA) ~ 0)) %>% 
  mutate(loss_after_death = case_when(loss_after_death == "Yes" ~ 1,
                                      loss_after_death == "No" ~ 0,
                                      loss_after_death %in% NA ~ 0))


#consump_agg
consumpagg_3 <- consumpagg_3 %>% 
  select(case_id, rexpagg, rexp_cat06, rexp_cat061, rexp_cat062, rexp_cat063) %>% 
  rename(real_annual_consump = rexpagg) %>% 
  rename(health_consump = rexp_cat06) %>%
  rename(drugs_consump = rexp_cat061) %>%
  rename(outpatient_consump = rexp_cat062) %>%
  rename(hospitalization_consump = rexp_cat063)


#geovariables
geovars_3 <- geovars_3 %>% 
  select(case_id, lat_modified, lon_modified, dist_road, dist_popcenter, 
         twi_mwi, af_bio_1, af_bio_12) %>% 
  rename(lat = lat_modified) %>% 
  rename(lon = lon_modified) %>% 
  rename(wetness_index = twi_mwi) %>% 
  rename(annual_mean_temp = af_bio_1) %>% 
  rename(annual_precipitation = af_bio_12)

#Merge all IHS3
sofar_hh3 <- mod_religion %>% 
  left_join(mod_daysate, by = "case_id") %>% 
  left_join(mod_a3, by = "case_id") %>% 
  left_join(mod_c3, by = "case_id") %>% 
  left_join(mod_d3, by = "case_id") %>% 
  left_join(mod_e3, by = "case_id") %>% 
  left_join(mod_g23, by = "case_id") %>%
  left_join(mod_h3, by = "case_id") %>%
  left_join(mod_n13, by = "case_id") %>%
  left_join(mod_t3, by = "case_id") %>%
  left_join(mod_u3, by = "case_id") %>%
  left_join(consumpagg_3, by = "case_id") %>% 
  left_join(geovars_3, by = "case_id")

sofar_hh3$year2020 <- 0

#Write to csv
setwd("C:/Users/young/OneDrive/Documents/MASTERPROJECT/data/processed_data")
write.csv(sofar_hh3, "sofar_hh3.csv")
sofar_hh3 <- read.csv("sofar_hh3.csv")

sofar_hh3$religion <- tolower(sofar_hh3$religion)

#-------------------------------------------------------------------------------
#Separate script combines all three survey rounds into "ruralmaster.csv"
#-------------------------------------------------------------------------------

#-----------------Adding HH size and educ to ruralmaster------------------------

setwd("C:/Users/young/OneDrive/Documents/MASTERPROJECT/data/processed_data")
`%notin%` <- Negate(`%in%`)

#IHS3
ihs3_size <- read.dta13("C:/Users/young/OneDrive/Documents/MASTERPROJECT/ihs3_household/hh_mod_b.dta")
ihs3_size <- ihs3_size %>% 
  group_by(case_id) %>%
  summarise(hh_size = n())

ihs3_educ <- read.dta13("C:/Users/young/OneDrive/Documents/MASTERPROJECT/ihs3_household/hh_mod_o.dta")
ihs3_educ <- ihs3_educ %>% 
  select(case_id, hh_o04) %>% 
  rename(highest_educ = hh_o04) %>% 
  mutate(highest_educ = case_when(highest_educ == "Nursery/Pre school" ~ 0,
                                  highest_educ == "std1" ~ 1,
                                  highest_educ == "std2" ~ 2,
                                  highest_educ == "std3" ~ 3,
                                  highest_educ == "std4" ~ 4,
                                  highest_educ == "std5" ~ 5,
                                  highest_educ == "std6" ~ 6,
                                  highest_educ == "std7" ~ 7,
                                  highest_educ == "std8" ~ 8,
                                  highest_educ == "form1" ~ 9,
                                  highest_educ == "form2" ~ 10,
                                  highest_educ == "form3" ~ 11,
                                  highest_educ == "form4" ~ 12,
                                  highest_educ == "form5" ~ 13,
                                  highest_educ == "form6" ~ 14,
                                  highest_educ == "univ1" ~ 15,
                                  highest_educ == "univ2" ~ 16,
                                  highest_educ == "univ3" ~ 17,
                                  highest_educ == "univ4" ~ 18,
                                  highest_educ == "univ5 & above" ~ 19,
                                  highest_educ == "TC Yr 1" ~ 20,
                                  highest_educ == "TC Yr 2" ~ 21,
                                  highest_educ == "TC Yr 3" ~ 22,
                                  highest_educ == "TC Yr 4" ~ 23))

ihs3_educ_nas <- ihs3_educ %>% 
  filter_all(any_vars(is.na(.))) %>% 
  distinct(case_id, .keep_all = TRUE)

ihs3_educ <- ihs3_educ %>% 
  group_by(case_id) %>% 
  slice_max(highest_educ) %>% 
  distinct(case_id, .keep_all = TRUE)

educ_nonna_cases <- as.list(ihs3_educ$case_id)

ihs3_educ_nas <- ihs3_educ_nas %>% 
  filter(case_id %notin% educ_nonna_cases)

ihs3_educ <- rbind(ihs3_educ, ihs3_educ_nas)

ihs3 <- left_join(ihs3_size, ihs3_educ, by = "case_id")


#IHS4
ihs4_size <- read.dta13("ihs4_consumption_aggregate.dta")
ihs4_size <- ihs4_size %>% 
  select(case_id, hhsize) %>% 
  rename(hh_size = hhsize)

ihs4_educ <- read.dta13("C:/Users/young/OneDrive/Documents/MASTERPROJECT/ihs4_household/hh_mod_o.dta")
ihs4_educ <- ihs4_educ %>% 
  select(case_id, hh_o04) %>% 
  rename(highest_educ = hh_o04) %>% 
  mutate(highest_educ = case_when(highest_educ == "Nursery/Pre school" ~ 0,
                                  highest_educ == "std1" ~ 1,
                                  highest_educ == "std2" ~ 2,
                                  highest_educ == "std3" ~ 3,
                                  highest_educ == "std4" ~ 4,
                                  highest_educ == "std5" ~ 5,
                                  highest_educ == "std6" ~ 6,
                                  highest_educ == "std7" ~ 7,
                                  highest_educ == "std8" ~ 8,
                                  highest_educ == "form1" ~ 9,
                                  highest_educ == "form2" ~ 10,
                                  highest_educ == "form3" ~ 11,
                                  highest_educ == "form4" ~ 12,
                                  highest_educ == "form5" ~ 13,
                                  highest_educ == "form6" ~ 14,
                                  highest_educ == "univ1" ~ 15,
                                  highest_educ == "univ2" ~ 16,
                                  highest_educ == "univ3" ~ 17,
                                  highest_educ == "univ4" ~ 18,
                                  highest_educ == "univ5 & above" ~ 19,
                                  highest_educ == "TC Yr 1" ~ 20,
                                  highest_educ == "TC Yr 2" ~ 21,
                                  highest_educ == "TC Yr 3" ~ 22,
                                  highest_educ == "TC Yr 4" ~ 23))

ihs4_educ_nas <- ihs4_educ %>% 
  filter_all(any_vars(is.na(.))) %>% 
  distinct(case_id, .keep_all = TRUE)

ihs4_educ <- ihs4_educ %>% 
  group_by(case_id) %>% 
  slice_max(highest_educ) %>% 
  distinct(case_id, .keep_all = TRUE)

educ_nonna_cases <- as.list(ihs4_educ$case_id)

ihs4_educ_nas <- ihs4_educ_nas %>% 
  filter(case_id %notin% educ_nonna_cases)

ihs4_educ <- rbind(ihs4_educ, ihs4_educ_nas)

ihs4 <- left_join(ihs4_size, ihs4_educ, by = "case_id")

#IHS5
ihs5_size <- read.dta13("ihs5_consumption_aggregate.dta")
ihs5_size <- ihs5_size %>% 
  select(case_id, hhsize) %>% 
  rename(hh_size = hhsize)

ihs5_educ <- read.dta13("C:/Users/young/OneDrive/Documents/MASTERPROJECT/ihs5_household/hh_mod_o.dta")
ihs5_educ <- ihs5_educ %>% 
  select(case_id, hh_o04) %>% 
  rename(highest_educ = hh_o04) %>% 
  mutate(highest_educ = case_when(highest_educ == "Nursery/Pre school" ~ 0,
                                  highest_educ == "std1" ~ 1,
                                  highest_educ == "std2" ~ 2,
                                  highest_educ == "std3" ~ 3,
                                  highest_educ == "std4" ~ 4,
                                  highest_educ == "std5" ~ 5,
                                  highest_educ == "std6" ~ 6,
                                  highest_educ == "std7" ~ 7,
                                  highest_educ == "std8" ~ 8,
                                  highest_educ == "form1" ~ 9,
                                  highest_educ == "form2" ~ 10,
                                  highest_educ == "form3" ~ 11,
                                  highest_educ == "form4" ~ 12,
                                  highest_educ == "form5" ~ 13,
                                  highest_educ == "form6" ~ 14,
                                  highest_educ == "univ1" ~ 15,
                                  highest_educ == "univ2" ~ 16,
                                  highest_educ == "univ3" ~ 17,
                                  highest_educ == "univ4" ~ 18,
                                  highest_educ == "univ5 & above" ~ 19,
                                  highest_educ == "TC Yr 1" ~ 20,
                                  highest_educ == "TC Yr 2" ~ 21,
                                  highest_educ == "TC Yr 3" ~ 22,
                                  highest_educ == "TC Yr 4" ~ 23))

ihs5_educ_nas <- ihs5_educ %>% 
  filter_all(any_vars(is.na(.))) %>% 
  distinct(case_id, .keep_all = TRUE)

ihs5_educ <- ihs5_educ %>% 
  group_by(case_id) %>% 
  slice_max(highest_educ) %>% 
  distinct(case_id, .keep_all = TRUE)

educ_nonna_cases <- as.list(ihs5_educ$case_id)

ihs5_educ_nas <- ihs5_educ_nas %>% 
  filter(case_id %notin% educ_nonna_cases)

ihs5_educ <- rbind(ihs5_educ, ihs5_educ_nas)

ihs5 <- left_join(ihs5_size, ihs5_educ, by = "case_id")

#All together
educ_and_size <- rbind(ihs3, ihs4, ihs5)
ruralmaster <- left_join(ruralmaster, educ_and_size, by = "case_id")

#Write to csv
setwd("C:/Users/young/OneDrive/Documents/MASTERPROJECT/data/processed_data")
#write.csv(ruralmaster, "ruralmaster.csv")

