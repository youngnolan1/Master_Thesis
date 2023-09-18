#-------------------------------------------------------------------------------
# Script Name: placebo_regs.R
# Author: NYZ
# Description: performs placebo test regressions and produces tex results tables.
#-------------------------------------------------------------------------------


#Libraries
library(tidyverse)
library(stargazer)

#Data
setwd("C:/Users/young/OneDrive/Documents/MASTERPROJECT/data/processed_data")
pre_test_data <- read.csv("ruralmaster_2010incl.csv")

#Clean
pre_test_data <- pre_test_data %>% 
  filter(year2020 != 1) %>% 
  mutate(year2016 = case_when(year_cont == 2016 ~ 1,
                              year_cont != 2016 ~ 0)) %>% 
  mutate(pre_did = binary_treat*year2016)

#Regs
hosp_reg <- lm(hosp_12mnths ~ year2016 + binary_treat + pre_did + annual_mean_temp + annual_precipitation + floods + real_annual_consump + hh_size, 
              data = pre_test_data)

ill_reg <- lm(ill_2wks ~ year2016 + binary_treat + pre_did + annual_mean_temp + annual_precipitation + floods + real_annual_consump + hh_size, 
                  data = pre_test_data)


health_reg <- lm(health_consump ~ year2016 + binary_treat + pre_did + annual_mean_temp + annual_precipitation + floods + real_annual_consump + hh_size, 
                  data = pre_test_data)


outpatient_reg <- lm(outpatient_consump ~ year2016 + binary_treat + pre_did + annual_mean_temp + annual_precipitation + floods + real_annual_consump + hh_size, 
                  data = pre_test_data)


spending_reg <- lm(spending_4wks_illnesses ~ year2016 + binary_treat + pre_did + annual_mean_temp + annual_precipitation + floods + real_annual_consump + hh_size, 
                  data = pre_test_data)


perception_reg <- lm(perception_healthcare ~ year2016 + binary_treat + pre_did + annual_mean_temp + annual_precipitation + floods + real_annual_consump + hh_size, 
                  data = pre_test_data)

#Output tex files
setwd("C:/Users/young/OneDrive/Documents/MASTERPROJECT/output")

stargazer(hosp_reg, ill_reg, spending_reg,
          type = "latex",
          title = "Placebo Test Results",
          dep.var.labels = c("Hospitalization", "Illness", "Spending on Illness"),
          covariate.labels = c("Post", "Treated", "Diff-in-Diff", "Annual Mean Temp", "Annual Mean Precip", "Floods", "Real Annual Consump", "HH Size"),
          out = "placebo_main_table.tex")

stargazer(health_reg, outpatient_reg, perception_reg,
          type = "latex",
          title = "Placebo Test Results - Health and Outpatient Consumption and Perception of Healthcare Standards",
          dep.var.labels = c("Health Consump", "Outpatient Consump", "Perception Healthcare"),
          covariate.labels = c("Post", "Treated", "Diff-in-Diff", "Annual Mean Temp", "Annual Mean Precip", "Floods", "Real Annual Consump", "HH Size"),
          out = "placebo_appendix_table.tex")
