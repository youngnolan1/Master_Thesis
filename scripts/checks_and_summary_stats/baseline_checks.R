#-------------------------------------------------------------------------------
# Script Name: baseline_checks.R
# Author: NYZ
# Description: perform baseline balance and parallel trends checks. Yields
#              summary tables and trends graphs.
#-------------------------------------------------------------------------------

#Libraries
library(vtable)
library(tidyverse)

#Data
setwd("C:/Users/young/OneDrive/Documents/MASTERPROJECT/data/processed_data")
balance <- read.csv("ruralmaster_2010incl.csv")

#Baseline balance - compare means and do t-tests
balance2010_16 <- balance %>% 
  filter(syear %in% c("2010", "2016")) %>% 
  select(binary_treat, annual_mean_temp, annual_precipitation,
         floods, real_annual_consump, hh_size) %>% 
  rename(Treated = binary_treat) %>% 
  mutate(annual_mean_temp = annual_mean_temp/10)

sumtable(balance2010_16, group = "Treated", group.test = TRUE,
         title = "Balance in Covariates",
         summ = c("mean(x)", "sd(x)"),
         digits = 3,
         labels = c("Temperature", "Precipitation", "Floods", "Total Consumption",
                    "HH Size"), 
         out = "latex", file = "balance_table.tex")

outcome_means <- balance %>% 
  filter(syear %in% c("2019", "2020")) %>% 
  select(binary_treat, ill_2wks, spending_4wks_illnesses, 
         health_consump, outpatient_consump,
         hosp_12mnths) %>% 
  rename(Treated = binary_treat)

sumtable(outcome_means, group = "Treated", group.test = TRUE,
         summ = c("mean(x)", "sd(x)"),
         title = "Means of Outcome Variables",
         labels = c("Illness Incidence", 
                    "Spending on Illnesses", "Health Consumption", 
                    "Outpatient Consumption", "Hospitalization"),
         out = "latex", file = "outcome_means.tex")


#Parallel trends - visual inspection (not for output to document)
ggplot(means, aes(x = year_cont, y = hosp_12mnths, color = binary_treat)) +
  geom_line() +
  xlab("Year") + 
  ylab("Hospitalization Incidence") +
  labs(color = "Treatment", title = "Parallel trends: Hospitalization Incidence") + 
  theme(
  plot.title = element_markdown(family = "Econ Sans Cnd", size = 18),
  axis.title.x = element_text(color="black", size=12),
  axis.title.y = element_text(color="black", size=12)
)
