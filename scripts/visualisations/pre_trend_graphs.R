#-------------------------------------------------------------------------------
# Script Name: pre_trend_graphs.R
# Author: NYZ
# Description: performs visual inspection of the parallel pre-trends assumption,
#              producing a graph for the appendix.
#-------------------------------------------------------------------------------

#Libraries
library(grid)
library(ggnewscale)
library(ggtext)
library(tidyverse)
library(shadowtext)
library(patchwork)
library(scales)

#Data
setwd("C:/Users/young/OneDrive/Documents/MASTERPROJECT/data/processed_data")
balance <- read.csv("ruralmaster_2010incl.csv")

parallel <- balance %>% 
  filter(year_cont %in% c("2010", "2016"))

means <- parallel %>% 
  select(year_cont, binary_treat, hosp_12mnths, ill_2wks, perception_healthcare, spending_4wks_illnesses,
         health_consump, outpatient_consump, drugs_consump) %>%
  drop_na(ill_2wks, spending_4wks_illnesses) %>% 
  group_by(year_cont, binary_treat) %>% 
  summarise(across(everything(), mean)) %>% 
  arrange(year_cont) %>% 
  mutate(binary_treat = case_when(binary_treat == 1 ~ TRUE,
                                  binary_treat == 0 ~ FALSE))


#Colours
BROWN <- "#AD8C97"
BROWN_DARKER <- "#7d3a46"
GREEN <- "#2FC1D3"
BLUE <- "#076FA1"
GREY <- "#C7C9CB"
GREY_DARKER <- "#5C5B5D"
RED <- "#E3120B"


#Plot
ggplot(means, aes(x = year_cont, y = perception_healthcare)) +
  geom_line(aes(color = binary_treat), size = 1) +
  scale_y_continuous(labels = label_number(accuracy = 0.01)) +
  scale_color_manual(values = c(RED, BLUE), labels = c("Control", "Treated")) +
  labs(x = "Year", y = "Perception of Healthcare Standards") +
  theme(legend.title=element_blank()) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "#A8BAC4", size = 0.3),
    axis.ticks.length.y = unit(0, "mm"), 
    axis.ticks.length.x = unit(2, "mm"))

