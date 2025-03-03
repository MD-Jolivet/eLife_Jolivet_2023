##Comparaison of the D of CPK3CA of 21.8.11 assay and CPK3FL of the 21.8.12 assay
#Required libraries
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(stringr) 
library(Hmisc)
library(forcats)
library(ggthemes)
library(scales)
library(readr)
library(car)
library(emmeans)
library(ggstatsplot)
library(plotrix) #to create gaps in plots
library(gg.gap) #to create gaps in plots
library(rstatix)

#Import data
GFP_220705 <- read_csv("output_data/MSD_Col0_MD157_2_GFP_220705.csv")
PlaMV_220705 <- read_csv("output_data/MSD_Col0_MD157_2_PlaMV_220705.csv")

GFP_220805 <- read_csv("output_data/MSD_Col0_MD157_2_GFP_220805.csv")
PlaMV_220805 <- read_csv("output_data/MSD_Col0_MD157_2_PlaMV_220805.csv")

GFP_220810 <- read_csv("output_data/MSD_Col0_MD157_2_GFP_220810.csv")
PlaMV_220810 <- read_csv("output_data/MSD_Col0_MD157_2_PlaMV_220810.csv")

GFP_220921 <- read_csv("output_data/MSD_Col0_MD157_2_GFP_220921.csv")
PlaMV_220921 <- read_csv("output_data/MSD_Col0_MD157_2_PlaMV_220921.csv")


MD157_220705 <- bind_rows(GFP_220705, PlaMV_220705)
ggline(MD157_220705, x= "interval", y="MSD", merge = TRUE, add = c("mean_se"), color = "Condition")

MD157_220805 <- bind_rows(GFP_220805, PlaMV_220805)
ggline(MD157_220805, x= "interval", y="MSD", merge = TRUE, add = c("mean_se"), color = "Condition")

MD157_220810 <- bind_rows(GFP_220810, PlaMV_220810)
ggline(MD157_220810, x= "interval", y="MSD", merge = TRUE, add = c("mean_se"), color = "Condition")

MD157_220921 <- bind_rows(GFP_220921, PlaMV_220921)
ggline(MD157_220921, x= "interval", y="MSD", merge = TRUE, add = c("mean_se"), color = "Condition")


MD157_2 <- bind_rows(GFP_220805, PlaMV_220805,
                     GFP_220810, PlaMV_220810,
                     GFP_220921, PlaMV_220921)

MD157_2 <- MD157_2 %>% 
  filter(interval %in% c("x0", "x0.05", "x0.1", "x0.15", "x0.2", "x0.25", "x0.3", "x0.35", "x0.4", "x0.45", "x0.5"))
MD157_2 <- MD157_2 %>%
  filter(!is.na(MSD))

#Display density plot
ggline(MD157_2, x= "interval", y="MSD", merge = TRUE, add = c("mean_se"), color = "Condition")

#Export dataset containing the four streams
write.csv(MD157_2, file ="output_data/MSD/Col0_MD157_2_merged.csv")