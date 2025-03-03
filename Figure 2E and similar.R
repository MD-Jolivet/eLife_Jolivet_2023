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
library(gg.gap) #to create gaps in plots only on y axis though
library(rstatix)

#Import data
GFP_220818 <- read_csv("output_data/atrem3ko_MD157_1_GFP_220818.csv")
PlaMV_220818 <- read_csv("output_data/atrem3ko_MD157_1_PlaMV_220818.csv")

GFP_220930 <- read_csv("output_data/atrem3ko_MD157_1_GFP_220930.csv")
PlaMV_220930 <- read_csv("output_data/atrem3ko_MD157_1_PlaMV_220930.csv")

GFP_230316 <- read_csv("output_data/atrem3ko_MD157_1_GFP_230316.csv")
PlaMV_230316 <- read_csv("output_data/atrem3ko_MD157_1_PlaMV_230316.csv")

MD157_0818 <- bind_rows(GFP_220818, PlaMV_220818)
ggdensity(MD157_0818, y="..density..", x="logD", color ="Condition")

MD157_0930 <- bind_rows(GFP_220930, PlaMV_220930)
ggdensity(MD157_0818, y="..density..", x="logD", color ="Condition")

MD157_0316 <- bind_rows(GFP_230316, PlaMV_230316)
ggdensity(MD157_0316, y="..density..", x="logD", color ="Condition")


MD157_2 <- bind_rows(MD157_0818, MD157_0930, MD157_0316)
MD157_2_2 <-subset(MD157_2, logD<"-5")
MD157_2_2 <- MD157_2_2 %>% 
  filter(Condition == "atrem3ko_MD157_1_PlaMV")

#Display density plot
ggdensity(MD157_2, y="..density..", x="logD", color ="Condition")
MD157_2_2 <-subset(MD157_2, logD<"-5")
ggdensity(MD157_2_2, y="..density..", x="logD", color ="Condition")

#Export dataset containing the four streams
write.csv(MD157_2, file ="output_data/atrem3ko_MD157_1_merged.csv")
write.csv(MD157_2_2, file ="output_data/atrem3ko_MD157_2_2_merged.csv")
