#####SR-Tesseler analysis: statistics
####Libraries
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

####Import csv files to R
CPK3CA_211007_s1 <- read_csv("output_data/tesselation/CPK3_transient/CPK3CA_211007_s1_Areas.csv")
CPK3CA_211007_s4 <- read_csv("output_data/tesselation/CPK3_transient/CPK3CA_211007_s4_Areas.csv")
CPK3CA_211007_s5 <- read_csv("output_data/tesselation/CPK3_transient/CPK3CA_211007_s5_Areas.csv")

CPK3CA_211021_s1 <- read_csv("output_data/tesselation/CPK3_transient/CPK3CA_211021_s1_Areas.csv")
CPK3CA_211021_s2 <- read_csv("output_data/tesselation/CPK3_transient/CPK3CA_211021_s2_Areas.csv")
CPK3CA_211021_s3 <- read_csv("output_data/tesselation/CPK3_transient/CPK3CA_211021_s3_Areas.csv")
CPK3CA_211021_s5 <- read_csv("output_data/tesselation/CPK3_transient/CPK3CA_211021_s5_Areas.csv")
CPK3CA_211021_s6 <- read_csv("output_data/tesselation/CPK3_transient/CPK3CA_211021_s6_Areas.csv")

CPK3CA_211110_s3 <- read_csv("output_data/tesselation/CPK3_transient/CPK3CA_211110_s3_Areas.csv")
CPK3CA_211110_s4 <- read_csv("output_data/tesselation/CPK3_transient/CPK3CA_211110_s4_Areas.csv")
CPK3CA_211110_s5 <- read_csv("output_data/tesselation/CPK3_transient/CPK3CA_211110_s5_Areas.csv")
CPK3CA_211110_s6 <- read_csv("output_data/tesselation/CPK3_transient/CPK3CA_211110_s6_Areas.csv")

CPK3FL_211007_s5 <- read_csv("output_data/tesselation/CPK3_transient/CPK3FL_211007_s5_Areas.csv")
CPK3FL_211007_s7 <- read_csv("output_data/tesselation/CPK3_transient/CPK3FL_211007_s7_Areas.csv")

CPK3FL_211021_s1 <- read_csv("output_data/tesselation/CPK3_transient/CPK3FL_211021_s1_Areas.csv")
CPK3FL_211021_s3 <- read_csv("output_data/tesselation/CPK3_transient/CPK3FL_211021_s3_Areas.csv")
CPK3FL_211021_s4 <- read_csv("output_data/tesselation/CPK3_transient/CPK3FL_211021_s4_Areas.csv")

CPK3FL_211118_s1 <- read_csv("output_data/tesselation/CPK3_transient/CPK3FL_211118_s1_Areas.csv")
CPK3FL_211118_s5 <- read_csv("output_data/tesselation/CPK3_transient/CPK3FL_211118_s5_Areas.csv")

CPK3FL_PlaMV_211021_s4 <- read_csv("output_data/tesselation/CPK3_transient/CPK3FL_PlaMV_211021_s4_Areas.csv")
CPK3FL_PlaMV_211021_s6 <- read_csv("output_data/tesselation/CPK3_transient/CPK3FL_PlaMV_211021_s6_Areas.csv")
CPK3FL_PlaMV_211021_s7 <- read_csv("output_data/tesselation/CPK3_transient/CPK3FL_PlaMV_211021_s7_Areas.csv")

CPK3FL_PlaMV_211112_s2 <- read_csv("output_data/tesselation/CPK3_transient/CPK3FL_PlaMV_211112_s2_Areas.csv")
CPK3FL_PlaMV_211112_s3 <- read_csv("output_data/tesselation/CPK3_transient/CPK3FL_PlaMV_211112_s3_Areas.csv")
CPK3FL_PlaMV_211112_s4 <- read_csv("output_data/tesselation/CPK3_transient/CPK3FL_PlaMV_211112_s4_Areas.csv")
CPK3FL_PlaMV_211112_s5 <- read_csv("output_data/tesselation/CPK3_transient/CPK3FL_PlaMV_211112_s5_Areas.csv")

#mergedata
CPK3CA_211007 <- bind_rows(CPK3CA_211007_s1, CPK3CA_211007_s4, CPK3CA_211007_s5)
CPK3CA_211007 <- CPK3CA_211007 %>%
  mutate(Date = "211007")

CPK3CA_211021 <- bind_rows(CPK3CA_211021_s1, CPK3CA_211021_s2, CPK3CA_211021_s3, CPK3CA_211021_s5, CPK3CA_211021_s6)
CPK3CA_211021 <- CPK3CA_211021 %>%
  mutate(Date = "211021")

CPK3CA_211110 <- bind_rows(CPK3CA_211110_s3, CPK3CA_211110_s4, CPK3CA_211110_s5, CPK3CA_211110_s6)
CPK3CA_211110 <- CPK3CA_211110 %>%
  mutate(Date = "211110")

CPK3FL_211007 <- bind_rows(CPK3FL_211007_s5, CPK3FL_211007_s7)
CPK3FL_211007 <- CPK3FL_211007 %>%
  mutate(Date = "211007")

CPK3FL_211021 <- bind_rows(CPK3FL_211021_s1, CPK3FL_211021_s3, CPK3FL_211021_s4)
CPK3FL_211021 <- CPK3FL_211021 %>%
  mutate(Date = "211021")

CPK3FL_211118 <- bind_rows(CPK3FL_211118_s1, CPK3FL_211118_s5)
CPK3FL_211118 <- CPK3FL_211118 %>%
  mutate(Date = "211118")

CPK3FL_PlaMV_211021 <- bind_rows(CPK3FL_PlaMV_211021_s4, CPK3FL_PlaMV_211021_s6, CPK3FL_PlaMV_211021_s7)
CPK3FL_PlaMV_211021 <- CPK3FL_PlaMV_211021 %>%
  mutate(Date = "211021")

CPK3FL_PlaMV_211112 <- bind_rows(CPK3FL_PlaMV_211112_s2, CPK3FL_PlaMV_211112_s3, CPK3FL_PlaMV_211112_s4, CPK3FL_PlaMV_211112_s5)
CPK3FL_PlaMV_211112 <- CPK3FL_PlaMV_211112 %>%
  mutate(Date = "211112")

CPK3 <- bind_rows(CPK3CA_211007, CPK3CA_211021, CPK3CA_211110,
                  CPK3FL_211007, CPK3FL_211021, CPK3FL_211118,
                  CPK3FL_PlaMV_211021, CPK3FL_PlaMV_211112)

CPK3 <- CPK3 %>%
  mutate(logArea = log(Area))
CPK3 <- CPK3 %>%
  mutate(logDensity = log(Local_density))

#Display density plot
ggdensity(CPK3, y="..density..", x="Area", color ="Condition", size = 1)
ggdensity(CPK3, y="..density..", x="logArea", color ="Condition", size = 1)
ggdensity(CPK3, y="..density..", x="Diameter", color ="Condition", size = 1)
ggdensity(CPK3, y="..density..", x="Local_density", color ="Condition", size = 1)
ggdensity(CPK3, y="..density..", x="logDensity", color ="Condition", size = 1)