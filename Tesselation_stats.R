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
CPK3CA_211007_s1 <- read_csv("output_data/tesselation/CPK3_transient/CPK3CA_211007_s1.csv")
CPK3CA_211007_s4 <- read_csv("output_data/tesselation/CPK3_transient/CPK3CA_211007_s4.csv")
CPK3CA_211007_s5 <- read_csv("output_data/tesselation/CPK3_transient/CPK3CA_211007_s5.csv")

CPK3CA_211021_s1 <- read_csv("output_data/tesselation/CPK3_transient/CPK3CA_211021_s1.csv")
CPK3CA_211021_s2 <- read_csv("output_data/tesselation/CPK3_transient/CPK3CA_211021_s2.csv")
CPK3CA_211021_s3 <- read_csv("output_data/tesselation/CPK3_transient/CPK3CA_211021_s3.csv")
CPK3CA_211021_s5 <- read_csv("output_data/tesselation/CPK3_transient/CPK3CA_211021_s5.csv")
CPK3CA_211021_s6 <- read_csv("output_data/tesselation/CPK3_transient/CPK3CA_211021_s6.csv")

CPK3CA_211110_s3 <- read_csv("output_data/tesselation/CPK3_transient/CPK3CA_211110_s3.csv")
CPK3CA_211110_s4 <- read_csv("output_data/tesselation/CPK3_transient/CPK3CA_211110_s4.csv")
CPK3CA_211110_s5 <- read_csv("output_data/tesselation/CPK3_transient/CPK3CA_211110_s5.csv")
CPK3CA_211110_s6 <- read_csv("output_data/tesselation/CPK3_transient/CPK3CA_211110_s6.csv")


CPK3FL_211007_s5 <- read_csv("output_data/tesselation/CPK3_transient/CPK3FL_211007_s5.csv")
CPK3FL_211007_s7 <- read_csv("output_data/tesselation/CPK3_transient/CPK3FL_211007_s7.csv")

CPK3FL_211021_s1 <- read_csv("output_data/tesselation/CPK3_transient/CPK3FL_211021_s1.csv")
CPK3FL_211021_s3 <- read_csv("output_data/tesselation/CPK3_transient/CPK3FL_211021_s3.csv")
CPK3FL_211021_s4 <- read_csv("output_data/tesselation/CPK3_transient/CPK3FL_211021_s4.csv")

CPK3FL_211118_s1 <- read_csv("output_data/tesselation/CPK3_transient/CPK3FL_211118_s1.csv")
CPK3FL_211118_s5 <- read_csv("output_data/tesselation/CPK3_transient/CPK3FL_211118_s5.csv")


CPK3FL_PlaMV_211021_s4 <- read_csv("output_data/tesselation/CPK3_transient/CPK3FL_PlaMV_211021_s4.csv")
CPK3FL_PlaMV_211021_s6 <- read_csv("output_data/tesselation/CPK3_transient/CPK3FL_PlaMV_211021_s6.csv")
CPK3FL_PlaMV_211021_s7 <- read_csv("output_data/tesselation/CPK3_transient/CPK3FL_PlaMV_211021_s7.csv")

CPK3FL_PlaMV_211112_s2 <- read_csv("output_data/tesselation/CPK3_transient/CPK3FL_PlaMV_211112_s2.csv")
CPK3FL_PlaMV_211112_s3 <- read_csv("output_data/tesselation/CPK3_transient/CPK3FL_PlaMV_211112_s3.csv")
CPK3FL_PlaMV_211112_s4 <- read_csv("output_data/tesselation/CPK3_transient/CPK3FL_PlaMV_211112_s4.csv")
CPK3FL_PlaMV_211112_s5 <- read_csv("output_data/tesselation/CPK3_transient/CPK3FL_PlaMV_211112_s5.csv")

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

CPK3 <- bind_rows(CPK3FL_211007, CPK3FL_211021, CPK3FL_211118,
                  CPK3FL_PlaMV_211021, CPK3FL_PlaMV_211112,
                  CPK3CA_211007, CPK3CA_211021, CPK3CA_211110)

CPK3 <- CPK3 %>%
  mutate("#Clusters" = relativeClusters*1000)

#relativeArea: relative ND area to overall area
x <- list( c("CPK3FL", "CPK3CA"), c("CPK3FL", "CPK3FL_PlAMV"))
ggboxplot(CPK3, x = "Condition", y = "relativeArea", add = "mean_se", color = "black", fill = "white", size = 1) +
  stat_compare_means(comparisons = x, method = "wilcox.test", label = "p.signif",
                     label.y = c(0.032, 0.037), hide.ns = FALSE) +
  geom_jitter(color = "grey", size = 1)+
  theme(axis.text = element_text(size=20))

#relativeDetections: % of localization in ND
x <- list( c("CPK3FL", "CPK3CA"), c("CPK3FL", "CPK3FL_PlaMV"))
ggboxplot(CPK3, x = "Condition", y = "relativeDetections", add = "mean_se", color = "black", fill = "white", size = 1) +
  stat_compare_means(comparisons = x, method = "t.test", label = "p.signif",
                     label.y = c(0.5, 0.4), hide.ns = FALSE) +
  geom_jitter(color = "grey", size = 1)+
  theme(axis.text = element_text(size=20))

#relativeClusters: ND density (#/nm²)
x <- list( c("CPK3FL", "CPK3CA"), c("CPK3FL", "CPK3FL_PlAMV"))
ggboxplot(CPK3, x = "Condition", y = "#Clusters", add = "mean_se", color = "black", fill = "white", size = 1) +
  stat_compare_means(comparisons = x, method = "wilcox.test", label = "p.signif",
                     label.y = c(6, 7), hide.ns = FALSE) +
  geom_jitter(color = "grey", size = 1)+
  theme(axis.text = element_text(size=20))

#export dataset
write.csv(CPK3, here::here("output_data/tesselation", "CPK3_transient_vornoi_stats.csv"), row.names=FALSE)