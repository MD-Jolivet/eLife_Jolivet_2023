#####Peak value Diameter
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
CPK3 <- read_delim("input_data/tesselation/CPK3_transient/CPK3_Diameter_peak.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

Col0_MD187 <- read_delim("input_data/tesselation/REM_stable/Col0_MD187_Diameter_peak.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

cpk3_MD187 <- read_delim("input_data/tesselation/REM_stable/cpk3_MD187_Diameter_peak.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

Col0_MD157 <- read_delim("input_data/tesselation/CPK3_stable/Col0_MD157_Diameter_peak.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

atrem3ko_MD157 <- read_delim("input_data/tesselation/CPK3_stable/atrem3ko_MD157_Diameter_peak.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#relativeArea: relative ND area to overall area
ggboxplot(CPK3, x = "Condition", y = "Mean", add = "mean", color = "black", fill = "white", size = 1) +
  stat_compare_means(ref.group = "CPK3FL_PlaMV", method = "t.test", label = "p.signif",
                     label.y = c(150), hide.ns = FALSE, size = 10) +
  geom_jitter(color = "grey", size = 1)+
  theme(axis.text = element_text(size=20))

ggboxplot(Col0_MD187, x = "Condition", y = "Mean", add = "mean", color = "black", fill = "white", size = 1) +
  stat_compare_means(ref.group = "Col0_MD187_GFP", method = "t.test", label = "p.signif",
                     label.y = c(90), hide.ns = FALSE, size = 10) +
  geom_jitter(color = "grey", size = 1)+
  theme(axis.text = element_text(size=20))

ggboxplot(cpk3_MD187, x = "Condition", y = "Mean", add = "mean", color = "black", fill = "white", size = 1) +
  stat_compare_means(ref.group = "cpk3_MD187_GFP", method = "t.test", label = "p.signif",
                     label.y = c(125), hide.ns = FALSE, size = 10) +
  geom_jitter(color = "grey", size = 1)+
  theme(axis.text = element_text(size=20))

ggboxplot(Col0_MD157, x = "Condition", y = "Mean", add = "mean", color = "black", fill = "white", size = 1) +
  stat_compare_means(ref.group = "Col0_MD157_GFP", method = "wilcox.test", label = "p.signif",
                     label.y = c(150), hide.ns = FALSE, size = 10) +
  geom_jitter(color = "grey", size = 1)+
  theme(axis.text = element_text(size=20))

ggboxplot(atrem3ko_MD157, x = "Condition", y = "Mean", add = "mean", color = "black", fill = "white", size = 1) +
  stat_compare_means(ref.group = "atrem3ko_MD157_GFP", method = "t.test", label = "p.signif",
                     label.y = c(210), hide.ns = FALSE, size = 10) +
  geom_jitter(color = "grey", size = 1)+
  theme(axis.text = element_text(size=20))