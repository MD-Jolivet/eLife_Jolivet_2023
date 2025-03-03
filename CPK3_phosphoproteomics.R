#####Quantification protein quantity -/+ PlaMV
#Libraries
library(tidyverse)
library(pastecs)
library(car)
library(emmeans)
library(ggsignif)
library(ggpubr)
library(readr)

#Import datasets
MS <- read_delim("input_data/MS_CPK3.csv", 
                 ";", escape_double = FALSE, trim_ws = TRUE)

MS_S134 <- MS %>%
  filter(!Residue %in% c("S195", "S353"))
MS_S195 <- MS %>%
  filter(!Residue %in% c("S134", "S353"))
MS_S353 <- MS %>%
  filter(!Residue %in% c("S134", "S195"))


ggboxplot(MS_S134, x = "Condition", y = "PTM", add = "mean_se", label.y = c(60), hide.ns = FALSE) +
  stat_compare_means(label = "p.signif", method = "t.test", ref.group = "EGTA", hide.ns = FALSE, size = 8) +
  rotate_x_text(angle = 40) 

ggboxplot(MS_S195, x = "Condition", y = "PTM", add = "mean_se", label.y = c(60), hide.ns = FALSE) +
  stat_compare_means(label = "p.signif", method = "t.test", ref.group = "EGTA", hide.ns = FALSE, size = 8) +
  rotate_x_text(angle = 40)

ggboxplot(MS_S353, x = "Condition", y = "PTM", add = "mean_se", label.y = c(60), hide.ns = FALSE) +
  stat_compare_means(label = "p.signif", method = "t.test", ref.group = "EGTA", hide.ns = FALSE, size = 8) +
  rotate_x_text(angle = 40)

