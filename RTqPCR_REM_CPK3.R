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
RT <- read_delim("input_data/RTqPCR_REM_CPK3.csv", 
                 ";", escape_double = FALSE, trim_ws = TRUE)

RT_REM <- RT %>%
  filter(!Gene %in% c("CPK3"))

RT_CPK3 <- RT %>%
  filter(Gene == "CPK3") 


ggbarplot(RT_CPK3, x = "Condition", y = "XP", add = "mean_se", label.y = c(60), hide.ns = FALSE, ylab = "Expression of CPK3 (relative to actin)", font.y = c(14)) +
  stat_compare_means(label = "p.signif", method = "wilcox.test", ref.group = "mock", hide.ns = FALSE, size = 8) +
  rotate_x_text(angle = 40)


ggbarplot(RT_REM, x = "Gene", y = "XP", add = "mean_se", label.y = c(60), hide.ns = FALSE, ylab = "Expression of Remorins (relative to actin)", font.y = c(14), color = "Condition", position = position_dodge(0.8)) +
  stat_compare_means(label = "p.signif", method = "wilcox.test", ref.group = "mock", hide.ns = TRUE, size = 8) +
  rotate_x_text(angle = 40)
