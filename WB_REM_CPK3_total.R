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
WB <- read_delim("input_data/WB_REM_CPK3_quantifprot.csv", 
                                ";", escape_double = FALSE, trim_ws = TRUE)

WB_REM <- WB %>%
  filter(!Protein %in% c("CPK3"))

WB_CPK3 <- WB %>%
  filter(Protein == "CPK3") 


ggbarplot(WB_CPK3, x = "Condition", y = "IntDen", add = "mean_se", label.y = c(60), hide.ns = FALSE, ylab = "Integrated pixel density (A.U.)", font.y = c(14)) +
  stat_compare_means(label = "p.signif", method = "wilcox.test", ref.group = "mock", hide.ns = FALSE, size = 8) +
  rotate_x_text(angle = 40)

ggbarplot(WB_REM, x = "Protein", y = "IntDen", add = "mean_se", label.y = c(60), hide.ns = FALSE, ylab = "Integrated pixel density (A.U.)", font.y = c(14), color = "Condition", position = position_dodge(0.8)) +
  stat_compare_means(label = "p.signif", method = "wilcox.test", ref.group = "PlAMV", hide.ns = TRUE, size = 8) +
  rotate_x_text(angle = 40)
