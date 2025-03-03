#####Peak value D
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
atrem3ko_MD157 <- read_delim("input_data/D/cpk3_stable/peak_atrem3ko_MD157.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#relativeArea: relative ND area to overall area
x <- list( c("atrem3ko_MD157_GFP", "atrem3ko_MD157_PlaMV"))
ggboxplot(atrem3ko_MD157, x = "Condition", y = "Mean", add = "mean", color = "black", fill = "white", size = 1, ylim = c(-1, -0.2)) +
  stat_compare_means(comparisons = x, method = "wilcox.test", label = "p.signif",
                     label.y = c(-0.20), hide.ns = FALSE) +
  geom_jitter(color = "grey", size = 1)+
  theme(axis.text = element_text(size=20))