#####Propa PlaMV atrem3KO/CPK3 OE#7.5
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
A189_220613 <- read_csv("output_data/A189_220613.csv")
A189_220614 <- read_csv("output_data/A189_220614.csv")
A189_220615 <- read_csv("output_data/A189_220615.csv")
A189_220729 <- read_csv("output_data/A189_220729.csv")

#organize datasets
A189_assembly <- bind_rows(A189_220613, A189_220614, A189_220615, A189_220729)

#Plot data
ggboxplot(A189_assembly, x = "genotype", y = "Mean", font.tickslab = c(14), ylab = "PlaMV-GFP infection foci area (% of Col0)", font.y = c(14), add = "mean_se", size = 0.25) +
  stat_compare_means(label = "p.signif", method = "t.test", ref.group = "Col-0", hide.ns = TRUE, label.y = 550, size = 8) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 600)) + 
  rotate_x_text(angle = 40) +
  geom_jitter(size = 0.1, color = "grey") +
  theme(
    axis.ticks = element_line(size = 0.5),
    axis.line = element_line(size = 0.5)
  )

#export dataset
write.csv(A189_assembly, here::here("output_data", "A189_assembly.csv"), row.names=FALSE)