####Antilocalization plot workflow
###Libraries
library(tidyverse)
library(pastecs)
library(car) #to check distribution with qqplot function
library(emmeans) #to compare conditions two by two after anova
library(ggsignif)
library(ggpubr)
library(readr)

#Import datasets
coloc <- read_csv("CPK3_REM_airyscan.csv")
coloc_random <- read_csv("coloc_random.csv")

coloc2 <- bind_rows(coloc, coloc_random)

#Pearson
ggboxplot(coloc, x = "Condition", y = "pearson_above_threshold", add = "mean_se") +
  geom_jitter(color="grey", size=1)
ggboxplot(coloc_random, x = "Condition", y = "pearson_above_threshold", add = "mean_se") +
  geom_jitter(color="grey", size=1)
ggboxplot(coloc2, x = "Condition", y = "pearson_above_threshold", add = "mean_se", 
          color = "status", palette = c("#000000","#636262")) +
  geom_jitter(color="#c9c9c9", size=1)

#Export table propaArea_all, préciser la date dans le nom du fichier
write.csv2(coloc, "coloc.csv")
