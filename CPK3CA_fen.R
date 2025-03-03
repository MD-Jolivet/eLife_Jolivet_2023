#SCI calculus
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

#Import csv files to R
fen_220303 <- read_delim("input_data/CPK3CA_fen_220303.csv", ";", 
                          escape_double = FALSE, trim_ws = TRUE)
fen_220414 <- read_delim("input_data/CPK3CA_fen_220414.csv", ";", 
                         escape_double = FALSE, trim_ws = TRUE)
fen_220724 <- read_delim("input_data/CPK3CA_fen_220724.csv", ";", 
                         escape_double = FALSE, trim_ws = TRUE)
fen_220812 <- read_delim("input_data/CPK3CA_fen_220812.csv", ";", 
                         escape_double = FALSE, trim_ws = TRUE)

fen <- bind_rows(fen_220303, fen_220414, fen_220812)

fen <- fen %>%
  mutate(logSCI = log(SCI))

#SCI smaller in fen treated plants
ggboxplot(fen_220303, x = "Condition", y = "SCI", add = "mean_se", color = "black", fill = "white", size = 1) +
  stat_compare_means(label = "p.signif", method = "t.test", ref.group = "DMSO", hide.ns = TRUE, label.y = 130, size = 10) +
  geom_jitter(color = "grey", size = 1)+
  theme(axis.text = element_text(size=20))

#SCI smaller in fen treated plants
ggboxplot(fen_220414, x = "Condition", y = "SCI", add = "mean_se", color = "black", fill = "white", size = 1) +
  stat_compare_means(label = "p.signif", method = "t.test", ref.group = "DMSO", hide.ns = TRUE, label.y = 130, size = 10) +
  geom_jitter(color = "grey", size = 1)+
  theme(axis.text = element_text(size=20))

#no significant difference
ggboxplot(fen_220724, x = "Condition", y = "SCI", add = "mean_se", color = "black", fill = "white", size = 1) +
  stat_compare_means(label = "p.signif", method = "t.test", ref.group = "DMSO", hide.ns = TRUE, label.y = 130, size = 10) +
  geom_jitter(color = "grey", size = 1)+
  theme(axis.text = element_text(size=20))

#SCI smaller in fen treated plants
ggboxplot(fen_220812, x = "Condition", y = "SCI", add = "mean_se", color = "black", fill = "white", size = 1) +
  stat_compare_means(label = "p.signif", method = "t.test", ref.group = "DMSO", hide.ns = TRUE, label.y = 130, size = 10) +
  geom_jitter(color = "grey", size = 1)+
  theme(axis.text = element_text(size=20))

ggboxplot(fen, x = "Condition", y = "SCI", add = "mean_se", color = "black", fill = "white", size = 0.5) +
  stat_compare_means(label = "p.signif", method = "t.test", ref.group = "DMSO", hide.ns = TRUE, label.y = 70, size = 10) +
  geom_jitter(color = "grey", size = 0.1)+
  theme(axis.text = element_text(size=20))

ggdensity(fen, y="..density..", x="logSCI", color ="Condition")


#Export dataset containing
write.csv(fen, file ="output_data/fen.csv")