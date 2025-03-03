##Comparaison of the MSD all transient conditions
#Required libraries
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
library(gaussian)

#Import MSD data
CA <- read_csv("output_data/MSD_CPK3CA_diff_231602.csv")
FL <- read_csv("output_data/MSD_CPK3FL_diff_231602.csv")
FL_PlaMV <- read_csv("output_data/MSD_CPK3FL_PlaMV_diff_231602.csv")

#CA: filtrate date and Streams
CA_211007 <- CA %>%
  filter(date == "21_10_07")
CA_211021 <- CA %>%
  filter(date == "21_10_25")
CA_211110 <- CA %>%
  filter(date == "21_11_10")

CA_211007_slow <- CA_211007 %>% 
  group_by (Stream) %>%
  count(diff) %>%
  mutate(freq = n/sum(n), date="21_10_07")

CA_211021_slow <- CA_211021 %>% 
  group_by (Stream) %>%
  count(diff) %>%
  mutate(freq = n/sum(n), date="21_10_21")

CA_211110_slow <- CA_211110 %>% 
  group_by (Stream) %>%
  count(diff) %>%
  mutate(freq = n/sum(n), date="21_11_10")

CA_slow <- bind_rows(CA_211007_slow, CA_211021_slow, CA_211110_slow)
CA_slow <- CA_slow %>%
  mutate(Condition = "CA")

#FL: filtrate date and Streams
FL_210730 <- FL %>%
  filter(date == "21_07_30")
FL_211021 <- FL %>%
  filter(date == "21_10_21")
FL_211110 <- FL %>%
  filter(date == "21_11_10")
FL_211118 <- FL %>%
  filter(date == "21_11_18")

FL_210730_slow <- FL_210730 %>% 
  group_by (Stream) %>%
  count(diff) %>%
  mutate(freq = n/sum(n), date="21_07_30")

FL_211021_slow <- FL_211021 %>% 
  group_by (Stream) %>%
  count(diff) %>%
  mutate(freq = n/sum(n), date="21_10_21")

FL_211110_slow <- FL_211110 %>% 
  group_by (Stream) %>%
  count(diff) %>%
  mutate(freq = n/sum(n), date="21_11_10")

FL_211118_slow <- FL_211118 %>% 
  group_by (Stream) %>%
  count(diff) %>%
  mutate(freq = n/sum(n), date="21_11_18")

FL_slow <- bind_rows(FL_210730_slow, FL_211021_slow, FL_211110_slow, FL_211118_slow)
FL_slow <- FL_slow %>%
  mutate(Condition = "FL")


#FL_PlAMV: filtrate date and Streams
FL_PlaMV_211021 <- FL_PlaMV %>%
  filter(date == "21_10_21")
FL_PlaMV_211029 <- FL_PlaMV %>%
  filter(date == "21_10_29")
FL_PlaMV_211112 <- FL_PlaMV %>%
  filter(date == "21_11_12")

FL_PlaMV_211021_slow <- FL_PlaMV_211021 %>% 
  group_by (Stream) %>%
  count(diff) %>%
  mutate(freq = n/sum(n), date="21_10_21")

FL_PlaMV_211029_slow <- FL_PlaMV_211029 %>% 
  group_by (Stream) %>%
  count(diff) %>%
  mutate(freq = n/sum(n), date="21_10_29")

FL_PlaMV_211112_slow <- FL_PlaMV_211112 %>% 
  group_by (Stream) %>%
  count(diff) %>%
  mutate(freq = n/sum(n), date="21_11_12")

FL_PlaMV_slow <- bind_rows(FL_PlaMV_211021_slow, FL_PlaMV_211029_slow, FL_PlaMV_211112_slow)
FL_PlaMV_slow <- FL_PlaMV_slow %>%
  mutate(Condition = "FL_PlAMV")

#bind all together
all_slow <- bind_rows(FL_slow, FL_PlaMV_slow, CA_slow)
all_slow <- all_slow %>%
  filter(diff=="slow") %>%
  mutate(freq =freq*100)

#Plot data
ggboxplot(all_slow, x = "Condition", y = "freq", font.tickslab = c(14), ylab = "fraction of immobile protein (%)", font.y = c(14), add = "mean_se", size = 0.25) +
  stat_compare_means(label = "p.signif", method = "t.test", ref.group = "FL", hide.ns = TRUE, label.y = 55, size = 8) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 70)) + 
  rotate_x_text(angle = 40) +
  geom_jitter(size = 1, color = "grey") +
  theme(
    axis.ticks = element_line(size = 0.5),
    axis.line = element_line(size = 0.5))

#export dataset
write.csv(all_slow, here::here("output_data", "CPK3_transient_immobile_proteins.csv"), row.names=FALSE)