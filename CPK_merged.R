#####Local and systemic PlaMV propagation
##Libraries
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
library(rstatix)

##Import datasets
PlaMV_systemie_AFD1 <- read_delim("input data/PlaMV_systemie_AFD1.csv", 
                                  ";", escape_double = FALSE, trim_ws = TRUE)
PlaMV_systemie_AFD2 <- read_delim("input data/PlaMV_systemie_AFD2.csv", 
                                  ";", escape_double = FALSE, trim_ws = TRUE)
PlaMV_systemie_Toba1 <- read_delim("input data/PlaMV_systemie_Toba1.csv", 
                                   ";", escape_double = FALSE, trim_ws = TRUE)
PlaMV_systemie_Toba2 <- read_delim("input data/PlaMV_systemie_Toba2.csv", 
                                   ";", escape_double = FALSE, trim_ws = TRUE)

PlaMV_systemie_AFD <- bind_rows(PlaMV_systemie_AFD1, PlaMV_systemie_AFD2)
PlaMV_systemie_Toba <- bind_rows(PlaMV_systemie_Toba1, PlaMV_systemie_Toba2)

PlaMV_systemie_all <-  bind_rows(PlaMV_systemie_Toba1, PlaMV_systemie_AFD2)

PlaMV_systemie_cpk <- PlaMV_systemie_all %>%
  filter(!genotype %in% c("atrem 3KO", "AtCPK3 OE#8.2"))

PlaMV_systemie_cpk <- PlaMV_systemie_cpk %>%
  filter(!dpi %in% c("7", "9", "12", "19", "21"))

PlaMV_systemie_REM <- PlaMV_systemie_AFD2 %>%
  filter(!genotype %in% c("AtCPK3 OE#8.2", "cpk3-2", "AtCPK3 OE#16.2")) %>%
  filter(!dpi %in% c("9"))

#stat test
stat.test <- PlaMV_systemie_AFD2 %>%
  group_by(dpi) %>%
  wilcox_test(intden ~ genotype, ref.group = "Col-0") %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test <- stat.test %>%
  add_xy_position(x = "dpi", dodge = 0.8)

stat.test2 <- PlaMV_systemie_Toba1 %>%
  group_by(dpi) %>%
  wilcox_test(intden ~ genotype, ref.group = "Col-0") %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test2 <- stat.test2 %>%
  add_xy_position(x = "dpi", dodge = 0.8)

stat.test3 <- PlaMV_systemie_cpk %>%
  group_by(dpi) %>%
  wilcox_test(intden ~ genotype, ref.group = "Col-0") %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test3 <- stat.test3 %>%
  add_xy_position(x = "dpi", dodge = 0.8)

stat.test4 <- PlaMV_systemie_REM %>%
  group_by(dpi) %>%
  wilcox_test(intden ~ genotype, ref.group = "Col-0") %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test4 <- stat.test4 %>%
  add_xy_position(x = "dpi", dodge = 0.8)

##Display stats results on the graph (boxplot)
ggboxplot(PlaMV_systemie_AFD1, x = "dpi", y = "intden", fill = "genotype", palette = get_palette("Paired", 5)) +
  stat_pvalue_manual(stat.test,  label = "p.adj.signif", tip.length = 0, hide.ns = TRUE) +
  stat_summary(fun=mean, geom="line", aes(group=genotype, color = genotype))

ggboxplot(PlaMV_systemie_AFD2, x = "dpi", y = "intden", fill = "genotype", palette = get_palette("Paired", 5)) +
  stat_pvalue_manual(stat.test,  label = "p.adj.signif", tip.length = 0, hide.ns = TRUE) +
  stat_summary(fun=mean, geom="line", aes(group=genotype, color = genotype))

ggboxplot(PlaMV_systemie_Toba1, x = "dpi", y = "intden", fill = "genotype", palette = get_palette("Paired", 2)) +
  stat_pvalue_manual(stat.test2,  label = "p.adj.signif", tip.length = 0, hide.ns = TRUE) +
  stat_summary(fun=mean, geom="line", aes(group=genotype, color = genotype))

ggboxplot(PlaMV_systemie_Toba2, x = "dpi", y = "intden", fill = "genotype", palette = get_palette("Paired", 2)) +
  stat_pvalue_manual(stat.test2,  label = "p.adj.signif", tip.length = 0, hide.ns = TRUE) +
  stat_summary(fun=mean, geom="line", aes(group=genotype, color = genotype))


PlaMV_systemie_cpk$genotype <- ordered(PlaMV_systemie_cpk$genotype, levels =c("Col-0", "cpk3-2", "cpk3ko_2", "AtCPK3 OE#16.2"))
ggboxplot(PlaMV_systemie_cpk, x = "dpi", y = "intden", fill = "genotype", palette = get_palette("Pastel1", 3)) +
  stat_pvalue_manual(stat.test3,  label = "p.adj.signif", tip.length = 0, hide.ns = TRUE) +
  stat_summary(fun=mean, geom="line", aes(group=genotype, color = genotype))


PlaMV_systemie_REM$genotype <- ordered(PlaMV_systemie_REM$genotype, levels =c("Col-0", "atrem 3KO"))
ggboxplot(PlaMV_systemie_REM, x = "dpi", y = "intden", fill = "genotype", palette = get_palette("Pastel1", 3)) +
  stat_pvalue_manual(stat.test4,  label = "p.adj.signif", tip.length = 0, hide.ns = TRUE) +
  stat_summary(fun=mean, geom="line", aes(group=genotype, color = genotype))