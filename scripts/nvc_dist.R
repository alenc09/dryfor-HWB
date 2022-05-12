# Thu May 12 11:36:34 2022 ------------------------------
#Script para medir relação entre distancia da sede e variação na cobertura florestal das paisagens

#library----
library(here)
library(dplyr)
library(ggplot2)

#data----
read.csv(file = here("tabela_geral.csv"))-> tab_geral

#análises exploratórias----
tab_geral %>% 
  glimpse %>% 
  ggplot() +
  geom_point(aes(x = dist_near_sede/1000, y = vari_perc_nvc))+
  geom_hline(yintercept = 0) +
  xlab("Distance to nearest seat (Km)")+
  ylab("Variation in forest cover (%)")+
  theme_classic() -> fig.nvc_dist

ggsave(plot = fig.nvc_dist, filename = here("fig.nvc_dist.jpg"))
