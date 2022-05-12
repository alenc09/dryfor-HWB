# Thu May 12 11:21:17 2022 ------------------------------
#script para medir a relação entre distância do centro urbano e variação na FPP

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
  geom_point(aes(x = dist_near_sede/1000, y = vari_perc_pop_rural))+
  ylim(-100, 200)+
  geom_hline(yintercept = 0) +
  xlab("Distance to nearest seat (Km)")+
  ylab("Variation in FPP (%)")+
  theme_classic() -> fig.fpp_dist

ggsave(plot = fig.fpp_dist, filename = here("fig.fpp_dist.jpg"))
