# Thu May 12 10:24:48 2022 ------------------------------
# Script para avaliar a relação entre mudança populacional e diversidade da paisagem

#library----
library(here)
library(dplyr)
library(ggplot2)

#Data----
read.csv(file = here("tabela_geral.csv"))-> tab_geral

#análises exploratórias----
tab_geral %>% 
  mutate(vari_shdi = shdi_17 - shdi_06) %>% 
  glimpse %>% 
ggplot() +
  geom_point(aes(x = vari_perc_pop_rural, y = vari_shdi), alpha = 0.1)+
  geom_vline(xintercept = 0) + 
  geom_hline(yintercept = 0) +
  xlim(-100, 200)+
  xlab("Variation in FPP (%)") + 
  ylab("variation in landscape diversity") +
  theme(panel.background = element_blank(),
        axis.line.x.bottom = element_line(colour = "black"),
        axis.line.y.left = element_line(colour = "black")) -> fig.shdi_fpp 

ggsave(plot = fig.shdi_fpp , filename = here("fig.shdi_fpp.jpg"))
