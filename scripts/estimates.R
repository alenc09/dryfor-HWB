# Tue May 17 09:44:32 2022 ------------------------------
#Script para calcular estimativas (area, pessoas, floresta) em paisagens com mais de 20% de cobertura florestal

#library----
library(here)
library(dplyr)

#data----
read.csv(file = here("tabela_geral.csv"))-> tab_geral

#cáculos----
##n de paisagens
tab_geral %>% 
  filter(pland_nvc_06 >= 20) %>% 
  summarise(n_paisagens = n_distinct(buff_id)) %>% 
  glimpse

##number of FPP----
tab_geral %>% 
  filter(pland_nvc_06 >= 20) %>% 
  summarise(fpp_06 = sum(pop_rural_WP_06),
            fpp_17 = sum(pop_rural_WP_17)) %>% 
  glimpse ->fpp_buffs

((fpp_buffs$fpp_17*92.81447)/49.41354) - ((fpp_buffs$fpp_06*92.81447)/49.41354)
#(fpp_buffs$fpp_17 - fpp in 2017
#92.81447 - expected percentage of total area in caatinga with at least 20% of forest cover
#49.41354 - percentage of area in caatinga known to have at least 20% of forest cover
#fpp_buffs$fpp_06 - fpp in 2006

##area----
(((pi*5^2)*100)*6193*100)/sum(unique(tab_geral$mun_area_ha[-3867]))
#((pi*5^2)*100) - Area of one buff in hectares
# 6193 - total number of buffers
# 100 - 100% of caatinga area
# sum(unique(tab_geral$mun_area_ha[-3867])) - Sum of area of caatinga municipalities

##forest----
(sum(tab_geral$ca_nvc_17)*100)/(50828383+4159234)
#(sum(tab_geral$ca_nvc_17)*100) - Área em hectares de vegetação nativa dentro dos buffers
#(50828383+4159234) - total de área de vegetação nativa na Caatinga de acordo com o Mapbiomas (2017)

##change in forest and people by category of change----
tab_geral %>% 
  filter(pland_nvc_06 >= 20) %>% 
  group_by(cat_change) %>% 
  summarise(n = n(),
            mean_perc_change_fpp = mean(vari_perc_pop_rural, na.rm = T),
            mean_perc_change_nvc = mean(vari_perc_nvc, na.rm=T)) %>% 
  glimpse
