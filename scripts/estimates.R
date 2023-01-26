# Tue May 17 09:44:32 2022 ------------------------------
#Script para calcular estimativas (area, pessoas, floresta) em paisagens com mais de 20% de cobertura florestal

#library----
library(here)
library(dplyr)
library(streamgraph)
library(ggplot2)
library(tidyr)

#data----
read.csv(file = here("data/tabela_geral.csv"))-> tab_geral

#cáculos----
##n de paisagens
tab_geral %>% 
  rename(buff_id = X) %>% 
  filter(pland_nvc_06 >= 20) %>% 
  summarise(n_paisagens = n_distinct(buff_id)) %>% 
  glimpse

##number of FPP----
tab_geral %>% 
  filter(pland_nvc_06 >= 10) %>% 
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

##Percentage of landscapes in each category----
tab_geral %>% 
  filter(pland_nvc_06 >= 20, 
         vari_perc_pop_rural > 0) %>% 
  glimpse

##changes per state----
tab_geral %>% 
  rename("buffer" = "X") %>% 
  mutate(pland_nvc_17 = pland_nvc_06 + vari_perc_nvc,
         buffer = as.factor(buffer)) %>%
  select(buffer, code_uf, pland_nvc_17, pop_rural_WP_17) -> tab_fpp_state

# ###Cumulative from 10 to 100----
# list()-> list_fpp_state
# for(i in seq(0,90,10)){
#   tab_fpp_state %>% 
#     filter(pland_nvc_17 <i+10) %>% 
#     group_by(code_uf) %>% 
#     summarise(n_buff = n(),
#               mean_fpp = mean(pop_rural_WP_17),
#               sd_fpp = sd(pop_rural_WP_17)) %>% 
#     glimpse -> a
#   a -> list_fpp_state[[i+10]]
# }
# 
# # for (i in seq(10,100,10)) {
# #   list_fpp_state[[i]] -> get(paste0("fpp_state_", i))
# # }
# 
# list_fpp_state[[10]]-> fpp_state_10
# list_fpp_state[[20]]-> fpp_state_20
# list_fpp_state[[30]]-> fpp_state_30
# list_fpp_state[[40]]-> fpp_state_40
# list_fpp_state[[40]]-> fpp_state_40
# list_fpp_state[[50]]-> fpp_state_50
# list_fpp_state[[60]]-> fpp_state_60
# list_fpp_state[[70]]-> fpp_state_70
# list_fpp_state[[80]]-> fpp_state_80
# list_fpp_state[[90]]-> fpp_state_90
# list_fpp_state[[100]]-> fpp_state_100
# 
# fpp_state_10 %>% 
#   select(-n_buff) %>% 
#   right_join(select(fpp_state_20, -n_buff), by = "code_uf") %>% 
#   rename(mean_fpp_10 = mean_fpp.x,
#          mean_fpp_20 = mean_fpp.y,
#          sd_fpp_10 = sd_fpp.x,
#          sd_fpp_20 = sd_fpp.y) %>% 
#   right_join(select(fpp_state_30, -n_buff), by = "code_uf") %>%
#   rename(mean_fpp_30 = mean_fpp,
#          sd_fpp_30 = sd_fpp) %>%
#   right_join(select(fpp_state_40, -n_buff), by = "code_uf") %>%
#   rename(mean_fpp_40 = mean_fpp,
#          sd_fpp_40 = sd_fpp) %>%
#   right_join(select(fpp_state_50, -n_buff), by = "code_uf") %>%
#   rename(mean_fpp_50 = mean_fpp,
#          sd_fpp_50 = sd_fpp) %>%
#   right_join(select(fpp_state_60, -n_buff), by = "code_uf") %>%
#   rename(mean_fpp_60 = mean_fpp,
#          sd_fpp_60 = sd_fpp) %>%
#   right_join(select(fpp_state_70, -n_buff), by = "code_uf") %>%
#   rename(mean_fpp_70 = mean_fpp,
#          sd_fpp_70 = sd_fpp) %>%
#   right_join(select(fpp_state_80, -n_buff), by = "code_uf") %>%
#   rename(mean_fpp_80 = mean_fpp,
#          sd_fpp_80 = sd_fpp) %>%
#   right_join(select(fpp_state_90, -n_buff), by = "code_uf") %>%
#   rename(mean_fpp_90 = mean_fpp,
#          sd_fpp_90 = sd_fpp) %>%
#   right_join(select(fpp_state_100, -n_buff), by = "code_uf") %>%
#   rename(mean_fpp_100 = mean_fpp,
#          sd_fpp_100 = sd_fpp) %>%
#   glimpse -> fpp_state_all
# 
# ####Figure----
# fpp_state_all [-10,]%>% 
#   replace(is.na(.), 0) %>%
#   pivot_longer(cols = 2:21, 
#                names_pattern = "(.)_(.)_",
#                names_to = c(".value", "nvc_thresh")) %>%
#   mutate(nvc_thresh = as.numeric(nvc_thresh),
#          code_uf = as.factor(code_uf))%>%
#   glimpse #-> fpp_state_all_long
# 
# fpp_state_all_long %>%
# ggplot()+
#   geom_line(aes(x = nvc_thresh, y = mean_fpp, color = code_uf))
# 
# 
# ###from 100 to 10----
# list()-> list_fpp_state
# for(i in seq(0,90,10)){
#   tab_fpp_state %>% 
#     filter(pland_nvc_17 >i+10) %>% 
#     group_by(code_uf) %>% 
#     summarise(n_buff = n(),
#               mean_fpp = mean(pop_rural_WP_17),
#               sd_fpp = sd(pop_rural_WP_17)) %>% 
#     glimpse -> a
#   a -> list_fpp_state[[i+10]]
# }
# 
# # for (i in seq(10,100,10)) {
# #   list_fpp_state[[i]] -> get(paste0("fpp_state_", i))
# # }
# 
# list_fpp_state[[10]]-> fpp_state_10
# list_fpp_state[[20]]-> fpp_state_20
# list_fpp_state[[30]]-> fpp_state_30
# list_fpp_state[[40]]-> fpp_state_40
# list_fpp_state[[40]]-> fpp_state_40
# list_fpp_state[[50]]-> fpp_state_50
# list_fpp_state[[60]]-> fpp_state_60
# list_fpp_state[[70]]-> fpp_state_70
# list_fpp_state[[80]]-> fpp_state_80
# list_fpp_state[[90]]-> fpp_state_90
# list_fpp_state[[100]]-> fpp_state_100
# 
# fpp_state_10 %>% 
#   select(-n_buff) %>% 
#   right_join(select(fpp_state_20, -n_buff), by = "code_uf") %>% 
#   rename(mean_fpp_10 = mean_fpp.x,
#          mean_fpp_20 = mean_fpp.y,
#          sd_fpp_10 = sd_fpp.x,
#          sd_fpp_20 = sd_fpp.y) %>% 
#   right_join(select(fpp_state_30, -n_buff), by = "code_uf") %>%
#   rename(mean_fpp_30 = mean_fpp,
#          sd_fpp_30 = sd_fpp) %>%
#   # right_join(select(fpp_state_40, code_uf, mean_fpp), by = "code_uf") %>% 
#   # rename(mean_fpp_40 = mean_fpp) %>% 
#   # right_join(select(fpp_state_50, code_uf, mean_fpp), by = "code_uf") %>% 
#   # rename(mean_fpp_50 = mean_fpp) %>% 
#   # right_join(select(fpp_state_60, code_uf, mean_fpp), by = "code_uf") %>% 
#   # rename(mean_fpp_60 = mean_fpp) %>% 
#   # right_join(select(fpp_state_70, code_uf, mean_fpp), by = "code_uf") %>% 
#   # rename(mean_fpp_70 = mean_fpp) %>% 
#   # right_join(select(fpp_state_80, code_uf, mean_fpp), by = "code_uf") %>% 
#   # rename(mean_fpp_80 = mean_fpp) %>% 
#   # right_join(select(fpp_state_90, code_uf, mean_fpp), by = "code_uf") %>% 
# # rename(mean_fpp_90 = mean_fpp) %>% 
# # right_join(select(fpp_state_100, code_uf, mean_fpp), by = "code_uf") %>% 
# # rename(mean_fpp_100 = mean_fpp) %>% 
# glimpse #-> fpp_state_all
# 
# ####Figure----
# cols <- c("#FFB400", "#FFC740", "#C20008", "#FF020D", "#13AFEF", "black", "purple", "green", "darkred")
# 
# fpp_state_all [-10,]%>% 
#   replace(is.na(.), 0) %>% 
#   pivot_longer(cols = 2:11, names_prefix = "mean_fpp_", names_to = "nvc_thresh", values_to = "mean_fpp") %>%
#   mutate(nvc_thresh = as.numeric(nvc_thresh),
#          code_uf = as.factor(code_uf))%>%
#   glimpse -> fpp_state_all_long
# 
# fpp_state_all_long %>%
#   ggplot()+
#   geom_line(aes(x = nvc_thresh, y = mean_fpp, color = code_uf))

###cumulative population density----
list()-> list_fpp_dens
for(i in seq(0,90,10)){
  tab_fpp_state %>% 
    filter (pland_nvc_17 >= i+10) %>% 
  group_by(code_uf) %>% 
  summarise(n_buff = n(),
            area_buffs = n_buff * 78.539816,
            sum_fpp = sum(pop_rural_WP_17),
            fpp_dens = sum_fpp/area_buffs) %>% 
  glimpse -> list_fpp_dens [[i+10]]
}

list_fpp_dens[[10]]-> fpp_dens_10
list_fpp_dens[[20]]-> fpp_dens_20
list_fpp_dens[[30]]-> fpp_dens_30
list_fpp_dens[[40]]-> fpp_dens_40
list_fpp_dens[[40]]-> fpp_dens_40
list_fpp_dens[[50]]-> fpp_dens_50
list_fpp_dens[[60]]-> fpp_dens_60
list_fpp_dens[[70]]-> fpp_dens_70
list_fpp_dens[[80]]-> fpp_dens_80
list_fpp_dens[[90]]-> fpp_dens_90
list_fpp_dens[[100]]-> fpp_dens_100

fpp_dens_10 %>% 
  left_join(select(fpp_dens_20, code_uf, fpp_dens), by = "code_uf") %>% 
  rename(fpp_dens_10 = fpp_dens.x, fpp_dens_20 = fpp_dens.y) %>% 
  left_join(select(fpp_dens_30, code_uf, fpp_dens), by = "code_uf") %>% 
  rename(fpp_dens_30 = fpp_dens) %>% 
  left_join(select(fpp_dens_40, code_uf, fpp_dens), by = "code_uf") %>% 
  rename(fpp_dens_40 = fpp_dens) %>% 
  left_join(select(fpp_dens_50, code_uf, fpp_dens), by = "code_uf") %>% 
  rename(fpp_dens_50 = fpp_dens) %>% 
  left_join(select(fpp_dens_60, code_uf, fpp_dens), by = "code_uf") %>% 
  rename(fpp_dens_60 = fpp_dens) %>% 
  left_join(select(fpp_dens_70, code_uf, fpp_dens), by = "code_uf") %>% 
  rename(fpp_dens_70 = fpp_dens) %>%
  left_join(select(fpp_dens_80, code_uf, fpp_dens), by = "code_uf") %>% 
  rename(fpp_dens_80 = fpp_dens) %>% 
  left_join(select(fpp_dens_90, code_uf, fpp_dens), by = "code_uf") %>% 
  rename(fpp_dens_90 = fpp_dens) %>% 
  left_join(select(fpp_dens_100, code_uf, fpp_dens), by = "code_uf") %>% 
  rename(fpp_dens_100 = fpp_dens) %>% 
  glimpse -> fpp_dens_all

fpp_dens_all[-10,]%>% 
  replace(is.na(.), 0) %>%
  pivot_longer(cols = 5:14, names_prefix = "fpp_dens_", names_to = "nvc_thresh", values_to = "fpp_dens") %>%
  mutate(nvc_thresh = as.numeric(nvc_thresh),
         code_uf = as.factor(code_uf))%>%
  glimpse -> fpp_dens_all_long

fpp_dens_all_long %>%
  ggplot()+
  geom_line(aes(x = nvc_thresh, y = fpp_dens, color = code_uf))+
  labs(x = "Forest cover threshold (%)", y = "FPP density (n/km²)")+
  scale_color_discrete(labels = c("Piauí", "Ceará", "Rio Grande do Norte",
                                  "Paraíba", "Pernambuco", "Alagoas", "Sergipe",
                                  "Bahia", "Minas Gerais"), name = "Estado")+
  theme_classic()
