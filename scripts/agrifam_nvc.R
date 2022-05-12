# Wed May 11 10:04:59 2022 ------------------------------
#script para análise de variação de vegetação nativa e agricultura familiar

#library----
library(readxl)
library(here)
library(geobr)
library(sf)
library(units)
library(ggplot2)
library(cowplot)

#Data----
##agricultura familiar----
read_xlsx(here("data/tabela2006.xlsx")) -> table_agrifamiliar_2006
read_xlsx(here("data/tabela2006.xlsx"), sheet = 2) -> table_agrifamiliar_area_2006
read_xlsx(here("data/tabela2017.xlsx")) -> table_agrifamiliar_2017
read_xlsx(here("data/tabela2017.xlsx"), sheet = 2) -> table_agrifamiliar_area_2017

##área dos municípios da caatinga----
read_municipality(simplified = F)-> mun_br
read_biomes() %>% 
  filter (name_biome == "Caatinga") -> caat_shp

sf_use_s2(FALSE) #função para desativar a checagem de vértices duplicados
st_transform(x = mun_br, crs =5880) ->mun_br
st_transform(x = caat_shp, crs = 5880) -> caat_shp
mun_br[caat_shp,] -> mun_caat
st_area(mun_caat)-> mun_caat$mun_area_ha
set_units(x = mun_caat$mun_area_ha, ha) -> mun_caat$mun_area_ha

###área e número de agricultura familiar----
tab_1 %>%
  left_join(y = dplyr::select(mun_caat, code_muni, mun_area_ha)) %>% 
  #rename(geom_mun = geom) %>% 
  mutate(area_estab_06 = set_units(x = area_estab_06, ha),
         area_estab_17 = set_units(x = area_estab_17, ha),
         perc_area_agrifam_06 = (agrifam_area_06/mun_area_ha)*100,
         perc_area_agrifam_17 = (agrifam_area_17/mun_area_ha)*100,
         vari_perc_area_agrifam = perc_area_agrifam_17 - perc_area_agrifam_06) %>% 
  glimpse -> tab_1

tab_1 %>% 
  dplyr::select(-geom) %>% 
  write.csv(x = ., file = here("tabela_geral.csv"))

#Figuras----
## exploratórias----
tab_1 %>% 
  group_by(code_muni) %>% 
  summarise(mean_vari_perc_nvc = mean(vari_perc_nvc),
            vari_perc_area_agrifam = vari_perc_area_agrifam) %>% 
   distinct() %>%
ggplot(aes(x = mean_vari_perc_nvc, y = drop_units(vari_perc_area_agrifam))) +
  geom_point(alpha = 0.2) +
  xlab("Mean variation in native vegetation (%)")+
  ylab("Variation in area of family agriculture (%)")+
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  ylim(-50, 50)+
  theme_classic() -> fig.agrifam.area_nvc

tab_1 %>% 
  group_by(code_muni) %>% 
  summarise(mean_vari_perc_nvc = mean(vari_perc_nvc),
            vari_perc_estab_agrifam = vari_perc_estab_agrifam) %>% 
  distinct() %>%
  ggplot(aes(x = mean_vari_perc_nvc, y = vari_perc_estab_agrifam)) +
  geom_point(alpha = 0.2) +
  xlab("Mean variation in native vegetation (%)")+
  ylab("Variation in number of family agriculture (%)")+
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  theme_classic() ->fig.agrifam.numb_nvc

plot_grid(fig.agrifam.area_nvc, fig.agrifam.numb_nvc) -> fig.agrifam_nvc

ggsave(plot = fig.agrifam_nvc, filename = here("img/fig.agrifam_nvc.jpg"))
