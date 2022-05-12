# Thu May 12 15:21:40 2022 ------------------------------
#Script para montar mapa da mudança de FPP na caatinga

#library----
library(here)
library(geobr)
library(dplyr)
library(sf)
library(ggplot2)
library(cowplot)

#data----
read.csv(file = here("tabela_geral.csv"))-> tab_geral
read_municipality(simplified = F)-> mun_br
read_biomes() %>% 
  filter (name_biome == "Caatinga") -> caat_shp

sf_use_s2(FALSE) #função para desativar a checagem de vértices duplicados
st_transform(x = mun_br, crs =5880) ->mun_br
st_transform(x = caat_shp, crs = 5880) -> caat_shp
mun_br[caat_shp,] -> mun_caat

#map----
tab_geral %>%
  group_by(code_muni) %>%
  summarise(mean_fpp_change = mean(vari_perc_pop_rural)) %>%
  right_join(y = mun_caat, by = "code_muni") %>%
  glimpse %>% 
ggplot() +
  geom_sf(aes(geometry = geom, fill = mean_fpp_change)) +
  scale_fill_fermenter(
    limits = c(-100, 100),
    type = "div",
    palette = "RdYlGn",
    direction = 1,
    breaks = c(-100, -75, -50, -25, 0, 25, 50, 75, 100),
    name = "FPP change"
  )+
  theme_map() -> map_fpp_change

ggsave(plot = map_fpp_change, filename = here("img/map.fpp_change.jpg"))
