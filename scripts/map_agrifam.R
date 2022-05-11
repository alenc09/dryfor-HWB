# Wed May 11 15:54:07 2022 ------------------------------
#script para construir mapa da variação na área e número de agricultura familiar

#libraries----
library(geobr)
library(dplyr)
library(ggplot2)

#data----
read.csv(file = here("tabela_geral.csv"))-> tab_1
read_municipality(simplified = F)-> mun_br
read_biomes() %>% 
  filter (name_biome == "Caatinga") -> caat_shp

sf_use_s2(FALSE) #função para desativar a checagem de vértices duplicados
st_transform(x = mun_br, crs =5880) ->mun_br
st_transform(x = caat_shp, crs = 5880) -> caat_shp
mun_br[caat_shp,] -> mun_caat

#map----
tab_1 %>%
  right_join(y = dplyr::select(mun_caat, code_muni)) -> tab_map 


ggplot() +
  geom_sf(data = tab_map, aes(geometry = geom, fill = vari_perc_area_agrifam)) +
  scale_fill_fermenter(
    breaks = c(-20,-10, 0, 10, 20),
    type = "div",
    palette = "RdYlGn",
    direction = 1,
    name = "Varition in area of\nfamily agriculture (%)"
  ) +
  theme_map() -> map.agrifam_area

ggplot() +
  geom_sf(data = tab_map, aes(geometry = geom, fill = vari_perc_estab_agrifam)) +
  scale_fill_fermenter(
    breaks = c(-20,-10, 0, 10, 20),
    type = "div",
    palette = "RdYlGn",
    direction = 1,
    name = "Varition in number of\nfamily agriculture (%)"
  ) +
  theme_map() -> map.agrifam_number

ggarrange(map.agrifam_area, map.agrifam_number, nrow = 2) %>%
  ggsave(plot = ., filename = here("img/map_agrifam.jpg"))
  