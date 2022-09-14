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
read_state() -> br_states
read_country() -> br

sf_use_s2(FALSE) #função para desativar a checagem de vértices duplicados
st_transform(x = mun_br, crs =5880) ->mun_br
st_transform(x = caat_shp, crs = 5880) -> caat_shp
st_transform(x = br_states, crs = 5880) -> br_states
mun_br[caat_shp,] -> mun_caat
br_states[caat_shp,] -> states_caat


#map----
tab_geral %>%
   group_by(code_muni) %>%
  summarise(mean_fpp_change = mean(vari_perc_pop_rural)) %>%
  right_join(y = mun_caat, by = "code_muni") %>%
  glimpse -> tab_map

st_as_sf(tab_map) %>% 
  st_transform(crs=4674) ->tab_map

tab_map %>% 
ggplot() +
  geom_sf(aes(geometry = geom, fill = mean_fpp_change), lwd = 0) +
  scale_fill_fermenter(
    palette = "BrBG",
    direction = 1,
    limits = c(-100, 100),
    breaks = c(-75, -50, -25, -12.5, 0, 12.5, 25, 50, 75),
    name = "FPP change (%)",
    label = c("-75","-50", "-25", "-12.5", "0","12.5","25", "50", "75"))+
  geom_sf(data = states_caat[-1,], fill="transparent", lwd=0.2)+
  coord_sf(xlim = c(-48, -34), ylim = c(-17.1, -3))+
  geom_text(data = states_caat[-1,], aes(x= c(-42, -39.5,-36.5,-35.5, -34.5, -34.4, -36, -39,-42.4),
                                y = c(-16.8, -15, -11, -10, -8.5, -7, -4.7, -2.9, -5),
                                label = c("MG", "BA", "SE", "AL", "PE", "PB", "RN", "CE", "PI")),
            size = 2)+
  theme_map()+
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 10))-> map_fpp_change

#Inset map####
ggplot()+
  geom_sf(data = br, fill = "transparent")+
  geom_sf(data = caat_shp, fill = "grey")+
  geom_sf(data = br_states, fill = "transparent", lwd = 0.1)+
  theme_map() -> inset_map
  
ggdraw()+
  draw_plot(map_fpp_change)+
  draw_plot(inset_map,
            x = -0.03, y = 0.6, width = 0.40, height = 0.40) -> map_fpp_change_inset

ggsave(plot = map_fpp_change_inset, filename = here("img/map.fpp_change.png"), dpi = 600)
