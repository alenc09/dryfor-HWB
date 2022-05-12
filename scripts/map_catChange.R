# Thu May 12 16:02:31 2022 ------------------------------
#Script para montar mapa da mudança de categoria(forest-people) por município

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

#mapa----
tab_geral %>%
  group_by(code_muni) %>%
  summarise(
    mean_pop_change = mean(vari_perc_pop_rural),
    mean_nvc_change = mean(vari_perc_nvc),
    cat_change = cat_change) %>% 
  mutate(
    cat_change = if_else(
      condition = mean_nvc_change > 0 & mean_pop_change > 0,
      true = "GG",
      false = if_else(
        condition = mean_nvc_change > 0 & mean_pop_change < 0,
        true = "GP",
        false = if_else(
          condition =  mean_nvc_change < 0 & mean_pop_change > 0,
          true = "PG",
          false = if_else(
            mean_nvc_change < 0 & mean_pop_change < 0,
            true = "PP",
            false = "stable"
          )
        )
      )
    )
  ) %>%
  distinct() %>% 
  filter(cat_change != "stable") %>%
  right_join(y = mun_caat) %>% 
  glimpse -> tab_cat_change

tab_cat_change %>% 
ggplot() +
  geom_sf(aes(geometry = geom, fill = cat_change)) +
  scale_fill_viridis_d(
    labels = c(
      "Off-farm livelihoods",
      "Land abandonment",
      "Small-holders settlement",
      "Disposession"
    ),
    na.value = "grey")+
  theme_map()+
  theme(legend.title = element_blank()) -> map_category_change

ggsave(plot=map_category_change, filename = here("img/map_category_change.jpg"))
       