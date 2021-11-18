# Thu Nov 18 15:33:08 2021 ------------------------------
#Script para testar amostragem de Bastin et al contra mapeamento do mapbiomas

#Library
library(here)
library(dplyr)
library(raster)
library(sf)

# Data ----
df_plots %>% #filtering only plots in Caatinga
  filter(
    location_x >= -45.07814 & location_x <= -35.06698,
    location_y >= -16.71264 & location_y <= -2.748264
    
  ) %>%
  glimpse -> all_plots_caat
st_as_sf(
  x = all_plots_caat,
  coords = c("location_x", "location_y"),
  crs = 4326
) -> all_plots_caat_points

raster(x="E:/lucas_alencar/downloads/mapbiomas-brazil-collection-50-caatinga-2017.tif")-> mapbiomas_caat

# analysis ----
all_plots_caat_points%>%
  mutate(land_use_mapb = raster::extract(x = mapbiomas_caat, y= .))%>%
  mutate(land_use_mapb = case_when(
    land_use_mapb== 3 ~ "forest",
    land_use_mapb== 4 ~ "savanna",
    land_use_mapb== 5 ~ "mangrove",
    land_use_mapb== 9 ~ "forest_plantation",
    land_use_mapb== 11 ~ "wetland",
    land_use_mapb== 12 ~ "grassland",
    land_use_mapb== 32 ~ "salt_flat",
    land_use_mapb== 29 ~ "rocky_outcrop",
    land_use_mapb== 13 ~ "other_non-forests",
    land_use_mapb== 15 ~ "pasture",
    land_use_mapb== 39 ~ "soybean",
    land_use_mapb== 20 ~ "sugarcane",
    land_use_mapb== 41 ~ "other_temp_crop",
    land_use_mapb== 36 ~ "perennial_crop",
    land_use_mapb== 21 ~ "agriculture_pasture",
    land_use_mapb== 23 ~ "sand",
    land_use_mapb== 24 ~ "urban",
    land_use_mapb== 30 ~ "mining",
    land_use_mapb== 25 ~ "other_shits",
    land_use_mapb== 33 ~ "river",
    land_use_mapb== 31 ~ "aquiculture",
    land_use_mapb== 27 ~ "non_obs"
    ))%>%
  filter(land_use_mapb != is.na)
  glimpse

# data export ----
write.csv(x = all_plots_caat, file = "E:/lucas_alencar/downloads/all_plots_caat.csv")
