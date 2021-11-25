# Thu Nov 18 15:33:08 2021 ------------------------------
#Script para testar amostragem de Bastin et al contra mapeamento do mapbiomas

#Library
library(here)
library(dplyr)
library(raster)
library(sf)
library(ggplot2)

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

raster(x=here("data/mapbiomas-brazil-collection-50-2015_5880.tif"))-> mapbiomas_caat

# analysis ----
## forested plots vs mapbiomas land-use ----
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
  filter(land_use_mapb != is.na(.$land_use_mapb))%>%
  glimpse-> plotdf_mapb

plotdf_mapb%>%
  filter(land_use_category == "forest")%>%
  group_by(land_use_mapb)%>%
  dplyr::summarise(land_use_count = n())%>%
  glimpse-> forest_mapblu

## forest cover by buffer ----
rcl<- matrix(c(3,1,
               4,1,
               5,1,
               9,0,
               11,1,
               12,1,
               32,1,
               29,1,
               13,1,
               15,0,
               39,0,
               20,0,
               41,0,
               36,0,
               21,0,
               23,0,
               24,0,
               30,0,
               25,0,
               33,0,
               31,0,
               27,0
),
ncol = 2, byrow = T)
reclassify(x = mapbiomas_caat, rcl = rcl)-> caat_nvc
raster::extract(x = caat_nvc,
                y = buff_1km_union,
                fun = sum,
                na.rm = FALSE) -> forest_1km
raster::extract(x = caat_nvc,
                y = buff_5km_union,
                fun = sum,
                na.rm = FALSE) -> forest_5km
raster::extract(x = caat_nvc,
                y = buff_10km_union,
                fun = sum,
                na.rm = FALSE) -> forest_10km
# data visualization ----
forest_mapblu%>%
  ggplot(aes(x = factor(land_use_mapb, 
                        levels = land_use_mapb[order(land_use_count,
                                                     decreasing = TRUE)]
  ),
  y = land_use_count
  )
  ) +
  xlab(label = "Land use - Mapbiomas") + ylab(label = "Number of forested plots")+
  geom_col() -> freq_forest_landuse

# data export ----
write.csv(x = all_plots_caat, file = "E:/lucas_alencar/downloads/all_plots_caat.csv")