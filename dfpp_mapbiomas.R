# Thu Nov 18 15:33:08 2021 ------------------------------
#Script para testar amostragem de Bastin et al contra mapeamento do mapbiomas

#Library
library(here)
library(dplyr)
library(raster)
library(sf)
library(ggplot2)
library(data.table)


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

list.files(path = "E:/lucas_alencar/downloads/grade_ibge_caatinga", pattern="//.shp$", full.names=TRUE)%>%
  lapply(X = .,read_sf)%>%
  do.call(what = rbind)-> ibge_pop_caat
st_intersection(x = ibge_pop_caat, y = caat_shp) -> ibge_pop_caat_clip
st_centroid(ibge_pop_caat_clip) -> cent_popibge

raster(x=here("data/caat_nvc.tif"))-> caat_nvc

raster(x="D:/lucas_alencar/downloads/caat_pop_landscan_2010.tif") ->caat_pop_landscan_sirgas2000
raster(x="D:/lucas_alencar/downloads/caat_pop_landscan_2019.tif") ->caat_pop_landscan_sirgas2000_2019

## data transformation ----
buff_1km_union%>%
  st_as_sf()%>%
  st_transform(crs = 4674) -> buff_1km_union_sigas2000

buff_5km_union%>%
  st_as_sf()%>%
  st_transform(crs = 4674)-> buff_5km_union_sigas2000

buff_10km_union%>%
  st_as_sf()%>%
  st_transform(crs = 4674)-> buff_10km_union_sigas2000

caat_pop_landscan_sirgas2000[is.na(caat_pop_landscan_sirgas2000)] <- 0
caat_pop_landscan_sirgas2000_2019[is.na(caat_pop_landscan_sirgas2000_2019)] <- 0

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

#reclassify(x = mapbiomas_caat, rcl = rcl)-> caat_nvc

aggregate(x = caat_nvc, fact = 33, fun = "max") -> caat_nvc_1000

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

## FPP by IBGE ----
cent_popibge[buff_1km_union_sigas2000,] -> cent_included_1km
sum(cent_included_1km$POP)-> ibge_1km_cent
cent_popibge[buff_5km_union_sigas2000,] -> cent_included_5km
sum(cent_included_5km$POP)-> ibge_5km_cent
cent_popibge[buff_10km_union_sigas2000,] -> cent_included_10km
sum(cent_included_10km$POP)-> ibge_10km_cent

st_intersection(x = ibge_pop_caat, y = buff_1km_union_sigas2000) -> ibge_1km
sum(ibge_1km$POP)->ibgepop_1km
st_intersection(x = ibge_pop_caat, y = buff_5km_union_sigas2000) -> ibge_5km
sum(ibge_5km$POP)->ibgepop_5km
st_intersection(x = ibge_pop_caat, y = buff_10km_union_sigas2000) -> ibge_10km
sum(ibge_10km$POP)->ibgepop_10km

## FPP by Landscan ----
### 2010 ----
raster::extract(x = caat_pop_landscan_sirgas2000,
                y = buff_1km_union_sigas2000,
                fun = sum,
                na.rm = FALSE) -> landscanpop_1km

raster::extract(x = caat_pop_landscan_sirgas2000,
                y = buff_5km_union_sigas2000,
                fun = sum,
                na.rm = FALSE) -> landscanpop_5km

raster::extract(x = caat_pop_landscan_sirgas2000,
                y = buff_10km_union_sigas2000,
                fun = sum,
                na.rm = FALSE) -> landscanpop_10km

###2019 ----
raster::extract(x = caat_pop_landscan_sirgas2000_2019,
                y = buff_1km_union_sigas2000,
                fun = sum,
                na.rm = FALSE) -> landscanpop_1km_2019

raster::extract(x = caat_pop_landscan_sirgas2000_2019,
                y = buff_5km_union_sigas2000,
                fun = sum,
                na.rm = FALSE) -> landscanpop_5km_2019

raster::extract(x = caat_pop_landscan_sirgas2000_2019,
                y = buff_10km_union_sigas2000,
                fun = sum,
                na.rm = FALSE) -> landscanpop_10km_2019

#results
c("IBGE - 2010",  "Landscan - 2010", "Landscan - 2019", "WorldPop - 2020") -> source
c(round(ibgepop_1km), round(landscanpop_1km), round(landscanpop_1km_2019), round(pop1km[2])) -> buffer_1km
c(round(ibgepop_5km), round(landscanpop_5km), round(landscanpop_5km_2019), round(pop5km[2])) -> buffer_5km
c(round(ibgepop_10km), round(landscanpop_10km), round(landscanpop_10km_2019), round(pop10km[2])) -> buffer_10km
data.frame(source, buffer_1km, buffer_5km, buffer_10km)-> df_pop_all
glimpse(df_pop_all)

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
writeRaster(x = caat_nvc, filename = here("data/caat_nvc.tif"))
