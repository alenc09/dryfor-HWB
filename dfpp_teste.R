# Tue Nov 09 11:46:47 2021 ------------------------------
#Script para montar mapa de forest proximate people na Caatinga

#library
library(raster)
library(geobr)
library(dplyr)
library(here)
library(sf)

#data----
read_biomes(simplified = F) %>%
  filter(name_biome == "Caatinga") %>%
  glimpse -> caat_shp

read.csv(file = here("data/aam6527_Bastin_Database-S1.csv"),
         sep = ";") -> df_plots

raster(x = here("data/bra_ppp_2020_UNadj_constrained.tif")) -> br_pop
raster(x = here("data/pop_caat_polybr_1000.tif")) -> pop_caat_polybr_1000

#data manipulation----
## Caatinga shape
st_transform(caat_shp, crs = 4326) -> caat_shp_wgs84 #coordinate system change to wgs84
as_Spatial(caat_shp_wgs84) -> caat_shp_wgs84_sp #object class change to be used in crop and mask functions
st_transform(caat_shp, crs = 5880) ->caat_shp_polybr

##population data
raster::crop(x = br_pop, y = caat_shp_wgs84_sp) -> br_pop_crop
raster::mask(x = br_pop_crop, mask = caat_shp_wgs84_sp) -> pop_caat_wgs84 #filtering only population in Caatinga
aggregate(x = pop_caat_wgs84, fact = 10, fun = sum) ->pop_caat_wgs84_1000
"+proj=poly +lat_0=0 +lon_0=-54 +x_0=5000000 +y_0=10000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" -> proj_polyBR
raster::projectRaster(pop_caat_wgs84_1000, crs = proj_polyBR, method = "bilinear")-> pop_caat_polybr_1000
pop_caat_polybr_1000[is.na(pop_caat_polybr_1000)] <- 0 #change "no data" to zero population
pop_caat_polybr_1000 -> pop_caat_rural_polybr
pop_caat_rural_polybr[pop_caat_rural_polybr > 130] <- 0 #excluding urban areas

##plots----
df_plots %>% #filtering only plots in Caatinga
  filter(
    location_x >= -45.07814 & location_x <= -35.06698,
    location_y >= -16.71264 & location_y <= -2.748264,
    land_use_category == "forest"
  ) %>%
  glimpse -> df_plots_caat
st_as_sf(
  x = df_plots_caat,
  coords = c("location_x", "location_y"),
  crs = 4326
) -> df_plots_caat_points
st_transform(x = df_plots_caat_points, crs = 5880)-> df_plots_caat_points_polybr

df_plots_caat_points_polybr[caat_shp_polybr,] ->plot_caat_polybr #only forested plots inside caatinga

##buffers----
st_buffer(x = plot_caat_polybr, dist = 1000)%>%
  st_union()%>%
  as_Spatial()->buff_1km_union

st_buffer(x = plot_caat_polybr, dist = 5000)%>%
  st_union()%>%
  as_Spatial()->buff_5km_union 

st_buffer(x = plot_caat_polybr, dist = 10000)%>%
  st_union()%>%
  as_Spatial()-> buff_10km_union

#Data analysis----
##rural population only----
raster::extract(x = pop_caat_rural_polybr,
                y = buff_1km_union,
                fun = sum,
                na.rm = FALSE) -> people_rural_1km
raster::extract(x = pop_caat_rural_polybr,
                y = buff_5km_union,
                fun = sum,
                na.rm = FALSE) -> people_rural_5km
raster::extract(x = pop_caat_rural_polybr,
                y = buff_10km_union,
                fun = sum,
                na.rm = FALSE) -> people_rural_10km


##all caatinga population----

#data visualization----
plot(pop_caat_polybr_1000)
plot(pop_caat_rural_polybr)
plot(pop_caat_polybr_1000 > 130)

plot(caat_shp_polybr$geom)
plot(plot_caat_polybr, add = T)

#data export----
writeRaster(x = pop_caat_polybr_1000, filename = here("data/pop_caat_polybr_1000.tif"))
