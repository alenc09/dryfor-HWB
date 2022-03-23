# Wed Feb 23 14:34:52 2022 ------------------------------
#script para análises do cap4 (fpp-caat)

#Libraries----
library(ggplot2)
library(tidyr)
library(landscapemetrics)
library(raster)
library(here)
library(sf)

#data----
raster(x = here("data/mapbiomas-brazil-collection-50-2010_5880.tif")) ->mapbiomas_caat
st_read(dsn = here("data/buffer_5km.shp")) -> all_buff
st_cast(all_buff, "MULTIPOLYGON") -> all_buff2
st_read(dsn = here("data/caat_points.shp"))-> caat_points

#buffer land cover----
landscapemetrics::sample_lsm(landscape = mapbiomas_caat,
                             y = all_buff,
                             plot_id = all_buff$id_buff,
                             #shape = "circle",
                             #size = 5000,
                             #all_classes = T,
                             return_raster = F,
                             #progress = T,
                             #what = "lsm_c_ca"
                             level = c("class", "class", "landscape"),
                             metric = c("ca","pland", "shdi")
                             )-> buff_lsm
