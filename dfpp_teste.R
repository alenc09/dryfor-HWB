# Tue Nov 09 11:46:47 2021 ------------------------------
#Script para montar mapa de forest proximate people na Caatinga

#library
library(raster)
library(geobr)
library(dplyr)
library(here)

#data----
read_biomes(simplified = F)%>%  
filter(name_biome == "Caatinga")%>%
  glimpse ->caat_shp

read.csv(file = here("data/aam6527_Bastin_Database-S1.csv"))->df_plots

raster(x = here("data/bra_ppp_2020_UNadj_constrained.tif"))-> br_pop

#data manipulation----

