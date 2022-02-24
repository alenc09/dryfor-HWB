# Wed Feb  9 14:16:12 2022 ------------------------------
#script para delimitar os setores censitários (IBGE - 2010) da Caatinga

#libaries####
library(sf)
library(here)
library(dplyr)
library(readxl)
library(geobr)

#data####
sf_use_s2(FALSE) #função para desativar a checagem de vértices duplicados

##import####
list.files(path = here("data/unzip"), pattern=".shp$", full.names=TRUE)%>%
  lapply(X = .,read_sf)%>%
  do.call(what = rbind) %>%
  .[caat_shp,] -> sc_caat

st_read(dsn = here("data/sc_rural_caat.shp"))-> sc_data_caat   #run after exporting sc_rural_caat object

read_xlsx("data/IBGE_filter_data.xlsx") -> sc_ibge_data

read.csv(file = here("data/aam6527_Bastin_Database-S1.csv"),
         sep = ";") -> df_plots

list.files(path = "D:/lucas_alencar/downloads/grade_ibge_caatinga", pattern=".shp$", full.names=TRUE)%>%
  lapply(X = .,read_sf)%>%
  do.call(what = rbind)-> ibge_pop_ne

st_read(dsn = here("data/ibge_pop_caat.shp")) -> ibge_grid_data

read_sf(here("data/pop_data_5km_forest_sirgas.shp"))-> pop_data_5km_forest_sirgas
read_sf(here("data/pop_data_5km_Nforest_sirgas.shp"))-> pop_data_5km_Nforest_sirgas

raster(x = here("data/pop_caat_polybr_1000.tif")) -> pop_caat_polybr_1000

read_biomes(simplified = F) %>%
  filter(name_biome == "Caatinga") %>%
  glimpse -> caat_shp

raster(x = here("data/bra_ppp_2020_UNadj_constrained.tif")) -> br_pop

##transformation####
sc_caat%>%
  dplyr::filter(TIPO== "RURAL") ->sc_rural_caat

left_join(y = sc_ibge_data,
          x = select(.data = sc_rural_caat, CD_GEOCODI, geometry),
          by = c("CD_GEOCODI" = "Cod_setor"))%>%
  glimpse -> sc_data_caat

df_plots %>% #filtering only plots in Caatinga
  filter(
    location_x >= -45.07814 & location_x <= -35.06698,
    location_y >= -16.71264 & location_y <= -2.748264
  ) %>%
  st_as_sf(
  x = .,
  coords = c("location_x", "location_y"),
  crs = 4326
  ) %>%
  st_transform(
    x = .,
    crs = 5880)%>%
  .[caat_shp_polybr,] ->plot_caat_polybr

st_intersection(x = ibge_pop_ne, y = sc_data_caat) -> ibge_pop_caat_sirgas

raster::rasterToPoints(pop_caat_polybr_1000) %>% 
  tibble::as_tibble() ->pop_caat_tibble

st_transform(caat_shp, crs = 5880) ->caat_shp_polybr

###buffers----
#### union----
st_buffer(x = plot_caat_polybr, dist = 1000)%>%
  st_union()%>%
  as_Spatial()->buff_1km_union

st_buffer(x = plot_caat_polybr, dist = 5000)%>%
  st_union()%>%
  as_Spatial()->buff_5km_union

st_buffer(x = plot_caat_polybr, dist = 10000)%>%
  st_union()%>%
  as_Spatial()-> buff_10km_union

#### disjoint----
st_buffer(x = plot_caat_polybr, dist = 1000)%>%
  filter(land_use_category == "forest")%>%
  as_Spatial()->buff_1km_forest
st_buffer(x = plot_caat_polybr, dist = 1000)%>%
  filter(land_use_category == "Non-forest")%>%
  as_Spatial()->buff_1km_Nforest

st_buffer(x = plot_caat_polybr, dist = 5000)%>%
  filter(land_use_category == "forest")%>%
  as_Spatial()->buff_5km_forest
st_buffer(x = plot_caat_polybr, dist = 5000)%>%
  filter(land_use_category == "Non-forest")%>%
  as_Spatial()->buff_5km_Nforest

st_buffer(x = plot_caat_polybr, dist = 10000)%>%
  filter(land_use_category == "forest")%>%
  as_Spatial()->buff_10km_forest
st_buffer(x = plot_caat_polybr, dist = 10000)%>%
  filter(land_use_category == "Non-forest")%>%
  as_Spatial()->buff_10km_Nforest

###intersects----
st_intersection(x = ibge_grid_data,
                y = st_transform(
                  x = st_as_sf(buff_5km_forest),
                  crs = 4674)
                )-> pop_data_5km_forest

###Weighted means----
perc_fun <- function(x,y){
  (x/y)*100
} 
####forested plots----
pop_data_5km_forest_sirgas%>%
  as.data.frame()%>%
  group_by(CD_GEOC, id_buff)%>%
  dplyr::summarise(pop_sc_buff = sum(POP))%>%
  glimpse() -> aeee_porraa_consegui

pop_data_5km_forest_sirgas%>%
  distinct(CD_GEOC, id_buff, .keep_all = T)%>%
  left_join(x = ., y = aeee_porraa_consegui, by= c("CD_GEOC", "id_buff"))%>%
  mutate(pop_prop = pop_sc_buff/V2 , .keep = "all")%>%
  glimpse -> pop_prop_sc_buff

pop_prop_sc_buff$pop_prop[pop_prop_sc_buff$pop_prop > 1] <- 1

pop_prop_sc_buff%>%
  as.data.frame()%>%
  dplyr::select(id_buff, CD_GEOC, lnd_s_c, tre_cvr,V1, V5, V6, V7, V8, V9, V10, V11, V12, V13, V19, V20, V21, V22, V23, pop_prop)%>%
  arrange(id_buff)%>%
  group_by(id_buff)%>%
  mutate(across(.cols = V1:V23, .fns =  list("wm" = ~ weighted.mean(x = ., w = pop_prop))))%>%
  mutate(across(.cols = c(V5_wm:V11_wm, V19_wm:V21_wm), .fns = list("perc" = ~ perc_fun(x =., y = V1_wm))))%>%
  mutate(across(.cols = c(V12_wm, V22_wm, V23_wm), .fns = list("perc" = ~ perc_fun(x =., y = V13_wm))))%>%
  glimpse -> buff_vars_5km_forest
  
####Non-forested plots----
pop_data_5km_Nforest_sirgas%>%
  as.data.frame()%>%
  group_by(CD_GEOC, id_buff)%>%
  dplyr::summarise(pop_sc_buff = sum(POP))%>%
  glimpse() -> aeee_porraa_consegui_2

pop_data_5km_Nforest_sirgas%>%
  distinct(CD_GEOC, id_buff, .keep_all = T)%>%
  left_join(x = ., y = aeee_porraa_consegui_2, by= c("CD_GEOC", "id_buff"))%>%
  mutate(pop_prop = pop_sc_buff/V2 , .keep = "all")%>%
  glimpse -> pop_prop_sc_buff_5km_Nforest

pop_prop_sc_buff_5km_Nforest$pop_prop[pop_prop_sc_buff_5km_Nforest$pop_prop > 1] <- 1

pop_prop_sc_buff_5km_Nforest%>%
  as.data.frame()%>%
  dplyr::select(id_buff, CD_GEOC, lnd_s_c, tre_cvr,V1, V5, V6, V7, V8, V9, V10, V11, V12, V13, V19, V20, V21, V22, V23, pop_prop)%>%
  arrange(id_buff)%>%
  group_by(id_buff)%>%
  mutate(across(.cols = V1:V23, .fns =  list("wm" = ~ weighted.mean(x = ., w = pop_prop))))%>%
  mutate(across(.cols = c(V5_wm:V11_wm, V19_wm:V21_wm), .fns = list("perc" = ~ perc_fun(x =., y = V1_wm))))%>%
  mutate(across(.cols = c(V12_wm, V22_wm, V23_wm), .fns = list("perc" = ~ perc_fun(x =., y = V13_wm))))%>%
  glimpse -> buff_vars_5km_Nforest


## visualization ####

hist(buff_W.vars$v23_wm)

# data export ####
st_write(obj = sc_data_caat, dsn = here("data/sc_rural_caat.shp"))
st_write(obj = ibge_pop_caat_sirgas, dsn = here("data/ibge_pop_caat.shp"))
st_write(obj = st_as_sf(buff_5km_forest), dsn = here("data/buff_5km_forest.shp"))
st_write(obj = st_as_sf(buff_5km_Nforest), dsn = here("data/buff_5km_Nforest.shp"))
st_write(obj = st_as_sf(buff_1km_forest), dsn = here("data/buff_1km_forest.shp"))
st_write(obj = st_as_sf(buff_1km_Nforest), dsn = here("data/buff_1km_Nforest.shp"))
st_write(obj = st_as_sf(buff_10km_forest), dsn = here("data/buff_10km_forest.shp"))
st_write(obj = st_as_sf(buff_10km_Nforest), dsn = here("data/buff_10km_Nforest.shp"))
