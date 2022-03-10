# Wed Feb  9 14:16:12 2022 ------------------------------
#script para delimitar os setores censitários (IBGE - 2010) da Caatinga

#libaries####
library(sf)
library(here)
library(dplyr)
library(readxl)
library(geobr)
library(foreign)
library(ggplot2)

#data----
sf_use_s2(FALSE) #função para desativar a checagem de vértices duplicados

##import----
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

st_read(dsn = here("data/pop_buff_5km.shp")) -> pop_buff_5km
st_read(dsn = here("data/ibge_pop_caat_rural_clean.shp")) -> ibge_pop_rural_caat
st_read(dsn = here("data/pop_data_5km_forest_sirgas_clean.dbf")) -> pop_data_5km_forest_sirgas_clean
st_read(dsn = here("data/pop_data_5km_Nforest_sirgas.dbf")) -> pop_data_5km_Nforest_sirgas_clean

##transformation----
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

st_centroid(pop_buff_5km) -> cent_grid_buff

st_intersection(x = pop_data_5km_forest_sirgas, y = cent_grid_buff) -> pop_data_5km_forest_unique_sirgas

pop_data_5km_forest_sirgas_clean%>%
  select(id_buff, CD_GEOC, ID_UNIC, POP, V1:V23, tre_cvr, geometry)%>%
  glimpse() -> data_pop_vars

ibge_pop_caat_clip%>%
  tibble()%>%
  summarise(pop_total = sum(POP))%>%
  glimpse -> pop_total_caat

ibge_pop_rural_caat%>%
  tibble()%>%
  summarise(pop_rural_caat = sum(POP))%>%
  glimpse -> pop_rural_caat

pop_data_5km_Nforest_sirgas_clean%>%
  tibble()%>%
  summarise(pop_buff_Nforest = sum(POP))%>%
  glimpse -> pop_rural_Nforest

pop_data_5km_forest_sirgas_clean%>%
  tibble()%>%
  summarise(pop_total_buff = sum(POP))%>%
  glimpse -> pop_rural_buff

data_pop_vars%>%
  tibble()%>%
  select(-geometry)%>%
  group_by(id_buff, CD_GEOC)%>%
  summarise(pop_sc_buff = sum(POP),
            dom_sc = sum(V1),
            dom_expov = sum(V19),
            dom_Senerg = sum(V11),
            dom_cist = sum(V6),
            pop_analf = sum(V12),
            pop_sc = sum(V13)
            )%>%
  mutate(prop_dom_expov = dom_expov/dom_sc,
         pop_expov = pop_sc_buff * prop_dom_expov,
         prop_dom_Senerg = dom_Senerg/dom_sc,
         pop_Senerg = pop_sc_buff * prop_dom_Senerg,
         prop_dom_cist = dom_cist/dom_sc,
         pop_cist = pop_sc_buff * prop_dom_cist,
         prop_pop_analf = pop_analf/pop_sc,
         pop_analf_buff = pop_sc_buff * prop_pop_analf)%>%
  glimpse -> pop_result_forest
  
pop_result_forest%>%
  #ungroup()%>%
  summarise(pop_total_expov = sum(pop_expov, na.rm = T),
            pop_total_Senerg = sum(pop_Senerg, na.rm = T),
            pop_total_cist = sum(pop_cist, na.rm = T),
            pop_total_analf = sum(pop_analf_buff, na.rm = T)
            )%>%
  glimpse -> pop_result_5km

pop_rural_buff+pop_rural_Nforest -> pop_inside_buff

cbind(pop_total_caat, pop_rural_caat, pop_inside_buff, pop_rural_buff)

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

# data export----
st_write(obj = ibge_pop_caat_clip, dsn = here("data/ibge_pop_caat_clip.shp"))
st_write(obj = sc_data_caat, dsn = here("data/sc_rural_caat.shp"))
st_write(obj = ibge_pop_caat_sirgas, dsn = here("data/ibge_pop_caat.shp"))
st_write(obj = st_as_sf(buff_5km_forest), dsn = here("data/buff_5km_forest.shp"))
st_write(obj = st_as_sf(buff_5km_Nforest), dsn = here("data/buff_5km_Nforest.shp"))
st_write(obj = st_as_sf(buff_1km_forest), dsn = here("data/buff_1km_forest.shp"))
st_write(obj = st_as_sf(buff_1km_Nforest), dsn = here("data/buff_1km_Nforest.shp"))
st_write(obj = st_as_sf(buff_10km_forest), dsn = here("data/buff_10km_forest.shp"))
st_write(obj = st_as_sf(buff_10km_Nforest), dsn = here("data/buff_10km_Nforest.shp"))
st_write(obj = cent_grid_buff, dsn = here("data/cent_grid_buff.shp"))
