# Wed Feb  9 14:16:12 2022 ------------------------------
#script para delimitar os setores censitários (IBGE - 2010) da Caatinga

#libaries####
library(sf)
library(here)
library(dplyr)
library(readxl)

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

list.files(path = "E:/lucas_alencar/downloads/grade_ibge_caatinga", pattern="//.shp$", full.names=TRUE)%>%
  lapply(X = .,read_sf)%>%
  do.call(what = rbind)-> ibge_pop_caat

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
  as_Spatial()->buff_1km

st_buffer(x = plot_caat_polybr, dist = 5000)%>%
  as_Spatial()->buff_5km

st_buffer(x = plot_caat_polybr, dist = 10000)%>%
  as_Spatial()-> buff_10km


## visualization ####
#plot(sc_rural_caat$geometry) #very RAM consuming. Plot only if need
#plot(sc_data_caat$geometry)  #very RAM consuming. Plot only if need

# data export ####
st_write(obj = sc_data_caat, dsn = here("data/sc_rural_caat.shp"))
         