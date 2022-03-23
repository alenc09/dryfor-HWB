# Wed Feb 23 14:34:52 2022 ------------------------------
#script para análises do cap4 (fpp-caat)

#Libraries----
library(ggplot2)
library(tidyr)
library(dplyr)
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

##data on people and households inside buffers----
data_pop_vars_forest%>%
  tibble()%>%
  select(-geometry)%>%
  group_by(id_buff,
           CD_GEOC
           )%>%
  summarise(pop_buff = sum(POP),
            dom_buff = sum(DOM_OCU),
            dom_sc = sum(V1),
            dom_expov = sum(V19),
            dom_Senerg = sum(V11),
            dom_cist = sum(V6),
            pop_analf = sum(V12),
            pop_sc = sum(V13),
  )%>%
  mutate(prop_dom_expov = dom_expov/dom_sc,
         dom_expov_buff = dom_buff * prop_dom_expov,
         prop_dom_Senerg = dom_Senerg/dom_sc,
         dom_Senerg_buff = dom_buff * prop_dom_Senerg,
         prop_dom_Scist = 1-(dom_cist/dom_sc),
         dom_Scist_buff = dom_buff * prop_dom_Scist,
         prop_pop_analf = pop_analf/pop_sc,
         pop_analf_buff = pop_buff * prop_pop_analf
  )%>%
  glimpse -> result_5km_forest

result_5km_forest%>%
  ungroup()%>%
  summarise(dom_total_expov = sum(dom_expov_buff, na.rm = T),
            dom_total_Senerg = sum(dom_Senerg_buff, na.rm = T),
            dom_total_Scist = sum(dom_Scist_buff, na.rm = T),
            pop_total_analf = sum(pop_analf_buff, na.rm = T)
  )%>%
  glimpse -> total_5km_forest

###non-forest----
data_pop_vars_Nforest%>%
  tibble()%>%
  select(-geometry)%>%
  group_by(id_buff,
           CD_GEOC
           )%>%
  summarise(pop_buff = sum(POP),
            dom_buff = sum(DOM_OCU),
            dom_sc = sum(V1),
            dom_expov = sum(V19),
            dom_Senerg = sum(V11),
            dom_cist = sum(V6),
            pop_analf = sum(V12),
            pop_sc = sum(V13),
            )%>%
  mutate(prop_dom_expov = dom_expov/dom_sc,
         dom_expov_buff = dom_buff * prop_dom_expov,
         prop_dom_Senerg = dom_Senerg/dom_sc,
         dom_Senerg_buff = dom_buff * prop_dom_Senerg,
         prop_dom_Scist = 1-(dom_cist/dom_sc),
         dom_Scist_buff = dom_buff * prop_dom_Scist,
         prop_pop_analf = pop_analf/pop_sc,
         pop_analf_buff = pop_buff * prop_pop_analf
         )%>%
  glimpse -> result_5km_Nforest

result_5km_Nforest%>%
  ungroup()%>%
  summarise(dom_total_expov = sum(dom_expov_buff, na.rm = T),
            dom_total_Senerg = sum(dom_Senerg_buff, na.rm = T),
            dom_total_Scist = sum(dom_Scist_buff, na.rm = T),
            pop_total_analf = sum(pop_analf_buff, na.rm = T)
  )%>%
  glimpse -> total_5km_Nforest

result_5km_forest%>%
  bind_rows(result_5km_Nforest)%>%
  summarise(pop = sum(pop_buff),
            dom = sum(dom_buff),
            dom_expov = sum(dom_expov_buff),
            dom_Senerg = sum(dom_Senerg_buff),
            dom_Scist = sum(dom_Scist_buff),
            pop_analf = sum(pop_analf_buff)
            )%>%
  mutate(prop_dom_expov = dom_expov/dom,
         prop_dom_Senerg = dom_Senerg/dom,
         prop_dom_Scist = dom_Scist/dom,
         prop_pop_analf = pop_analf/pop,
         buff_class = if_else(id_buff <= 1605, true = "forest", false = "non-forest")
         ) %>% 
  glimpse -> table_analysis2
