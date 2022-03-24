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
library(stringr)
library(lme4)
library(GGally)

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

buff_lsm%>%
  select(plot_id, metric, class, value) %>% 
  filter(plot_id != 1606) %>% 
  pivot_wider(id_cols = plot_id,
              names_sep = "_",
              names_from = c(metric, class),
              values_from = value
              ) %>% 
  left_join(y=table_analysis2, by = c("plot_id" = "id_buff")) %>%
  glimpse -> table_analysis3

table_analysis3[,2:48][is.na(table_analysis3[,2:48])] <- 0

#buff_lsm %>%
#  dplyr::group_by(plot_id, metric, class) %>%
#  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
#  dplyr::filter(n > 1L)

result_5km_forest%>%
  bind_rows(result_5km_Nforest)%>%
  ungroup() %>%
  select(id_buff, CD_GEOC) %>% 
  mutate(code_muni = str_sub(CD_GEOC, 1, 7)) %>% 
  mutate(across(.fns = as.factor)) %>% 
  select(-CD_GEOC) %>% 
  distinct(id_buff, .keep_all = T) %>% 
  right_join(y=table_analysis3, by = c("id_buff" = "plot_id")) %>% 
  glimpse -> table_analysis4

table_analysis4[,3:49][is.na(table_analysis4[,3:49])] <- 0

## linear models ----
### landscape diversity only ----
table_analysis3 %>%
  ggplot(aes(x=shdi_NA, y= prop_dom_expov))+
  geom_point() +
  geom_smooth(method = 'lm',
              y ~ x)

ggplot(data = table_analysis3, aes(x = shdi_NA))+
  geom_density()
  
glm(data = table_analysis3,
    prop_dom_expov ~ shdi_NA,
    family = quasibinomial) %>% 
  #plot() %>% 
  summary()

### controlling for other land covers ----
glm(data = table_analysis3,
    prop_dom_expov ~ shdi_NA + pland_4 + pland_15,
    family = quasibinomial) %>% 
  #plot() %>% 
  summary()

ggplot(data = table_analysis3, aes(x = pland_4, y = shdi_NA, color = prop_dom_expov > 0.3))+
  geom_point()+
  geom_smooth(method = "loess")

ggplot(data = table_analysis3, aes(x = pland_15, y = prop_dom_expov))+
  geom_point()+
  geom_smooth(method = "loess")

table_analysis3 %>% 
dplyr::select(pland_3, pland_4, pland_15, shdi_NA, dom_expov, prop_dom_expov) %>% 
ggpairs()

ggplot(table_analysis3, aes(x = sqrt(pland_3), y = dom_expov))+
  geom_point()+
  geom_smooth(method = "gam")

###log-transform----
table_analysis3 %>%
  ggplot(aes(x=log(shdi_NA + 0.0001), y= prop_dom_expov+0.001))+
  geom_point() +
  geom_smooth(method="loess")

ggplot(data = table_analysis3, aes(x = log(pland_4), y = prop_dom_expov))+
  geom_point()+
  geom_smooth(method = "loess")

ggplot(data = table_analysis3, aes(x = log(pland_15+0.0001), y = prop_dom_expov))+
  geom_point()+
  geom_smooth(method = "loess")

## quadratic models----
table_analysis3 %>%
  ggplot(aes(x=shdi_NA, y= prop_dom_expov))+
  geom_point() +
  geom_smooth(formula = y ~ x + I(x^2),
              method = "lm")

glm(data = table_analysis3,
    prop_dom_expov ~ shdi_NA + I(shdi_NA^2),
    family = quasibinomial) %>% 
  #plot() %>% 
  summary()

glm(data = table_analysis3,
    prop_dom_expov ~ pland_4 + I(pland_4^2),
    family = quasibinomial) %>% 
  #plot() %>% 
  summary()

glm(data = table_analysis3,
    prop_dom_expov ~ log(pland_15+0.0001) + I(pland_15^2),
    family = quasibinomial) %>% 
  #plot() %>% 
  summary()

## mixed effect----
glmer(data = table_analysis4,
      prop_dom_expov ~ shdi_NA + (1|code_muni),
      family = binomial) %>% 
  summary()

glmer(data = table_analysis4,
      prop_dom_expov ~ shdi_NA + pland_4 + pland_15 + (1|code_muni),
      family = binomial) %>% 
  summary()