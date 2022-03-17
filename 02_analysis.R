# Wed Feb 23 14:34:52 2022 ------------------------------
#script para análises do cap0 (fpp-caat)

#Libraries----
library(ggplot2)
library(tidyr)

#data----
seq(from = 1606, to = 6194, by = 1) -> id_buff

buff_vars_5km_forest%>%
  distinct(id_buff, .keep_all = T)%>%
  select(-V1:-V23_wm, -CD_GEOC)%>%
  ungroup()%>%
  glimpse -> tb_data_5km

buff_vars_5km_Nforest%>%
  distinct(id_buff, .keep_all = T)%>%
  ungroup()%>%
  select(-V1:-V23_wm)%>%
  bind_cols(id_buff)%>%
  select(-id_buff, -CD_GEOC)%>%
  rename(id_buff = ...18)%>%
  bind_rows(tb_data_5km)%>%
  arrange(id_buff)%>%
  glimpse-> tb_data_5km

#analysis---
##exploratory----
tb_data_5km%>%
  pivot_longer(cols = c(-tre_cvr, -id_buff, -lnd_s_c), names_to = "vars", values_to = "values")%>%
  ggplot(aes(x = values))+
  geom_histogram()+
  facet_wrap(~ lnd_s_c + vars, scales = "free")

tb_data_5km%>%
  ggplot(aes(x = lnd_s_c, y = V9_wm_perc))+
  geom_violin()

summary(glm(data = tb_data_5km, V10_wm_perc ~ lnd_s_c))
summary(glm(data = tb_data_5km, V11_wm_perc + 0.0001 ~ lnd_s_c, family = Gamma("inverse")))
summary(glm(data = tb_data_5km, V12_wm_perc ~ lnd_s_c))
summary(glm(data = tb_data_5km, V19_wm_perc ~ lnd_s_c))
summary(glm(data = tb_data_5km, V20_wm_perc ~ lnd_s_c))
summary(glm(data = tb_data_5km, V21_wm_perc +0.0001 ~ lnd_s_c, family = Gamma("inverse")))
summary(glm(data = tb_data_5km, V5_wm_perc +0.0001 ~ lnd_s_c, family = Gamma("inverse")))
summary(glm(data = tb_data_5km, V6_wm_perc +0.0001 ~ lnd_s_c, family = Gamma("inverse")))
summary(glm(data = tb_data_5km, V7_wm_perc ~ lnd_s_c))
summary(glm(data = tb_data_5km, V8_wm_perc ~ lnd_s_c))
summary(glm(data = tb_data_5km, V9_wm_perc +0.0001 ~ lnd_s_c, family = Gamma("inverse")))
summary(MASS:::glm.nb(data = tb_data_5km, V22_wm_perc ~ lnd_s_c))
summary(MASS:::glm.nb(data = tb_data_5km, V23_wm_perc ~ lnd_s_c))

##proportions and total values----
###forest----
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

##sampled amount----
###population----
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

pop_rural_buff+pop_rural_Nforest -> pop_inside_buff

pop_inside_buff/pop_rural_caat -> sampled_pop

###household----
ibge_pop_caat_clip%>%
  tibble()%>%
  summarise(dom_total = sum(DOM_OCU))%>%
  glimpse -> dom_total_caat

ibge_pop_rural_caat%>%
  tibble()%>%
  summarise(dom_rural_caat = sum(DOM_OCU))%>%
  glimpse -> dom_rural_caat

pop_data_5km_Nforest_sirgas_clean%>%
  tibble()%>%
  summarise(dom_buff_Nforest = sum(DOM_OCU))%>%
  glimpse -> dom_rural_Nforest

pop_data_5km_forest_sirgas_clean%>%
  tibble()%>%
  summarise(dom_total_buff = sum(DOM_OCU))%>%
  glimpse -> dom_rural_buff

dom_rural_buff+dom_rural_Nforest -> dom_inside_buff

dom_inside_buff/dom_rural_caat -> sampled_dom

###area----
sc_rural_caat%>%
  st_transform(x = ., crs = 5880)%>%
  sf::st_union()%>%
  st_as_sf()%>%
  mutate(area_m2 = st_area(.))%>%
  glimpse -> sc_rural_caat

#caat_shp_polybr%>%
#  mutate(area_m2 = st_area(.))%>%
#  glimpse-> caat_shp_polybr

buff_5km_union%>%
  st_as_sf()%>%
  st_transform(x = ., crs = 5880)%>%
  mutate(area_m2 = st_area(.))%>%
  glimpse-> buff_5km_union

buff_5km_union$area_m2/sc_rural_caat$area_m2 -> sampled_area

#forest-people ----
result_5km_forest%>%
  bind_rows(result_5km_Nforest)%>%
  summarise(expov = sum(dom_expov_buff),
            s_energ = sum(dom_Senerg_buff),
            s_cist = sum(dom_Scist_buff),
            analf = sum(pop_analf_buff)
            )%>%
  mutate(buff_class = if_else(condition = id_buff <= 1605,
                              true = "forest",
                              false = "non-forest")
         )%>%
  glimpse() -> table_analysis

hist(log(table_analysis$expov))

glm(data = table_analysis,
    round(expov) ~ buff_class)%>%
    family = poisson%>%
  summary()

glm(data = table_analysis,
    s_energ + 0.0001 ~ buff_class,
    family = Gamma())%>%
  summary()

glm(data = table_analysis,
    s_cist + 0.0001 ~ buff_class,
    family = Gamma())%>%
  summary()

glm(data = table_analysis,
    analf + 0.0001 ~ buff_class,
    family = Gamma())%>%
  summary()

ks.test(x = expov_forest, y =  expov_Nforest, alternative = "greater")

table_analysis%>%
  filter(buff_class == "forest")%>%
  select(expov)%>%
  na.exclude()%>%
  glimpse ->expov_forest
pull(expov_forest) -> expov_forest

table_analysis%>%
  filter(buff_class == "non-forest")%>%
  select(expov)%>%
  na.exclude() %>% 
  glimpse ->expov_Nforest
pull(expov_Nforest) -> expov_Nforest

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

table_analysis2 %>% 
  select(id_buff, prop_dom_Senerg, buff_class) %>% 
  group_by(buff_class) %>% 
  summarise(mean = mean(prop_dom_Senerg, na.rm =T),
            sd = sd(prop_dom_Senerg, na.rm =T),
            cv = sd/mean ) %>% 
  glimpse()



## forest-people proportion----

glm(data = table_analysis2,
  prop_dom_expov ~ buff_class,
  family = binomial) -> mod.expov
summary(mod.expov)
#performance::check_overdispersion(mod.expov)

glm(data = table_analysis2,
    prop_dom_Senerg ~ buff_class,
    family = binomial) -> mod.Senerg
summary(mod.Senerg)
performance::check_overdispersion(mod.Senerg)

glm(data = table_analysis2,
    prop_dom_Scist ~ buff_class,
    family = binomial)-> mod.Scist
summary(mod.Scist)
performance::check_overdispersion(mod.Scist)

glm(data = table_analysis2,
    prop_pop_analf ~ buff_class,
    family = binomial) -> mod.analf
summary(mod.analf)
#performance::check_overdispersion(mod.analf)
