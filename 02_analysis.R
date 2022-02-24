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
