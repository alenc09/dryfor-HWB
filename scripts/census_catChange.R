# Fri May 13 08:34:10 2022 ------------------------------
#Script para contar as características de cada grupo de mudança

#library----
library(here)
library(dplyr)

#data----
read.csv(here("tabela_geral.csv"))-> tab_geral

#análises----
tab_geral %>%
  group_by(code_muni) %>%
  summarise(
    mean_pop_change = mean(vari_perc_pop_rural),
    mean_nvc_change = mean(vari_perc_nvc),
    mean_nvc_2017 = mean(pland_nvc_17),
    fpp_2017 = sum(pop_rural_WP_17)
  ) %>%
  mutate(
    cat_change = if_else(
      condition = mean_nvc_change > 0 & mean_pop_change > 0,
      true = "GG",
      false = if_else(
        condition = mean_nvc_change > 0 & mean_pop_change < 0,
        true = "GP",
        false = if_else(
          condition =  mean_nvc_change < 0 & mean_pop_change > 0,
          true = "PG",
          false = if_else(
            mean_nvc_change < 0 & mean_pop_change < 0,
            true = "PP",
            false = "stable"
          )
        )
      )
    )
  ) -> tab_census

levels(as.factor(tab_census$cat_change))

tab_census %>%
  #mutate(code_muni = as.factor(code_muni)) %>% 
  group_by(cat_change) %>% 
  summarise(n_mun = length(code_muni),
            fpp_2017 = sum(fpp_2017),
            mean_pop_change = mean(mean_pop_change),
            mean_nvc_2017 = mean(mean_nvc_2017),
            mean_nvc_change = mean(mean_nvc_change)) %>% 
  glimpse -> tab_cat_change
