#script para avaliar mudança nos indicadores de desenvolvimento das paisagens 
#em relação à média geral utilizando boxplots

#Library----
library(readxl)
library(here)
library(tidyr)
library(dplyr)
library(ggplot2)

#data----
read.csv(file = here("data/tabela_geral.csv")) -> tab_geral
read_xlsx(path = "/home/alenc/Documents/Doutorado/tese/cap1/forest-develop/data/dbcap1_rma.xlsx") -> dbcap1_rma
read_xlsx(path = "/home/alenc/Documents/Doutorado/tese/cap0/data/atlas_dadosbrutos_00_10.xlsx", 
          sheet = 2) -> dados_atlas

##manipulation----
dados_atlas %>% 
  select(i, Codmun7, pesourb) %>% 
  pivot_wider(names_from = i, values_from = pesourb, names_prefix = "pop_urb_") %>% 
  glimpse -> pop_urb


tab_geral %>% 
  select(buff_id, code_muni, vari_perc_nvc, vari_perc_pop_rural, perc_area_agrifam_06, perc_area_agrifam_17,
         pop_urb_mun_06, pop_urb_mun_17) %>% 
  left_join(y = select(dbcap1_rma, code_muni, IDHM_L_2000, IDHM_L_2010, expov_2000,
                       expov_2010, gini_2000, gini_2010, u5mort_2000, U5mort_2010),
            by = "code_muni") %>% 
  left_join(y = pop_urb, by = c("code_muni" = "Codmun7")) %>%  
  group_by(code_muni) %>% 
  summarise(mean_change_nvc = mean(vari_perc_nvc),
            mean_change_fpp = mean(vari_perc_pop_rural),
            agrifam_2000 = mean(perc_area_agrifam_06),
            agrifam_2010 = mean(perc_area_agrifam_17),
            pop_urb_2000 = mean(pop_urb_2000),
            pop_urb_2010 = mean(pop_urb_2010),
            IDHM_L_2000 = mean(IDHM_L_2000),
            IDHM_L_2010 = mean(IDHM_L_2010),
            expov_2000 = mean(expov_2000),
            expov_2010 = mean(expov_2010),
            gini_2000 = mean(gini_2000),
            gini_2010 = mean(gini_2010),
            u5mort_2000 = mean(u5mort_2000),
            u5mort_2010 = mean(U5mort_2010)) %>%
  filter(!is.na(.$code_muni)) %>%
  mutate(
    cat_change = if_else(
      condition = mean_change_nvc > 0 & mean_change_fpp > 0,
      true = "GG",
      false = if_else(
        condition = mean_change_nvc > 0 & mean_change_fpp < 0,
        true = "GP",
        false = if_else(
          condition =  mean_change_nvc < 0 & mean_change_fpp > 0,
          true = "PG",
          false = if_else(
            mean_change_nvc < 0 & mean_change_fpp < 0,
            true = "PP",
            false = "stable"
          )
        )
      )
    )
  ) %>%
  filter(cat_change != "stable") %>% 
  pivot_longer(cols = 4:15, names_to = c(".value", "year"), names_pattern = "(.+)_(.+)") %>% 
  mutate(year = as.factor(year),
         cat_change = as.factor(cat_change)) %>% 
  glimpse -> tab_context

tab_context %>%   
  group_by(year) %>%
  summarise(across(.cols = 5:10, .fns = ~ mean(.x, na.rm = T), .names = "mean_{.col}")) %>%
  glimpse -> tab_mean_geral

###group differences----
tab_context %>% 
  left_join(y = tab_mean_geral, by = "year") %>% 
  mutate(diff_mean_agrifam =((agrifam/mean_agrifam)-1)*100,
         diff_mean_popUrb = ((pop_urb/mean_pop_urb)-1)*100,
         diff_mean_idhL = ((IDHM_L/mean_IDHM_L)-1)*100,
         diff_mean_expov = ((expov/mean_expov)-1)*100,
         diff_mean_gini = ((gini/mean_gini)-1)*100,
         diff_mean_u5mort = ((u5mort/mean_u5mort)-1)*100) %>% 
  select(code_muni, cat_change, year, 18:23) %>% 
  pivot_longer(cols = 4:9, names_to = "var", values_to =  "diff_mean") %>% 
  glimpse -> tab_diff_mean

#Figures----
##Gain-gain----
tab_diff_mean %>% 
  filter(cat_change == "GG",
         #var != "diff_mean_popUrb"
         ) %>%
  ggplot()+
  geom_boxplot(aes(x = as.factor(var), y = diff_mean, fill = year, middle = mean(diff_mean)))+
  geom_hline(yintercept = 0)+
  scale_y_continuous(trans = "sqrt")+
  labs(title = "GG")

tab_diff_mean %>% 
  filter(cat_change == "PP",
         #var != "diff_mean_popUrb"
         ) %>%
  ggplot()+
  geom_boxplot(aes(x = as.factor(var), y = log(1 + diff_mean), fill = year, middle = mean(diff_mean)))+
  geom_hline(yintercept = 0)+
  labs(title = "PP")

