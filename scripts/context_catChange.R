# Script to evaluate changes in socioeconomic conditions per group

#Library----
library(readxl)
library(here)
library(tidyr)

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
  select(X, code_muni, vari_perc_nvc, vari_perc_pop_rural, agrifam_06, agrifam_17,
         pop_urb_mun_06, pop_urb_mun_17) %>% 
  left_join(y = select(dbcap1_rma, code_muni, IDHM_L_2000, IDHM_L_2010, expov_2000,
                       expov_2010, gini_2000, gini_2010, u5mort_2000, U5mort_2010),
            by = "code_muni") %>% 
  left_join(y = pop_urb, by = c("code_muni" = "Codmun7")) %>%  
  group_by(code_muni) %>% 
  summarise(mean_change_nvc = mean(vari_perc_nvc),
            mean_change_fpp = mean(vari_perc_pop_rural),
            agrifam_06 = mean(agrifam_06),
            agrifam_17 = mean(agrifam_17),
            pop_urb_2000 = mean(pop_urb_2000),
            pop_urb_2010 = mean(pop_urb_2010),
            IDHM_L_2000 = mean(IDHM_L_2000),
            IDHM_L_2010 = mean(IDHM_L_2010),
            expov_2000 = mean(expov_2000),
            expov_2010 = mean(expov_2010),
            gini_2000 = mean(gini_2000),
            gini_2010 = mean(gini_2010),
            u5mort_2000 = mean(u5mort_2000),
            U5mort_2010 = mean(U5mort_2010)) %>%
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
  glimpse -> tab_context

#Analysis----
