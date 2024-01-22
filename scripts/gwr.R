# Fri Feb 10 10:44:47 2023 ------------------------------
#script para construir modelos GWR

#Libraries----
library(here)
library(readxl)
library(dplyr)
library(tidyr)
library(geobr)
library(spdep)
library(tmaptools)
library(spgwr)
library(tmap)

#Data----
read.csv(file = here("data/tabela_geral.csv")) -> tab_geral
read_xlsx(path = "/home/alenc/Documents/Doutorado/tese/cap1/forest-develop/data/dbcap1_rma.xlsx") -> dbcap1_rma
read_xlsx(path = "/home/alenc/Documents/Doutorado/tese/cap0/data/atlas_dadosbrutos_00_10.xlsx", 
          sheet = 2) -> dados_atlas

##manipulation----
dados_atlas %>% 
  select(i,
         Codmun7,
         pesourb
  ) %>% 
  pivot_wider(names_from = i,
              values_from = pesourb,
              names_prefix = "pop_urb_"
  ) %>% 
  glimpse -> pop_urb

tab_geral %>% 
  select(buff_id,
         code_muni,
         pland_nvc_06,
         pland_nvc_17, 
         vari_perc_nvc, 
         pop_rural_WP_06,
         pop_rural_WP_17,
         vari_perc_pop_rural, 
         perc_area_agrifam_06, 
         perc_area_agrifam_17
  ) %>% 
  left_join(y = select(dbcap1_rma,
                       code_muni,
                       IDHM_L_2000, 
                       IDHM_L_2010, 
                       expov_2000,
                       expov_2010, 
                       gini_2000, 
                       gini_2010, 
                       u5mort_2000, 
                       U5mort_2010
  ),
  by = "code_muni"
  ) %>%
  left_join(y = pop_urb, 
            by = c("code_muni" = "Codmun7")
  ) %>%
  group_by(code_muni) %>%
  summarise(mean_nvc_2000 = mean(pland_nvc_06),
            mean_nvc_2010 = mean(pland_nvc_17),
            # mean_change_nvc = mean(vari_perc_nvc),
            mean_change_nvc_mun = mean_nvc_2010 - mean_nvc_2000,
            sum_fpp_2000 = sum(pop_rural_WP_06),
            sum_fpp_2010 = sum(pop_rural_WP_17),
            # mean_change_fpp = mean(vari_perc_pop_rural),
            mean_change_fpp_mun = ((sum_fpp_2010/sum_fpp_2000)-1)*100,
            across(.cols = perc_area_agrifam_06:pop_urb_2010, .fns = mean)
  ) %>% 
  filter(!is.na(.$code_muni)
  ) %>%
  na.omit() %>% 
  glimpse -> tab_mun 

### Table for absolute change in development indicators----
tab_mun %>%
  mutate(code_muni = code_muni,
         change_agrifam = perc_area_agrifam_17 - perc_area_agrifam_06,
         change_popUrb = pop_urb_2010 - pop_urb_2000,
         change_idhL = IDHM_L_2010 - IDHM_L_2000,
         change_expov = expov_2010 - expov_2000,
         change_gini = gini_2010 - gini_2000,
         change_u5mort = U5mort_2010 - u5mort_2000,
  ) %>% 
  rename(mean_change_nvc = mean_change_nvc_mun,
         mean_change_fpp = mean_change_fpp_mun,
         agrifam_2000 = perc_area_agrifam_06,
         agrifam_2010 = perc_area_agrifam_17
  ) %>% 
  mutate(cat_change = if_else(condition = mean_change_nvc > 0 & mean_change_fpp > 0,
                              true = "GG",
                              false = if_else(condition = mean_change_nvc > 0 & mean_change_fpp < 0,
                                              true = "GP",
                                              false = if_else(condition =  mean_change_nvc < 0 & mean_change_fpp > 0,
                                                              true = "PG",
                                                              false = if_else(mean_change_nvc < 0 & mean_change_fpp < 0,
                                                                              true = "PP",
                                                                              false = "stable"
                                                              )
                                              )
                              )
  ),
  .before = 2
  ) %>%
  filter(cat_change != "stable") %>%
  glimpse -> tab_abs_change_mun

read_municipality(simplified = F)->mun_cat

tab_abs_change_mun %>% 
  left_join(y = select(mun_cat,
                       code_muni,
                       geom),
            by = "code_muni"
  ) %>% 
  glimpse -> tab_abs_change_mun_map

as(st_as_sf(tab_abs_change_mun_map), 
   Class = "Spatial") -> tab_abs_change_mun_map

#Analysis----
##all municipalities----
gwr.sel(data = tab_abs_change_mun_map,
        mean_change_fpp ~ 
          agrifam_2000 + 
          pop_urb_2000 + 
          IDHM_L_2000 + 
          expov_2000 + 
          gini_2000 + 
          u5mort_2000 +
          change_agrifam +
          change_popUrb +
          change_idhL +
          change_expov +
          change_gini +
          change_u5mort,
        adapt = T,           #Adaptative kernel distance
        ) -> bw_fpp

gwr(data = tab_abs_change_mun_map,
    mean_change_fpp ~ 
      agrifam_2000 + 
      pop_urb_2000 + 
      IDHM_L_2000 + 
      expov_2000 + 
      gini_2000 + 
      u5mort_2000 +
      change_agrifam +
      change_popUrb +
      change_idhL +
      change_expov +
      change_gini +
      change_u5mort,
    adapt = bw_fpp,
    hatmatrix = T,
    se.fit = T
    ) -> gwr_fpp

###spatial autocorrelation----
poly2nb(tab_abs_change_mun_map, 
        queen=TRUE
) -> mat_dist_abs_change_mun

nb2listw(mat_dist_abs_change_mun,
         zero.policy = T) -> mat_dist_list_abs_change_mun

gwr.morantest(x = gwr_fpp, lw = mat_dist_list_abs_change_mun)

as.data.frame(gwr_fpp$SDF) -> results_fpp
cbind(tab_abs_change_mun_map, as.matrix(results_fpp))-> results_fpp_map

results_fpp$change_gini/results_fpp$change_gini_se
## map results----
st_as_sf(results_fpp_map) -> results_fpp_map2

qtm(results_fpp_map2, fill = "localR2")
qtm(results_fpp_map2, fill = "IDHM_L_2000.1")
qtm(results_fpp_map2, fill = "change_gini.1")
