# Tue Sep 12 17:00:18 2023 ------------------------------
#Script to creat LISA clusters

#library----

#data----
read.csv(file = here("data/tabela_geral.csv")) -> tab_geral
read_xlsx(path = "/home/alenc/Documents/Doutorado/tese/cap1/forest-develop/data/dbcap1_rma.xlsx") -> dbcap1_rma
read_xlsx(path = "/home/alenc/Documents/Doutorado/tese/cap0/data/atlas_dadosbrutos_00_10.xlsx", 
          sheet = 2) -> dados_atlas

##table urban population----
dados_atlas %>% 
  select(i, Codmun7, pesourb) %>% 
  pivot_wider(names_from = i, values_from = pesourb, names_prefix = "pop_urb_") %>% 
  glimpse -> pop_urb

##table municipalities----
tab_geral %>% 
  select(buff_id, code_muni, pland_nvc_06, pland_nvc_17, vari_perc_nvc, pop_rural_WP_06,
         pop_rural_WP_17, vari_perc_pop_rural, perc_area_agrifam_06, perc_area_agrifam_17) %>% 
  left_join(y = select(dbcap1_rma, code_muni, IDHM_L_2000, IDHM_L_2010, expov_2000,
                       expov_2010, gini_2000, gini_2010, u5mort_2000, U5mort_2010),
            by = "code_muni") %>%
  left_join(y = pop_urb, by = c("code_muni" = "Codmun7")) %>%
  group_by(code_muni) %>%
  summarise(mean_nvc_2000 = mean(pland_nvc_06),
            mean_nvc_2010 = mean(pland_nvc_17),
            # mean_change_nvc = mean(vari_perc_nvc),
            mean_change_nvc_mun = mean_nvc_2010 - mean_nvc_2000,
            sum_fpp_2000 = sum(pop_rural_WP_06),
            sum_fpp_2010 = sum(pop_rural_WP_17),
            # mean_change_fpp = mean(vari_perc_pop_rural),
            mean_change_fpp_mun = ((sum_fpp_2010/sum_fpp_2000)-1)*100,
            across(.cols = perc_area_agrifam_06:pop_urb_2010, .fns = mean)) %>% 
  filter(!is.na(.$code_muni)) %>%
  na.omit() %>% 
  glimpse -> tab_mun

## Table for absolute change in development indicators----
tab_mun %>%
  mutate(
    code_muni = code_muni,
    mean_change_nvc = mean_change_nvc_mun,
    mean_change_fpp = mean_change_fpp_mun,
    change_agrifam = perc_area_agrifam_17 - perc_area_agrifam_06,
    change_popUrb = pop_urb_2010 - pop_urb_2000,
    change_idhL = IDHM_L_2010 - IDHM_L_2000,
    change_expov = expov_2010 - expov_2000,
    change_gini = gini_2010 - gini_2000,
    change_u5mort = U5mort_2010 - u5mort_2000,
    .keep = "unused"
  ) %>% 
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
    ),
    .before = 2
  ) %>% 
  filter(cat_change != "stable") %>%
  glimpse -> tab_abs_change_mun

#analysis----
##spatial matrix----
read_municipality(simplified = F)->mun_cat

tab_abs_change_mun %>% 
  left_join(y = select(mun_cat,
                       code_muni,
                       geom),
            by = "code_muni"
  ) %>% 
  glimpse -> tab_abs_change_mun_map

poly2nb(tab_abs_change_mun_map$geom, 
        queen=TRUE
) -> mat_dist_abs_change_mun

nb2listw(mat_dist_abs_change_mun,
         zero.policy = T
) -> mat_dist_list_abs_change_mun

##fpp change----
localmoran(x = tab_abs_change_mun$mean_change_nvc,
           listw = mat_dist_list_abs_change_mun,
           zero.policy = T) -> Lmoran_change_nvc

queen_weights(sf_obj = st_as_sf(tab_abs_change_mun_map)) -> qw_tab_abs_change_mun

local_moran(w = qw_tab_abs_change_mun,
            df = tab_abs_change_mun["mean_change_fpp"],
) -> Lmoran_change_fpp

lisa_labels(Lmoran_change_fpp) -> moran_lbls_fpp
setNames(lisa_colors(Lmoran_change_fpp), moran_lbls_fpp) -> moran_colors_fpp

tab_abs_change_mun_map %>%
  st_drop_geometry() %>%
  select(code_muni) %>%
  mutate(cluster_num = lisa_clusters(Lmoran_change_fpp) + 1, # add 1 bc clusters are zero-indexed
         cluster = factor(moran_lbls_fpp[cluster_num], levels = moran_lbls_fpp)) %>%
  right_join(tab_abs_change_mun_map, by = "code_muni") %>%
  st_as_sf() %>%
  glimpse -> fpp_cluster

###map----
ggplot(fpp_cluster, aes(fill = cluster)) +
  geom_sf(color = "white", size = 0) +
  scale_fill_manual(values = moran_colors_fpp, na.value = "green") +
  theme_map()

###rates of change----
fpp_cluster %>% 
  as_tibble(.) %>% 
  filter(cluster == "High-High" | cluster == "Low-Low") %>% 
  mutate(abs_fpp_change = sum_fpp_2010 - sum_fpp_2000,
         perc_fpp_change = (abs_fpp_change/sum_fpp_2010)*100,
         annual_fpp_change = perc_fpp_change/11) %>% 
  group_by(cluster) %>%
  summarise(mean_fpp_change = mean(annual_fpp_change),
           sd_fpp_change = sd(annual_fpp_change)) %>%
  glimpse
