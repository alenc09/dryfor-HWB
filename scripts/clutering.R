#script para construir clustering dos tipos de municípios

#Libaries----
library(here)
library(readxl)
library(ape)
library(vegan)
library(dplyr)
library(tidyr)
library(cluster)
library(Rtsne)
library(ggplot2)
library(GGally)
library(geobr)
library(spdep)
library(rgeoda)

#Data----
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

### Table for absolute change in development indicators----
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

### Table for relative change in development indicators----
tab_mun %>%
  mutate(
    code_muni = code_muni,
    mean_change_nvc = mean_change_nvc_mun,
    mean_change_fpp = mean_change_fpp_mun,
    perc_change_agrifam = ((perc_area_agrifam_17/perc_area_agrifam_06)-1)*100,
    perc_change_popUrb = ((pop_urb_2010/pop_urb_2000)-1)*100,
    perc_change_idhL = ((IDHM_L_2010/IDHM_L_2000)-1)*100,
    perc_change_expov = ((expov_2010/expov_2000)-1)*100,
    perc_change_gini = ((gini_2010/gini_2000)-1)*100,
    perc_change_u5mort = ((U5mort_2010/u5mort_2000)-1)*100,
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
  glimpse -> tab_perc_change_mun

#analysis----
##absolute change----
###pcoa----
decostand(
  select(
    .data = tab_abs_change_mun,
    -code_muni,
    -cat_change,
    # -mean_nvc_2000,
    # -mean_nvc_2010,
    # -sum_fpp_2000,
    # -sum_fpp_2010,
    # -mean_change_nvc,
    # -mean_change_fpp
    ),
  method = "standardize"
  ) %>% 
vegdist(
  method = "euclidean"
  ) -> dist_abs_change_mun
pcoa(dist_abs_change_mun) -> pcoa_abs_change_mun

summary(pcoa_abs_change_mun)

biplot.pcoa(
  pcoa_abs_change_mun,
  Y = tab_abs_change_mun[,-1:-2]
  )

betadisper(
  dist_abs_change_mun,
  group = tab_abs_change_mun$cat_change
  )-> betadisp_abs_change_mun

permutest(betadisp_abs_change_mun)

plot(betadisp_abs_change_mun,
     hull=F,
     ellipse=T,
     label = T,
     label.cex = 0.5
     )

TukeyHSD(betadisp_abs_change_mun)

###multivariate clustering through trees----
hclust(d = dist_abs_change_mun, method = "average") -> clust_abs_change_mun
plot(clust_abs_change_mun, cex =0.5, hang = 0)
rect.hclust(tree = clust_abs_change_mun, k = 4)

cutree(clust_abs_change_mun, k = 4) -> membs
hclust(d = mun_dist, method = "average", members = membs) -> clust_membs
plot(clust_membs, cex = 0.5)

##"natural" grouping----
sil_width_abs_change_mun <- c()

for(i in 2:8){
  
  pam_fit_abs_change_mun <- pam(
    dist_abs_change_mun,
    diss = TRUE,
    k = i
    )
  
  sil_width_abs_change_mun[i] <- pam_fit_abs_change_mun$silinfo$avg.width
  
}

plot(
  1:8,
  sil_width_abs_change_mun,
  xlab = "Number of clusters",
  ylab = "Silhouette Width"
  )

lines(
  1:8,
  sil_width_abs_change_mun
  )

pam(
  dist_abs_change_mun,
  diss = TRUE,
  k = 2
  ) -> pam_fit2_abs_change_mun

tab_abs_change_mun %>% 
  dplyr::select(-code_muni,
                # -ends_with(c("2000","2010")),
                -mean_change_nvc,
                -mean_change_fpp
                ) %>%
  mutate(cluster = pam_fit2_abs_change_mun$clustering,
         cat_change = as.factor(cat_change)
         ) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.)
     ) -> pam_results_abs_change_mun

pam_results_abs_change_mun$the_summary

set.seed(123)
Rtsne(dist_abs_change_mun,
      is_distance = TRUE
      ) -> tsne_obj_abs_change_mun

tsne_obj_abs_change_mun$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit2_abs_change_mun$clustering),
         name = tab_abs_change_mun$code_muni) -> tsne_data_abs_change_mun

ggplot(aes(x = X, y = Y), data = tsne_data_abs_change_mun) +
  geom_point(aes(color = cluster))

###pca----
tab_abs_change_mun %>% 
  select(-1:-6) %>% 
  glimpse %>% 
prcomp(scale. = T, center = T) -> pca_abs_change

summary(pca_abs_change)
biplot(pca_abs_change)

pca_abs_change$x

ggplot(data = as.data.frame(pca_abs_change$x), aes(x = PC1, y = PC2))+
  geom_point()

tab_abs_change_mun %>% 
  select(cat_change) %>% 
  cbind(pca_abs_change$x) %>% 
  ggplot()+
  geom_point(aes(x = PC1, y = PC2, color = cat_change))

###geographical cluster----
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
####global----
moran.mc(x = tab_abs_change_mun$mean_change_nvc,
         listw = mat_dist_list_abs_change_mun,
         nsim = 999,
         zero.policy = T) -> Gmoran_change_nvc

moran.mc(x = tab_abs_change_mun$mean_change_fpp,
         listw = mat_dist_list_abs_change_mun,
         nsim = 999,
         zero.policy = T) -> Gmoran_change_fpp

####local----
localmoran(x = tab_abs_change_mun$mean_change_nvc,
           listw = mat_dist_list_abs_change_mun,
           zero.policy = T) -> Lmoran_change_nvc

queen_weights(sf_obj = st_as_sf(tab_abs_change_mun_map)) -> qw_tab_abs_change_mun
 
local_moran(w = qw_tab_abs_change_mun,
            df = tab_abs_change_mun["mean_change_nvc"],
            ) -> Lmoran_change_nvc

lisa_labels(Lmoran_change_nvc) -> moran_lbls
setNames(lisa_colors(Lmoran_change_nvc), moran_lbls) -> moran_colors

tab_abs_change_mun_map %>%
  st_drop_geometry() %>%
  select(code_muni) %>%
  mutate(cluster_num = lisa_clusters(Lmoran_change_nvc) + 1, # add 1 bc clusters are zero-indexed
         cluster = factor(moran_lbls[cluster_num], levels = moran_lbls)) %>%
  right_join(tab_abs_change_mun_map, by = "code_muni") %>%
  st_as_sf() %>%
  glimpse -> nvc_clusterd

ggplot(nvc_clusterd, aes(fill = cluster)) +
  geom_sf(color = "white", size = 0) +
  scale_fill_manual(values = moran_colors, na.value = "green") +
  theme_dark()

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
  glimpse -> fpp_clusterd

ggplot(fpp_clusterd, aes(fill = cluster)) +
  geom_sf(color = "white", size = 0) +
  scale_fill_manual(values = moran_colors_fpp, na.value = "green") +
  theme_dark()
