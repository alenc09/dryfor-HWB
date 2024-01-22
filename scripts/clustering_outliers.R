###outliers----
# tab_geral %>% 
#   rename(buff_id = X) %>% 
#   glimpse -> tab_geral
# Q <- quantile(tab_geral$vari_perc_nvc, probs=c(.25, .75), na.rm = TRUE)
# iqr <- IQR(tab_geral$vari_perc_nvc, na.rm = T)
# up <-  Q[2]+1.5*iqr # Upper Range
# low<- Q[1]-1.5*iqr # Lower Range
# 
# quantile(tab_geral$vari_perc_pop_rural, probs=c(.25, .75), na.rm = TRUE) -> Q_fpp
# IQR(tab_geral$vari_perc_pop_rural, na.rm = T) -> iqr_fpp
# Q_fpp[2]+1.5*iqr_fpp -> up_fpp # Upper Range
# Q_fpp[1]-1.5*iqr_fpp -> low_fpp # Lower Range
# 
# tab_geral %>% 
#   select(buff_id, code_muni, vari_perc_nvc, vari_perc_pop_rural, perc_area_agrifam_06, perc_area_agrifam_17) %>% 
#   filter(vari_perc_nvc < up &
#            vari_perc_nvc > low &
#            vari_perc_pop_rural < up_fpp &
#            vari_perc_pop_rural > low_fpp) %>%
#   glimpse -> tab_s_outlier
# 
# mean(tab_s_outlier$vari_perc_nvc) + sd(tab_s_outlier$vari_perc_nvc)
# mean(tab_s_outlier$vari_perc_nvc) - sd(tab_s_outlier$vari_perc_nvc)
# mean(tab_s_outlier$vari_perc_pop_rural, na.rm=T) + sd(tab_s_outlier$vari_perc_pop_rural, na.rm=T)
# mean(tab_s_outlier$vari_perc_pop_rural, na.rm=T) - sd(tab_s_outlier$vari_perc_pop_rural, na.rm=T)
# 
# tab_s_outlier %>%  
#   filter(vari_perc_nvc > 6.575527 |
#            vari_perc_nvc < -5.839117) %>% 
#   filter(vari_perc_pop_rural > 24.18684 |
#            vari_perc_pop_rural < -11.30523) %>%
#   glimpse() -> tab_sd
# 
# tab_sd %>% 
#   select(buff_id, code_muni, vari_perc_nvc, vari_perc_pop_rural, perc_area_agrifam_06, perc_area_agrifam_17) %>% 
#   left_join(y = select(dbcap1_rma, code_muni, IDHM_L_2000, IDHM_L_2010, expov_2000,
#                        expov_2010, gini_2000, gini_2010, u5mort_2000, U5mort_2010),
#             by = "code_muni") %>% 
#   left_join(y = pop_urb, by = c("code_muni" = "Codmun7")) %>% 
#   group_by(code_muni) %>% 
#   summarise(mean_change_nvc = mean(vari_perc_nvc),
#             mean_change_fpp = mean(vari_perc_pop_rural),
#             agrifam_2000 = mean(perc_area_agrifam_06),
#             agrifam_2010 = mean(perc_area_agrifam_17),
#             pop_urb_2000 = mean(pop_urb_2000),
#             pop_urb_2010 = mean(pop_urb_2010),
#             IDHM_L_2000 = mean(IDHM_L_2000),
#             IDHM_L_2010 = mean(IDHM_L_2010),
#             expov_2000 = mean(expov_2000),
#             expov_2010 = mean(expov_2010),
#             gini_2000 = mean(gini_2000),
#             gini_2010 = mean(gini_2010),
#             u5mort_2000 = mean(u5mort_2000),
#             u5mort_2010 = mean(U5mort_2010)) %>%
#   filter(!is.na(.$code_muni)) %>%
#   na.omit() %>% 
#   mutate(code_muni = code_muni,
#          mean_change_nvc = mean_change_nvc,
#          mean_change_fpp = mean_change_fpp,
#          mean_change_agrifam = agrifam_2010 - agrifam_2000,
#          mean_change_popUrb = pop_urb_2010 - pop_urb_2000,
#          mean_change_idhL = IDHM_L_2010 - IDHM_L_2000,
#          mean_change_expov = expov_2010 - expov_2000,
#          mean_change_gini = gini_2010 - gini_2000,
#          mean_change_u5mort = u5mort_2010 - u5mort_2000,
#          .keep = "unused") %>% 
#   mutate(
#     cat_change = if_else(
#       condition = mean_change_nvc > 0 & mean_change_fpp > 0,
#       true = "GG",
#       false = if_else(
#         condition = mean_change_nvc > 0 & mean_change_fpp < 0,
#         true = "GP",
#         false = if_else(
#           condition =  mean_change_nvc < 0 & mean_change_fpp > 0,
#           true = "PG",
#           false = if_else(
#             mean_change_nvc < 0 & mean_change_fpp < 0,
#             true = "PP",
#             false = "stable"
#           )
#         )
#       )
#     ),
#     .before = 2) %>%
#   filter(cat_change != "stable") %>%
#   glimpse -> tab_munSD_change

### outliers ----
# decostand(tab_munSD_change[,-1:-2], method = "standardize") %>%
#   vegdist(method = "euclidean") -> munSD_dist
# betadisper(munSD_dist, group = tab_munSD_change$cat_change)-> dispSD
# permutest(dispSD)
# plot(dispSD, hull=F, ellipse=T, label = T, label.cex = 0.5)
# TukeyHSD(dispSD)