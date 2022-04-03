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
library(ggpubr)
library(readxl)
library(ggtern)

#data----
raster(x = here("data/mapbiomas-brazil-collection-50-2010_5880.tif")) ->mapbiomas_caat
raster(x = here("data/mapbiomas-brazil-collection-60-caatinga-2000_5880.tif")) ->mapbiomas_caat_2000
st_read(dsn = here("data/buffer_5km.shp")) -> all_buff
st_cast(all_buff, "MULTIPOLYGON") -> all_buff2
st_read(dsn = here("data/caat_points.shp"))-> caat_points
read.csv("/home/alenc/Documents/Doutorado/tese/cap0/dryfor-HWB/table_analysis4.csv")->table_analysis4
read_xlsx("/home/alenc/Documents/Doutorado/tese/cap0/dryfor-HWB/data/tabela2006.xlsx") -> table_agrifamiliar
read_xlsx("/home/alenc/Documents/Doutorado/tese/cap0/dryfor-HWB/data/tabela2006.xlsx", sheet = 2) -> table_agrifamiliar_area
read_xlsx("/home/alenc/Documents/Doutorado/tese/cap0/dryfor-HWB/data/Tabela2017.xlsx") -> table_agrifamiliar_2017
read_xlsx("/home/alenc/Documents/Doutorado/tese/cap0/dryfor-HWB/data/Tabela2017.xlsx", sheet = 2) -> table_agrifamiliar_area_2017

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

landscapemetrics::sample_lsm(landscape = mapbiomas_caat_2000,
                             y = all_buff,
                             plot_id = all_buff$id_buff,
                             #shape = "circle",
                             #size = 5000,
                             #all_classes = T,
                             return_raster = F,
                             #progress = T,
                             #what = "lsm_c_ca"
                             level = c("class", "class"),
                             metric = c("ca","pland")
                             )-> buff_lsm_2000

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

#Exploratory----
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

## PCA----
table_analysis4 %>% 
  dplyr::select(id_buff, pop, dom, dom_expov, dom_Senerg, dom_Scist, pop_analf, prop_dom_expov,
                prop_dom_Senerg, prop_dom_Scist, prop_pop_analf, pland_3, pland_4,
                pland_12, pland_15, pland_21, pland_25,
         pland_33, pland_29, pland_24, pland_48, pland_41, pland_23, pland_20,
         pland_31, pland_5, pland_32, pland_30, pland_46, pland_9, pland_39,
         pland_11, pland_13, shdi_NA) %>% 
  dplyr::rename(pland_forest = pland_3,
         pland_savanna = pland_4,
         pland_mangrove = pland_5,
         pland_wetland = pland_11,
         pland_grass = pland_12,
         pland_salt = pland_32,
         pland_rocky = pland_29,
         pland_otherVeg = pland_13,
         pland_pasture = pland_15,
         pland_soy = pland_39,
         pland_sugar = pland_20,
         pland_otherTcrop = pland_41,
         pland_coffe = pland_46,
         pland_otherPcrop = pland_48,
         pland_Fplantation = pland_9,
         pland_mosaicAP = pland_21,
         pland_sand = pland_23,
         pland_urban = pland_24,
         pland_mine = pland_30,
         pland_otherNveg = pland_25,
         pland_water = pland_33,
         pland_aquacult = pland_31
         ) %>% 
  dplyr::mutate(pland_nvc = pland_forest + pland_savanna + pland_grass + pland_rocky +
           pland_mangrove + pland_wetland + pland_otherVeg + pland_salt,
         pland_agri = pland_pasture + pland_mosaicAP + pland_otherPcrop + 
           pland_otherTcrop + pland_sugar + pland_aquacult + pland_coffe + 
           pland_Fplantation + pland_soy, .keep="unused"
         ) %>% 
  glimpse ->table_analysis5

prcomp(table_analysis5[,-1], scale = T) -> pca
summary(pca)
plot(x = pca$x[,1], y = pca$x[,2])
pca$sdev^2 -> pca.var
pca.var/sum(pca.var)*100 -> pca.var.perc
barplot(pca.var.perc)

ggbiplot::ggbiplot(pca,
                   #choices = 3:4,
                   #obs.scale = 1,
                   #var.scale = 1,
                   alpha = 0.05)

##people and forest----
table_analysis5 %>% 
  filter(pland_nvc >=10) %>% 
  dplyr::mutate(pop_10 = sum(pop, na.rm=T), .keep = "none") %>% 
  glimpse -> pop_10

table_analysis5 %>% 
  filter(pland_nvc >=100) %>% 
  dplyr::mutate(dom_100 = sum(dom, na.rm=T), .keep = "none") %>% 
  glimpse -> dom_100

c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)-> threshold

c(pop_10[1,], pop_20[1,], pop_30[1,], pop_40[1,], pop_50[1,], pop_60[1,],
      pop_70[1,], pop_80[1,], pop_90[1,], pop_100[1,]) -> pop
c(dom_10[1,], dom_20[1,], dom_30[1,], dom_40[1,], dom_50[1,], dom_60[1,],
      dom_70[1,], dom_80[1,], dom_90[1,], dom_100[1,]) -> dom

cbind(pop, dom, threshold)-> pop_thresh
as_tibble(pop_thresh) -> pop_thresh

#table_analysis6
data.frame(table_analysis6) %>% 
  #select(-geometry) %>% 
  filter(pland_nvc >=100) %>% 
  dplyr::mutate(pop_LS_100 = sum(pop_LS_2010, na.rm=T), .keep = "none") %>% 
  glimpse -> pop_LS_100

c(pop_LS_10[1,], pop_LS_20[1,], pop_LS_30[1,], pop_LS_40[1,], pop_LS_50[1,], pop_LS_60[1,],
      pop_LS_70[1,], pop_LS_80[1,], pop_LS_90[1,], pop_LS_100[1,]) -> pop_LS
cbind(pop_LS, threshold)-> pop_thresh_LS
as_tibble(pop_thresh_LS) -> pop_thresh_LS

data.frame(table_analysis6) %>% 
  #select(-geometry) %>% 
  filter(pland_nvc >=100) %>% 
  dplyr::mutate(pop_WP_100 = sum(pop_WP_2010, na.rm=T), .keep = "none") %>% 
  glimpse -> pop_WP_100

c(pop_WP_10[1,], pop_WP_20[1,], pop_WP_30[1,], pop_WP_40[1,], pop_WP_50[1,], pop_WP_60[1,],
      pop_WP_70[1,], pop_WP_80[1,], pop_WP_90[1,], pop_WP_100[1,]) -> pop_WP
cbind(pop_WP, threshold)-> pop_thresh_WP
#colnames(pop_thresh_WP) <- c("pop_WP", "nvc")
as_tibble(pop_thresh_WP) -> pop_thresh_WP

pop_thresh %>% 
  #as_tibble() %>% 
  left_join(pop_thresh_LS, by= "threshold") %>%
  left_join(pop_thresh_WP, by = "threshold") %>% 
  glimpse -> pop_thresh_all

ggplot(data = pop_thresh_all)+
  geom_point(aes(x = threshold, y = pop_ibge_2010))+
  geom_point(aes(x = threshold, y = pop_LS_2010), color="red")+
  geom_point(aes(x = threshold, y = pop_WP_2010), color="green")+
  ylab("forest proximate people") +
  xlab("forest threshold") +
  labs(title = "2010") +
  scale_y_continuous(breaks = c(0, 1250000, 2500000, 3750000,5000000, 6250000, 7500000, 8750000, 10000000))+
  theme_classic()-> fpp_thresholds_2010

data.frame(table_analysis6) %>% 
  #select(-geometry) %>% 
  filter(pland_nvc >=100) %>% 
  dplyr::mutate(pop_LS_100 = sum(pop_LS_2000, na.rm=T), .keep = "none") %>% 
  glimpse -> pop_LS_2000_100

c(pop_LS_2000_10[1,], pop_LS_2000_20[1,], pop_LS_2000_30[1,], pop_LS_2000_40[1,], pop_LS_2000_50[1,], pop_LS_2000_60[1,],
  pop_LS_2000_70[1,], pop_LS_2000_80[1,], pop_LS_2000_90[1,], pop_LS_2000_100[1,]) -> pop_LS_2000
cbind(pop_LS_2000, threshold)-> pop_LS_2000_thresh
#colnames(pop_thresh_LS) <- c("pop_LS", "nvc")
as_tibble(pop_LS_2000_thresh) -> pop_LS_2000_thresh

data.frame(table_analysis6) %>% 
  #select(-geometry) %>% 
  filter(pland_nvc >=100) %>% 
  dplyr::mutate(pop_WP_100 = sum(pop_WP_2000, na.rm=T), .keep = "none") %>% 
  glimpse -> pop_WP_2000_100

c(pop_WP_2000_10[1,], pop_WP_2000_20[1,], pop_WP_2000_30[1,], pop_WP_2000_40[1,], pop_WP_2000_50[1,], pop_WP_2000_60[1,],
  pop_WP_2000_70[1,], pop_WP_2000_80[1,], pop_WP_2000_90[1,], pop_WP_2000_100[1,]) -> pop_WP_2000
cbind(pop_WP_2000, threshold)-> pop_WP_2000_thresh
#colnames(pop_thresh_LS) <- c("pop_LS", "nvc")
as_tibble(pop_WP_2000_thresh) -> pop_WP_2000_thresh

pop_LS_2000_thresh %>% 
  left_join(pop_WP_2000_thresh, by = "threshold") %>% 
  right_join(y=pop_thresh_all, by = "threshold") %>% 
  rename(pop_ibge_2010 = pop,
         dom_ibge_2010 = dom,
         pop_LS_2010 = pop_LS,
         pop_WP_2010 = pop_WP) %>% 
  select(threshold, pop_ibge_2010, dom_ibge_2010, pop_LS_2000, pop_WP_2000, pop_LS_2010, pop_WP_2010) %>% 
  glimpse -> pop_thresh_all

ggplot(data = pop_thresh_all)+
  geom_point(aes(x = threshold, y = pop_LS_2000), color="red")+
  geom_point(aes(x = threshold, y = pop_WP_2000), color="green")+
  ylab("forest proximate people") +
  xlab("forest threshold") +
  labs(title = "2000") +
  scale_y_continuous(breaks = c(0, 1250000, 2500000, 3750000,5000000, 6250000, 7500000, 8750000, 10000000))+
  theme_classic()-> fpp_thresholds_2000
ggplot(data = pop_thresh_all)+
  geom_point(aes(x = threshold, y = pop_LS_2010), color="red")+
  geom_point(aes(x = threshold, y = pop_WP_2010), color="green")+
  ylab("forest proximate people") +
  xlab("forest threshold") +
  labs(title = "2010") +
  scale_y_continuous(breaks = c(0, 1250000, 2500000, 3750000,5000000, 6250000, 7500000, 8750000, 10000000))+
  theme_classic()-> fpp_thresholds_2010
ggpubr::ggarrange(fpp_thresholds_2000, fpp_thresholds_2010)

###people and forest change----
table_analysis4 %>% 
  select(id_buff, code_muni) %>% 
  left_join(y = table_analysis5, by= "id_buff") %>% 
  glimpse -> table_analysis5

buff_5km_pop_rural_WP_2010$id_buff<- as.integer(buff_5km_pop_rural_WP_2010$id_buff)
buff_5km_pop_rural_WP_2000$id_buff<- as.integer(buff_5km_pop_rural_WP_2000$id_buff)

read_xlsx(path = "/home/alenc/Documents/Doutorado/tese/cap1/forest-develop/dbcap1_clean.xlsx") %>% 
  select(code_muni, nvcPerc_00, nvcPerc_10) %>% 
  right_join(y = table_analysis5) %>% 
  left_join(y= select(buff_5km_pop_rural_WP_2000, id_buff, pop_sum), by = "id_buff") %>% 
  rename(pop_sum_2000 = pop_sum) %>% 
  left_join(y= select(buff_5km_pop_rural_WP_2010, id_buff, pop_sum), by = "id_buff") %>% 
  rename(pop_sum_2010 = pop_sum) %>%
  mutate(vari_pop = pop_sum_2010 - pop_sum_2000,
         vari_pop_perc = (pop_sum_2010/pop_sum_2000)*100-100,
         vari_nvc = nvcPerc_10 - nvcPerc_00) %>%
  glimpse -> table_analysis7

hist(table_analysis7$vari_pop)

table_analysis7 %>% 
#filter(nvcPerc_00 >=50) %>% 
  group_by(code_muni,vari_nvc) %>% 
  summarize_at(vars(vari_pop_perc), list(mean)) %>% 
  mutate(cat_quad=ifelse(vari_nvc<0 & vari_pop_perc<0,"PP",
                         ifelse(vari_nvc<0 & vari_pop_perc>0,"PG",
                               ifelse(vari_nvc>0 & vari_pop_perc>0,"GG",
                                "GP")))) -> table_variation
ggplot()+
  geom_point(aes(x=vari_nvc, y=vari_pop_perc, color = cat_quad, alpha=0.1))+
  ylim(-100,100)
  
table_analysis7

table_analysis7 %>% 
  left_join(y = table_variation, by="code_muni") %>% 
  group_by(code_muni, cat_quad) %>% 
  na.omit(.$prop_dom_expov) %>% 
  mutate(sd_cat = ifelse(test = vari_nvc.x > 2.964099, yes = "true", no = ifelse(vari_nvc.x < -5.291971,
                                                                               yes = "true",
                                                                               no = "foda_se"))) %>% 
  dplyr::summarise(prop_dom_expov_mean = mean(prop_dom_expov),
                   #sd_nvc = sd_nvc,
                   sd_cat= sd_cat) %>% 
  glimpse %>% 
  filter(sd_cat == "true") -> table_summary_pov
  #lm(prop_dom_expov_mean~cat_quad, data=.) %>% 
  #summary()
  
  ggplot()+
  geom_boxplot(aes(x=cat_quad, y=prop_dom_expov_mean))
  
  library(geobr)
read_municipality()->mun_br
read_biomes()-> biomas
read_state()-> states
states %>% 
  filter(abbrev_state == "PI" |
         abbrev_state == "CE" |
           abbrev_state == "RN"|
           abbrev_state =="PB" |
           abbrev_state =="PE" |
           abbrev_state =="AL" |
           abbrev_state =="SE" |
           abbrev_state =="BA" |
           abbrev_state =="MG" ) %>% 
  glimpse-> ne_sate

sd(table_analysis7$vari_nvc, na.rm = T)
mean(table_analysis7$vari_nvc, na.rm = T) +sd(table_analysis7$vari_nvc, na.rm = T) -> sd_positive
mean(table_analysis7$vari_nvc, na.rm = T) - sd(table_analysis7$vari_nvc, na.rm = T) -> sd_negative

mun_br %>% 
  select(code_muni, geom) %>% 
  right_join(y= table_summary_pov, by = "code_muni") %>% 
  glimpse %>% 
  ggplot()+
  geom_sf(data = ne_sate$geom)+
  geom_sf(data= table_summary_pov$geom, aes(fill =cat_quad))
  
table_variation %>%
  mutate(code_muni = as.factor(code_muni)) %>% 
  left_join(y = table_agrifamiliar, by="code_muni") %>%
  left_join(y = table_agrifamiliar_area, by="code_muni") %>%
  mutate(code_muni = as.factor(code_muni),
         familyAgri_area = as.double(familyAgri_area),
         area_familiar_perc = (familyAgri_area/area_estab)*100) %>% 
  na.omit() %>% 
  glimpse ->table_vari2

table_vari2 %>%   
ggplot(aes(x=vari_nvc, y=area_familiar_perc, color = cat_quad))+
  geom_point()+
  #geom_smooth(method = "loess")+
  geom_hline(yintercept = 49.33959)

table_variation %>%
  mutate(code_muni = as.factor(code_muni)) %>% 
  left_join(y = table_agrifamiliar, by="code_muni") %>%
  left_join(y = table_agrifamiliar_area, by="code_muni") %>%
  mutate(code_muni = as.factor(code_muni),
         familyAgri_area = as.double(familyAgri_area),
         area_familiar_perc = (familyAgri_area/area_estab)*100) %>% 
  na.omit() %>% 
  left_join(y= select(mun_br,code_muni, geom), by = "code_muni") %>% 
  glimpse ->table_vari2
  
ggplot()+
  #geom_sf(data = ne_sate$geom)+
  geom_sf(data = table_vari2$geom, aes(fill = table_vari2$area_familiar_perc))+
scale_fill_viridis_c()

table_vari2 %>% 
  left_join(y= table_agrifamiliar_2017, by="code_muni") %>% 
  left_join(y= table_agrifamiliar_area_2017, by="code_muni") %>% 
  mutate(area_familiar_perc_17 = (area_agri_familiar_17/area_estab_17)*100,
         vari_perc_area_familiar = area_familiar_perc_17 - area_familiar_perc) %>% 
  na.omit() %>% 
  glimpse -> table_vari3

ggtern(data=table_vari3, aes(x=vari_nvc, y=vari_pop_perc, z=vari_perc_area_familiar)) +
  #scale_T_continuous(limits = c(-100,100))+
  #scale_L_continuous(limits = c(-100,100))+
  #scale_R_continuous(limits = c(-100,100))+
  limit_tern(10,10,10)+
  geom_point()

  pca<-prcomp(table_vari3[,c(2,3,24)])
  summary(pca)
  pca$x
  
  biplot(pca)

  table_summary_pov %>% 
    ungroup %>% 
    distinct(code_muni, .keep_all = T) %>% 
    glimpse-> table_summary_pov
  
  table_analysis7 %>% 
    group_by(code_muni) %>% 
    summarise(shdi_NA_mean = mean(shdi_NA),
              vari_pop_perc = vari_pop_perc,
              vari_nvc = vari_nvc) %>% 
    mutate(code_muni = as.factor(code_muni)) %>% 
    right_join(y= table_vari3, by = "code_muni") %>% 
    distinct(code_muni, .keep_all = T) %>% 
    #left_join(y = table_summary_pov, by="code_muni") %>% 
    glimpse -> table_analysis8

  table_summary_pov %>% 
    left_join(table_analysis8, by = "code_muni") %>% 
    filter(code_muni != 2100907) %>% 
    glimpse %>% 
    #lm(data = ., shdi_NA_mean ~ cat_quad.x) %>% 
    #summary()
  
  ggplot(aes(x=cat_quad.x, y = shdi_NA_mean))+
    geom_boxplot()
  