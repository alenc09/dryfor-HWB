#Script para calcular curva de acumulo de FPP (e mudança) por limiar de cobertura florestal das paisagens

#library----
library(here)
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(cowplot)
library(scales)

#data----
#load files----
read.csv(file = here("data/buff_lsm_2006.csv"), sep = ",", dec=".")-> buff_lsm_2006
read.csv(file = here("data/buff_lsm_2017.csv"))-> buff_lsm_2017
st_read("data/buff_5km_pop_rural_WP_2006.shp")-> pop_rural_2006
st_read("data/buff_5km_pop_rural_WP_2017.shp")-> pop_rural_2017
read.csv(file = here("data/tabela_geral.csv"))-> tab_geral

##organizing buffer land cover data----
buff_lsm_2006 %>% 
  select(plot_id, metric, class, value) %>% 
  filter(plot_id != 1606) %>% 
  pivot_wider(id_cols = plot_id,
              names_sep = "_",
              names_from = c(metric, class),
              values_from = value
  ) %>% 
  select(plot_id,shdi_NA, pland_3, pland_4, pland_5, pland_9, pland_11, pland_12, 
         pland_13, pland_15, pland_20, pland_21, pland_23, pland_24, pland_25,
         pland_29, pland_30, pland_31, pland_32, pland_33, pland_39, pland_41,
         pland_46, pland_48, ca_3, ca_4, ca_5, ca_9, ca_11, ca_12, ca_13, ca_15,
         ca_20, ca_21, ca_23, ca_24, ca_25, ca_29, ca_30, ca_31, ca_32, ca_33,
         ca_39, ca_41, ca_46, ca_48) %>%
  replace(is.na(.), 0) %>% 
  dplyr::rename(pland_forest = pland_3,
         pland_savanna = pland_4,
         pland_mangrove = pland_5,
         pland_Fplantation = pland_9,
         pland_wetland = pland_11,
         pland_grass = pland_12,
         pland_otherVeg = pland_13,
         pland_pasture = pland_15,
         pland_sugar = pland_20,
         pland_mosaicAP = pland_21,
         pland_sand = pland_23,
         pland_urban = pland_24,
         pland_otherNveg = pland_25,
         pland_rocky = pland_29,
         pland_mine = pland_30,
         pland_aquacult = pland_31,
         pland_salt = pland_32,
         pland_water = pland_33,
         pland_soy = pland_39,
         pland_otherTcrop = pland_41,
         pland_coffe = pland_46,
         pland_otherPcrop = pland_48,
         ca_forest = ca_3,
         ca_savanna = ca_4,
         ca_mangrove = ca_5,
         ca_Fplantation = ca_9,
         ca_wetland = ca_11,
         ca_grass = ca_12,
         ca_otherVeg = ca_13,
         ca_pasture = ca_15,
         ca_sugar = ca_20,
         ca_mosaicAP = ca_21,
         ca_sand = ca_23,
         ca_urban = ca_24,
         ca_otherNveg = ca_25,
         ca_rocky = ca_29,
         ca_mine = ca_30,
         ca_aquacult = ca_31,
         ca_salt = ca_32,
         ca_water = ca_33,
         ca_soy = ca_39,
         ca_otherTcrop = ca_41,
         ca_coffe = ca_46,
         ca_otherPcrop = ca_48
  ) -> tab_forest_06

buff_lsm_2017 %>% 
  select(plot_id, metric, class, value) %>% 
  filter(plot_id != 1606) %>% 
  pivot_wider(id_cols = plot_id,
              names_sep = "_",
              names_from = c(metric, class),
              values_from = value) %>%
  select(plot_id,shdi_NA, pland_3, pland_4, pland_5, pland_9, pland_11, pland_12, 
         pland_13, pland_15, pland_20, pland_21, pland_23, pland_24, pland_25,
         pland_29, pland_30, pland_31, pland_32, pland_33, pland_39, pland_41,
         pland_46, pland_48, ca_3, ca_4, ca_5, ca_9, ca_11, ca_12, ca_13, ca_15,
         ca_20, ca_21, ca_23, ca_24, ca_25, ca_29, ca_30, ca_31, ca_32, ca_33,
         ca_39, ca_41, ca_46, ca_48) %>%
  replace(is.na(.), 0) %>% 
  dplyr::rename(pland_forest = pland_3,
         pland_savanna = pland_4,
         pland_mangrove = pland_5,
         pland_Fplantation = pland_9,
         pland_wetland = pland_11,
         pland_grass = pland_12,
         pland_otherVeg = pland_13,
         pland_pasture = pland_15,
         pland_sugar = pland_20,
         pland_mosaicAP = pland_21,
         pland_sand = pland_23,
         pland_urban = pland_24,
         pland_otherNveg = pland_25,
         pland_rocky = pland_29,
         pland_mine = pland_30,
         pland_aquacult = pland_31,
         pland_salt = pland_32,
         pland_water = pland_33,
         pland_soy = pland_39,
         pland_otherTcrop = pland_41,
         pland_coffe = pland_46,
         pland_otherPcrop = pland_48,
         ca_forest = ca_3,
         ca_savanna = ca_4,
         ca_mangrove = ca_5,
         ca_Fplantation = ca_9,
         ca_wetland = ca_11,
         ca_grass = ca_12,
         ca_otherVeg = ca_13,
         ca_pasture = ca_15,
         ca_sugar = ca_20,
         ca_mosaicAP = ca_21,
         ca_sand = ca_23,
         ca_urban = ca_24,
         ca_otherNveg = ca_25,
         ca_rocky = ca_29,
         ca_mine = ca_30,
         ca_aquacult = ca_31,
         ca_salt = ca_32,
         ca_water = ca_33,
         ca_soy = ca_39,
         ca_otherTcrop = ca_41,
         ca_coffe = ca_46,
         ca_otherPcrop = ca_48
  )  -> tab_forest_17

tab_forest_06 %>% 
  select(plot_id, pland_forest, pland_savanna, pland_mangrove) %>% 
  left_join(y = select(tab_forest_17, plot_id,pland_forest, pland_savanna, pland_mangrove), by = "plot_id" ) %>% 
  mutate(pland_nvc_06 = pland_forest.x + pland_savanna.x + pland_mangrove.x,
         pland_nvc_17 = pland_forest.y + pland_savanna.y + pland_mangrove.y) %>% 
  glimpse -> tab_obj1

##population data----
pop_rural_2006 %>% 
  select(id_buff, popsum) %>% 
  dplyr::rename(buff_id = id_buff,
         pop_rural_WP_06 = popsum,
         geom_buff = geometry) %>% 
  left_join(y = tibble(pop_rural_2017), by = c("buff_id" = "id_buff")) %>% 
  select(buff_id, pop_rural_WP_06, pop_sum, geom_buff) %>% 
  dplyr::rename(pop_rural_WP_17 = pop_sum) %>% 
  glimpse -> table_pop

tab_obj1 %>% 
  mutate(plot_id = as.character(plot_id)) %>% 
  left_join(y = table_pop, by = c("plot_id" = "buff_id")) %>% 
  glimpse ->tab_obj1

##cumulative threshold----
seq(from = 10, to = 100, by = 10) -> perc_thresh

c() -> list_fpp06
for(i in perc_thresh){
  tab_geral %>%
    filter(pland_nvc_06 >= i) %>%
    summarise(fpp = sum(pop_rural_WP_06)) %>%
    glimpse -> list_fpp06[i]
} 

list_fpp06 %>% 
  unlist %>% 
  glimpse -> fpp_06

c() -> list_fpp17
for(i in perc_thresh){
  tab_geral %>%
    filter(pland_nvc_17 >= i) %>%
    summarise(fpp = sum(pop_rural_WP_17)) %>%
    glimpse -> list_fpp17[i]
}

list_fpp17 %>% 
  unlist %>% 
  glimpse -> fpp_17

as.data.frame(cbind(perc_thresh, fpp_06, fpp_17))-> tab_fpp

##change per threshold----
tab_fpp %>% 
  mutate(fpp_change = fpp_17 - fpp_06) %>% 
  glimpse -> tab_fpp_change

##summary table----
tab_geral %>% 
  filter(pland_nvc_17 >= 100) %>%
  summarise(fpp_06 = sum(pop_rural_WP_06),
            fpp_17 = sum(pop_rural_WP_17),
            abs_change = fpp_17 - fpp_06,
            mean_fpp_change = mean(pop_rural_WP_17 - pop_rural_WP_06),
            sd_fpp_change = sd(pop_rural_WP_17 - pop_rural_WP_06),
            n = n(),
            mean_nvc = mean(pland_nvc_17),
            sd_nvc = sd(pland_nvc_17)
            ) %>%
  glimpse

#Figures----
## Cumulative curve extrapolated to entire biome----
tab_fpp %>%
  mutate(fpp_06_extra = floor((fpp_06*100)/53.54764),
         fpp_17_extra = floor((fpp_17*100)/53.54764)) %>% 
  #glimpse
  ggplot(aes(x = perc_thresh))+
  geom_segment(aes(x = 0, xend = 20, y = 8951887, yend = 8951887), linetype = "dashed", color = "lightgrey")+
  geom_segment(aes(x = 20, xend = 20, y = 0, yend = 8951887), linetype = "dashed", color = "lightgrey")+
  geom_segment(aes(x = 0, xend = 50, y = 5396928, yend = 5396928), linetype = "dashed", color = "lightgrey")+
  geom_segment(aes(x = 50, xend = 50, y = 0, yend = 5396928), linetype = "dashed", color = "lightgrey")+
  geom_segment(aes(x = 0, xend = 70, y = 3114681, yend = 3114681), linetype = "dashed", color = "lightgrey")+
  geom_segment(aes(x = 70, xend = 70, y = 0, yend = 3114681), linetype = "dashed", color = "lightgrey")+
  geom_point(aes(y = fpp_06_extra, color = "fpp_06"))+
  geom_line(aes(y = fpp_06_extra,color = "fpp_06"))+
  geom_point(aes(y = fpp_17_extra, color = "fpp_17"))+
  geom_line(aes(y = fpp_17_extra, color = "fpp_17"))+
  scale_x_continuous(breaks = perc_thresh, expand = expansion(add = 1))+
  scale_y_continuous(labels = c(0, 2, 3.1, 4, 5.3, 6, 8, 8.9, 10),
                     breaks = c(0, 2000000, 3114681, 4000000, 5396928, 6000000, 8000000, 8951887, 10000000),
                     expand = expansion(mult = 0.01))+
 scale_color_manual(values = c("#ffa600", "#5c3811"), name = "Year", labels = c("2006", "2017"))+
  labs(x = "Forest cover threshold (%)", y = "Number of FPP (million)", color = "Legend")+
  theme_classic()+
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.position = c(0.9,0.6),
        plot.margin = margin(6, 30, 6, 6))  -> fig.fpp

#ggsave(plot = fig.fpp, filename = "img/fig_fpp.jpg", dpi = 300)

## changes per category of forest cover ---- 
options(scipen=10000)
tab_fpp %>%
  mutate(fpp_06_extra = floor((fpp_06*100)/53.54764),
         fpp_17_extra = floor((fpp_17*100)/53.54764),
         fpp_change_extra = fpp_17_extra - fpp_06_extra) %>% 
  #glimpse
  ggplot(aes(x = as.factor(perc_thresh), y = fpp_change_extra)) +
  geom_bar(stat = "identity", fill = "#018571")+
  scale_y_continuous(labels = comma, expand = c(0, 0),
                     breaks = c(200000, 384658, 568894, 600000, 733505, 800000))+
  geom_segment(aes(x = 0, xend = 2, y = 733505, yend = 733505), linetype = "dashed", color = "lightgrey")+
  geom_segment(aes(x = 0, xend = 5, y = 568894, yend = 568894), linetype = "dashed", color = "lightgrey")+
  geom_segment(aes(x = 0, xend = 7, y = 384658, yend = 384658), linetype = "dashed", color = "lightgrey")+
  labs(x = " Forest cover threshold (%)", y = "Absolute change")+
  theme_classic() -> fig.fpp_cat_change

tab_geral %>% 
  filter(vari_perc_pop_rural <200) %>% 
  ggplot()+
  geom_histogram(aes(x=vari_perc_pop_rural), bins = 100)+
  geom_vline(xintercept = mean(tab_geral$vari_perc_pop_rural, na.rm = T), color = "red")+
  geom_vline(xintercept = median(tab_geral$vari_perc_pop_rural, na.rm = T), color = "red", linetype = "dashed")+
  scale_x_continuous(limits = c(-100, 200), name = "Change in FPP number per landscape (%)")+
  scale_y_continuous(expand = c(0, 0), name = ("Frequency"))+
  theme_classic() ->supp_fig_3

## panel fpp change----
plot_grid(fig.fpp, fig.fpp_cat_change, labels = "auto") -> fig.fpp_change

# ggsave(plot = fig.fpp_change, filename = here("img/fig.fpp_change.jpg"), width = 8, bg = "White")
# ggsave(plot = supp_fig_3, filename = here("img/supp_fig_3.jpg"))

## FPP per forest cover category----
tab_geral %>% 
  select(buff_id, pland_nvc_06, pland_nvc_17, pop_rural_WP_06, pop_rural_WP_17) %>% 
  rename(fpp_06 = pop_rural_WP_06,
         fpp_17 = pop_rural_WP_17) %>% 
  mutate(catFc_06 = if_else(0 < pland_nvc_06 & pland_nvc_06 < 33,
                             true = "0-33",
                             false = if_else(33 <= pland_nvc_06 & pland_nvc_06 < 66,
                                             true = "33-66",
                                             false = if_else(66 < pland_nvc_06 & pland_nvc_06 <= 100,
                                                             true = "66-100",
                                                             false = NA))),
         catFc_17 = if_else(0 < pland_nvc_17 & pland_nvc_17 < 33,
                             true = "0-33",
                             false = if_else(33 <= pland_nvc_17 & pland_nvc_17 < 66,
                                             true = "33-66",
                                             false = if_else(66 < pland_nvc_17 & pland_nvc_17 <= 100,
                                                             true = "66-100",
                                                             false = NA)))) %>% 
  pivot_longer(cols = -1:-3,
               names_to = c(".value", "year"),
               names_sep = "_" ,
               values_to = c("fpp", "cat_fc"),
               names_transform = list(year = as.factor),
               values_transform = list(catFc = as.factor))%>% 
  group_by(year, catFc) %>% 
  summarise(fpp_total = sum(fpp)) %>% 
  glimpse %>% 
ggplot()+
  geom_col(aes(x=catFc, y = fpp_total, fill = year), position = "dodge")+
  scale_x_discrete(name = "Forest cover (%)")+
  scale_y_continuous(name = "Forest-proximate population size", expand = c(0,0), labels = comma)+
  scale_fill_manual(values = c("#ffa600", "#5c3811"), name = "Year", labels = c("2006", "2017"))+
  theme_classic() -> fpp_catFC

ggsave(plot = fpp_catFC, filename = here("img/fig1_alter.jpg"), dpi = 3000)
