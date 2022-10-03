# Thu Jul 14 13:50:46 2022 ------------------------------
#Script para montar figura do número de FPP por limiar de cobertura florestal das paisagens

#library----
library(here)
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)

#data----
#load files----
read.csv(file = here("buff_lsm_2006.csv"), sep = ",", dec=".")-> buff_lsm_2006
read.csv(file = here("buff_lsm_2017.csv"))-> buff_lsm_2017
st_read("data/buff_5km_pop_rural_WP_2006.shp")-> pop_rural_2006
st_read("data/buff_5km_pop_rural_WP_2017.shp")-> pop_rural_2017

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

##table for figure----
c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)-> perc_thresh

for(i in perc_thresh){
  tab_obj1 %>%
    filter(pland_nvc_06 > i) %>%
    summarise(fpp = sum(pop_rural_WP_06)) %>%
    glimpse
}
c(4853511, 4305189, 3644584, 3037533, 2428882, 1893891, 1312354, 794606.3, 292520, 5.341603) -> fpp_06

for(i in perc_thresh){
  tab_obj1 %>%
    filter(pland_nvc_17 > i) %>%
    summarise(fpp = sum(pop_rural_WP_17)) %>%
    glimpse
}
c(5239184, 4646104, 3976349, 3323920, 2697328, 2079576, 1500688, 929346.9, 339360.2, 3.231455) -> fpp_17

as.data.frame(cbind(perc_thresh, fpp_06, fpp_17))-> tab_fpp


#Figure----
c("fpp_06" = "#ffa600", "fpp_17" = "#5c3811") -> colors

tab_fpp %>% 
  ggplot(aes(x = perc_thresh))+
  geom_point(aes(y = fpp_06, color = "fpp_06"))+
  geom_line(aes(y = fpp_06,color = "fpp_06"))+
  geom_point(aes(y = fpp_17, color = "fpp_17"))+
  geom_line(aes(y = fpp_17, color = "fpp_17"))+
  scale_x_continuous(breaks = perc_thresh)+
  scale_y_continuous(labels = c(0,1,2,3,4,5))+
  scale_color_manual(values = colors, name = "Year", labels = c("2006", "2017"))+
  labs(x = "Forest cover threshold (%)", y = "Number of FPP (million)", color = "Legend")+
  theme_classic()+
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.position = c(0.8,0.8)) -> fig.fpp

ggsave(plot = fig.fpp, filename = "img/fig_fpp.jpg", dpi = 300)
