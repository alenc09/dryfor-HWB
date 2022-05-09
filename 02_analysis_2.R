# Thu Apr 07 09:26:54 2022 ------------------------------
#script para novas análises

#library----
library(dplyr)
library(ggplot2)
library(sf)
library(ggpubr)

#data----
read.csv(file = here("tabela_geral.csv"))-> tab_1

#análises exploratorias----
##forested landscapes----
tab_forest_06 %>% 
  select(plot_id, pland_forest, pland_savanna, pland_grass, pland_rocky,
  pland_mangrove, pland_wetland, pland_otherVeg, pland_salt) %>% 
  left_join(y = select(tab_forest_17, plot_id,pland_forest, pland_savanna, pland_grass, pland_rocky,
                       pland_mangrove, pland_wetland, pland_otherVeg, pland_salt), by = "plot_id" ) %>% 
  mutate(pland_nvc_06 = pland_forest.x + pland_savanna.x + pland_grass.x + pland_rocky.x +
         pland_mangrove.x + pland_wetland.x + pland_otherVeg.x + pland_salt.x,
         pland_nvc_17 = pland_forest.y + pland_savanna.y + pland_grass.y + pland_rocky.y +
           pland_mangrove.y + pland_wetland.y + pland_otherVeg.y + pland_salt.y) %>% 
  glimpse -> tab_obj1

# tab_obj1 %>% 
#   filter(pland_nvc_17 == 100) %>% Change the filter limiar to get the number of landscapes with 10%...20%...30%...
#   count() %>% 
#   glimpse

c(855, 562, 424, 318, 245, 169, 101, 55, 26, 0)-> nland_forest_06
c(849, 549, 408, 301, 227, 160, 95, 54, 26, 0)-> nland_forest_17
c(6058, 5748, 5313, 4806, 4244, 3621, 2886, 2003, 1051, 21)-> nland_nvc_06
c(6053, 5752, 5329, 4850, 4337, 3697, 2967, 2128, 1093, 16)-> nland_nvc_17
c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)-> perc_thresh
as.data.frame(cbind(perc_thresh, nland_forest_06, nland_forest_17, nland_nvc_06, nland_nvc_17)) ->tab_nland

ggplot()+
  geom_point(data = tab_nland, aes(x = perc_thresh, y = nland_forest_17), color = "DarkGreen")+
  geom_path(data = tab_nland, aes(x = perc_thresh, y = nland_forest_17),
            linetype = "solid", color = "DarkGreen")+
  geom_point(data = tab_nland, aes(x = perc_thresh, y = nland_nvc_17), color = "brown")+
  geom_path(data = tab_nland, aes(x = perc_thresh, y = nland_nvc_17),
            linetype = "solid", color = "brown")+
  scale_x_continuous(breaks = perc_thresh)+
  scale_y_continuous(breaks = c (1000, 2000, 3000, 4000, 5000, 6000))+
  xlab("Land cover threshold (%)")+
  ylab("Number of landscapes")+
  theme_classic() -> fig.forest_nvc
  
ggsave(plot = fig.forest_nvc, filename = here("img/fig.forest_nvc.jpg"))  

## fpp and nvc(forest) threshold----
tab_obj1 %>% 
  mutate(plot_id = as.character(plot_id)) %>% 
  left_join(y = table_pop, by = c("plot_id" = "buff_id")) %>% 
  glimpse ->tab_obj1



for(i in perc_thresh){
tab_obj1 %>% 
  filter(pland_forest.x > i) %>% 
  summarise(fpp = sum(pop_rural_WP_06)) %>% 
    glimpse 
} 
c(914873.5, 620139.9, 452889.7,334987.9,  246105.2, 169683.1, 82448.42,40635.99, 25309.32, 0) -> fpp_forest_06

for(i in perc_thresh){
  tab_obj1 %>% 
    filter(pland_forest.y > i) %>% 
    summarise(fpp = sum(pop_rural_WP_17)) %>% 
    glimpse 
}
c(1069881, 697544.4, 514603.4, 365129.7, 271340.4, 195505.4, 95731.14, 51285.22, 27593.96, 0) -> fpp_forest_17

for(i in perc_thresh){
  tab_obj1 %>% 
    filter(pland_nvc_06 > i) %>% 
    summarise(fpp = sum(pop_rural_WP_06)) %>% 
    glimpse 
}
c(4906978, 4400750, 3810168, 3172125, 2585299, 2044387, 1461863, 876472, 335107.8, 8.438939) -> fpp_nvc_06

for(i in perc_thresh){
  tab_obj1 %>% 
    filter(pland_nvc_17 > i) %>% 
    summarise(fpp = sum(pop_rural_WP_17)) %>% 
    glimpse 
}
c(5304623, 4793525, 4134340, 3482737, 2889928, 2294548, 1667838, 1063042, 398354.5, 48.79978) -> fpp_nvc_17

## pop and nvc change----
ggplot(data = na.omit(tab_1), aes(x = vari_perc_nvc, y = vari_perc_pop_rural, color = cat_change))+
  geom_point()+
  ylim(-100,200)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  labs(title = "all buffers")+
  scale_color_discrete(labels = c("Off-farm livelihoods",
                                  "Land abandonment",
                                  "small-holders settlement",
                                  "Disposession"))+
  theme_classic() -> pop_nvc_all

tab_1 %>%  
  filter(nvc_outlier == "outlier" & pop_outlier == "outlier") %>%
ggplot(aes(x = vari_perc_nvc, y = vari_perc_pop_rural, color = cat_change))+
  geom_point()+
  ylim(-100,200)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  labs(title = "'outliers'")+
  scale_color_discrete(labels = c("Off-farm livelihoods",
                                  "Land abandonment",
                                  "small-holders settlement",
                                  "Disposession"))+
  theme_classic() -> pop_nvc_sd


##family agriculture----
tab_1 %>%  
  tibble() %>% 
  group_by(code_muni) %>% 
  summarise( mean_vari_perc_nvc = mean(vari_perc_nvc),
             vari_perc_estab_agrifam = mean(vari_perc_estab_agrifam)
             #cat_change = cat_change
            ) %>% 
  #glimpse
  #filter(nvc_outlier == "outlier" & pop_outlier == "outlier") %>%
  ggplot(aes(x = mean_vari_perc_nvc, y = vari_perc_estab_agrifam
             #, color = cat_change
             ))+
  geom_point(alpha = 0.2)+
  #geom_smooth(method = "lm")+
  ylim(-100,200)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  #labs(title = "nvc and agrifam relation in 'outliers' ")+
  theme_classic() -> agrifam_estab_nvc_all

tab_1 %>%  
  tibble() %>% 
  group_by(code_muni) %>% 
  summarise( mean_vari_perc_nvc = mean(vari_perc_nvc),
             vari_perc_area_agrifam = mean(vari_perc_area_agrifam)
             #cat_change = cat_change
  ) %>% 
  #glimpse
  #filter(nvc_outlier == "outlier" & pop_outlier == "outlier") %>%
  ggplot(aes(x = mean_vari_perc_nvc, y = vari_perc_area_agrifam
             #, color = cat_change
  ))+
  geom_point(alpha = 0.2)+
  #geom_smooth(method = "lm")+
  ylim(-100,200)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  #labs(title = "nvc and agrifam relation in 'outliers' ")+
  theme_classic() -> agrifam_area_nvc_all

tab_1 %>%  
  group_by(code_muni) %>% 
  summarise(mean_pop_change = mean(vari_perc_pop_rural),
            mean_nvc_change = mean(vari_perc_nvc),
            vari_perc_area_agrifam = mean(vari_perc_area_agrifam)) %>% 
  mutate(cat_change = if_else(condition = mean_nvc_change<0 & mean_pop_change<0,
                              true = "PP",
                              false = if_else(condition =  mean_nvc_change<0 & mean_pop_change>0,
                                              true = "PG",
                                              false = if_else(mean_nvc_change>0 & mean_pop_change>0,
                                                              true = "GG",
                                                              false = "GP")))) %>%
  na.omit() %>% 
  #glimpse
  ggplot(aes(x = cat_change, y = vari_perc_area_agrifam, color = cat_change))+
  geom_boxplot()+
  #geom_smooth(method = "lm")+
  #ylim(-100,200)+
  geom_hline(yintercept = 0, type = "dot")+
  #geom_vline(xintercept = 0)+
  #labs(title = "pop and agrifam variation in 'outliers' ")+
  scale_color_discrete(labels = c("Off-farm livelihoods",
                                  "Land abandonment",
                                  "small-holders settlement",
                                  "Disposession"))+
  theme_classic() -> agrifam_area_cat_change

tab_1 %>%  
  group_by(code_muni) %>% 
  summarise(mean_pop_change = mean(vari_perc_pop_rural),
            mean_nvc_change = mean(vari_perc_nvc),
            vari_perc_estab_agrifam= mean(vari_perc_estab_agrifam)) %>% 
  mutate(cat_change = if_else(condition = mean_nvc_change<0 & mean_pop_change<0,
                              true = "PP",
                              false = if_else(condition =  mean_nvc_change<0 & mean_pop_change>0,
                                              true = "PG",
                                              false = if_else(mean_nvc_change>0 & mean_pop_change>0,
                                                              true = "GG",
                                                              false = "GP")))) %>%
  na.omit() %>% 
  #glimpse
  ggplot(aes(x = cat_change, y = vari_perc_estab_agrifam, color = cat_change))+
  geom_boxplot()+
  #geom_smooth(method = "lm")+
  #ylim(-100,200)+
  geom_hline(yintercept = 0, type = "dot")+
  #geom_vline(xintercept = 0)+
  #labs(title = "pop and agrifam variation in 'outliers' ")+
  scale_color_discrete(labels = c("Off-farm livelihoods",
                                  "Land abandonment",
                                  "small-holders settlement",
                                  "Disposession"))+
  theme_classic()-> agrifam_estab_cat_change

##landscape diversity----
tab_1 %>%  
  #filter(nvc_outlier == "outlier" & pop_outlier == "outlier") %>%
  filter(cat_change != "estable") %>% 
  ggplot(aes(x = vari_perc_pop_rural, y = vari_shdi
             , color = cat_change
             ))+
  geom_point()+
  geom_smooth(method = "lm")+
  xlim(-100,200)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  #labs(title = "pop and landscape diversity variation in 'outliers' ")+
  theme_classic() -> shdi_pop_all

##distance to municipality seat----
tab_1 %>%  
  
#  filter(nvc_outlier == "outlier" & pop_outlier == "outlier") %>%
  ggplot(aes(y = vari_perc_pop_rural, x = dist_near_sede#, color = cat_change
             ))+
  geom_point()+
  geom_smooth(method = "lm")+
  ylim(-100,200)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
#  labs(title = "pop in relation to distance to nearest sede in 'outliers' ")+
  theme_classic() -> popRural_dist

tab_1 %>%  
  #filter(nvc_outlier == "outlier" & pop_outlier == "outlier") %>%
  ggplot(aes(y = vari_perc_nvc, x = dist_near_sede#, color = cat_change
             ))+
  geom_point()+
  geom_smooth(method = "lm")+
  #ylim(-100,200)+
  geom_hline(yintercept = 0)+
  #geom_vline(xintercept = 0)+
  #labs(title = "pop in relation to distance to nearest sede in 'outliers' ")+
  theme_classic() -> nvc_change_dist

tab_1 %>%  
  #filter(nvc_outlier == "outlier" & pop_outlier == "outlier") %>%
  ggplot()+
  geom_point(aes(y = log(pop_rural_WP_06), x = dist_near_sede))+
  geom_point(aes(y = log(pop_rural_WP_17), x = dist_near_sede))+
  geom_smooth(method = "lm", aes(y = log(pop_rural_WP_06), x = dist_near_sede), color = "red")+
  geom_smooth(method = "lm", aes(y = log(pop_rural_WP_17), x = dist_near_sede))+
  #ylim(-100,200)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  #labs(title = "pop in relation to distance to nearest sede in 'outliers' ")+
  theme_classic()->fpp_dist_change

##variation in urban population----
tab_1 %>% 
  group_by(code_muni) %>% 
  summarise(mean_popRural_change = mean(vari_perc_pop_rural),
            mean_popUrb_change = mean(vari_perc_pop_urb)) %>% 
  #filter(nvc_outlier == "outlier" & pop_outlier == "outlier") %>%
  ggplot(aes(y = mean_popRural_change, x = mean_popUrb_change
             #, color = cat_change
             ))+
  geom_point()+
  geom_smooth(method = "lm")+
  ylim(-100,200)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  #labs(title = "pop rural and urban variation in 'outliers' ")+
  theme_classic() -> pop_rural_urb

tab_1 %>%  
  #filter(nvc_outlier == "outlier") %>%
  group_by(code_muni) %>% 
  summarise(mean_nvc_change = mean(vari_perc_nvc),
            mean_popUrb_change = mean(vari_perc_pop_urb)) %>% 
  ggplot(aes(y = mean_nvc_change, x = mean_popUrb_change
             #, color = cat_change)
             ))+
  geom_point(alpha=0.1)+
  geom_smooth(method = "lm")+
  #ylim(-100,200)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  #labs(title = "nvc and urban pop relation in 'outliers' ")+
  theme_classic()-> nvc_popUrb_change

tab_1 %>%  
  ggplot(aes(y = vari_perc_estab_agrifam, x = vari_perc_pop_urb
             #, color = cat_change)
  ))+
  geom_point(alpha=0.1)+
  geom_smooth(method = "lm")+
  #ylim(-100,200)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  labs(title = "Number")+
  theme_classic() -> estab_agrifam_popUrb_change

tab_1 %>%  
  ggplot(aes(y = vari_perc_area_agrifam, x = vari_perc_pop_urb
             #, color = cat_change)
  ))+
  geom_point(alpha=0.1)+
  geom_smooth(method = "lm")+
  #ylim(-100,200)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  labs(title = "area")+
  theme_classic() -> area_agrifam_popUrb_change

##maps----
caat_mun %>%
  select(code_muni, geom) %>% 
  rename(geom_mun = geom) %>% 
  left_join(y=tab_1) %>% 
  glimpse() ->tab_1

tab_1 %>% 
  group_by(code_muni) %>% 
  summarise(mean_nvc_change = mean(vari_perc_nvc)) %>%
ggplot()+
  geom_sf(aes(fill=mean_nvc_change))+
  scale_fill_fermenter(type = "div", palette = "RdYlGn", direction = 1
                       ) -> map_nvc_change

tab_1 %>% 
  group_by(code_muni) %>% 
  summarise(mean_pop_change = mean(vari_perc_pop_rural)) %>% 
  #glimpse
ggplot()+
  geom_sf(aes(fill=mean_pop_change))+
  scale_fill_fermenter(type = "div", palette = "RdYlGn", direction = 1
                       ) -> map_pop_change

tab_1 %>% 
  group_by(code_muni) %>% 
  summarise(mean_agrifam_change = mean(vari_perc_area_agrifam)) %>% 
  #glimpse
  ggplot()+
  geom_sf(aes(fill=mean_agrifam_change))+
  scale_fill_fermenter(type = "div", palette = "RdYlGn", direction = 1
                       ) -> map_agrifam_change

tab_1 %>% 
  group_by(code_muni) %>% 
  summarise(mean_pop_change = mean(vari_perc_pop_rural),
            mean_nvc_change = mean(vari_perc_nvc)) %>% 
  mutate(cat_change = if_else(condition = mean_nvc_change==0 | mean_pop_change==0,
                              true = "estable",
                              false = if_else(condition = mean_nvc_change<0 & mean_pop_change<0,
                                              true = "PP",
                                              false = if_else(condition =  mean_nvc_change<0 & mean_pop_change>0,
                                                              true = "PG",
                                                              false = if_else(mean_nvc_change>0 & mean_pop_change>0,
                                                                              true = "GG",
                                                                              false = "GP"
                                                                              )
                                                              )
                                              )
                              )
         ) %>%  
  #glimpse
  filter(cat_change != "estable") %>% 
  ggplot()+
  geom_sf(aes(fill=cat_change))+
  #scale_fill_discrete()+
  scale_fill_viridis_d(labels = c("Off-farm livelihoods",
                                  "Land abandonment",
                                  "small-holders settlement",
                                  "Disposession"))-> map_category_change

#export----
ggarrange(map_nvc_change, map_pop_change, map_agrifam_change, map_category_change)
ggsave(plot = map_pop_change, filename = here("img/map_pop_change.jpg"))
ggsave(plot = map_nvc_change, filename = here("img/map_nvc_change.jpg"))
ggsave(plot = map_category_change, filename = here("img/map_cat_change.jpg"))
ggsave(plot = map_agrifam_change, filename = here("img/map_agrifam_change.jpg"))

ggarrange(pop_nvc_all, pop_nvc_sd, common.legend = T, legend = "bottom") %>% 
  ggsave(filename = here("img/pop_nvc.jpg"))

ggarrange(agrifam_area_nvc_all, agrifam_estab_nvc_all, common.legend = T, legend = "bottom") %>% 
  ggsave(filename = here("img/agrifam_nvc.jpg"))

ggarrange(agrifam_area_cat_change, agrifam_estab_cat_change, common.legend = T, legend = "bottom") %>% 
  ggsave(filename = here("img/agrifam_cat-change.jpg"))

ggsave(plot = shdi_pop_all, filename = here("img/shdi_pop_all.jpg"))
ggsave(plot = pop_rural_urb, filename = here("img/pop_rural_urb.jpg"))
ggsave(plot=nvc_popUrb_change, filename = here("img/nvc_popUrb_change.jpg"))

ggarrange(estab_agrifam_popUrb_change, area_agrifam_popUrb_change) %>% 
  ggsave(filename = here("img/agrifam_popUrb.jpg"))
ggsave(plot = popRural_dist, filename = here("img/popRural_dist.jpg"))
ggsave(plot=nvc_change_dist, filename = here("img/nvc_change_dist.jpg"))
