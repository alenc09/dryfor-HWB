# Thu Apr 07 09:26:54 2022 ------------------------------
#script para novas análises

#library----
library(dplyr)
library(ggplot2)
library(sf)
library(ggpubr)

#data----

#análises exploratorias----
## pop and nvc change----
ggplot(data = tab_1, aes(x = vari_perc_nvc, y = vari_perc_pop_rural, color = cat_change))+
  geom_point()+
  ylim(-100,200)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  labs(title = "nvc and pop variation in all buffers")+
  theme_classic()

tab_1 %>%  
  filter(nvc_outlier == "outlier" & pop_outlier == "outlier") %>%
ggplot(aes(x = vari_perc_nvc, y = vari_perc_pop_rural, color = cat_change))+
  geom_point()+
  ylim(-100,200)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  labs(title = "nvc and pop variation in 'outliers' ")+
  theme_classic()

##family agriculture----
tab_1 %>%  
  filter(nvc_outlier == "outlier" & pop_outlier == "outlier") %>%
  ggplot(aes(x = vari_perc_nvc, y = vari_perc_area_agrifam#, color = cat_change
             ))+
  geom_point()+
  ylim(-100,200)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  labs(title = "nvc and agrifam variation in 'outliers' ")+
  theme_classic()

tab_1 %>%  
  filter(nvc_outlier == "outlier" & pop_outlier == "outlier") %>%
  ggplot(aes(x = vari_perc_pop_rural, y = vari_perc_area_agrifam#, color = cat_change
             ))+
  geom_point()+
  #geom_smooth(method = "lm")+
  ylim(-100,200)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  labs(title = "pop and agrifam variation in 'outliers' ")+
  theme_classic()

tab_1 %>%  
  filter(nvc_outlier == "outlier" & pop_outlier == "outlier") %>%
  ggplot(aes(x = cat_change, y = vari_perc_area_agrifam, color = cat_change))+
  geom_boxplot()+
  #geom_smooth(method = "lm")+
  #ylim(-100,200)+
  geom_hline(yintercept = 0)+
  #geom_vline(xintercept = 0)+
  #labs(title = "pop and agrifam variation in 'outliers' ")+
  theme_classic()

tab_1 %>%  
  filter(nvc_outlier == "outlier" & pop_outlier == "outlier") %>%
  ggplot(aes(x = cat_change, y = vari_estab_agrifam, color = cat_change))+
  geom_boxplot()+
  #geom_smooth(method = "lm")+
  #ylim(-100,200)+
  geom_hline(yintercept = 0)+
  #geom_vline(xintercept = 0)+
  #labs(title = "pop and agrifam variation in 'outliers' ")+
  theme_classic()
##landscape diversity----
tab_1 %>%  
  filter(nvc_outlier == "outlier" & pop_outlier == "outlier") %>%
  ggplot(aes(x = vari_perc_pop_rural, y = vari_shdi, color = cat_change))+
  geom_point()+
  geom_smooth(method = "lm")+
  #ylim(-100,200)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  labs(title = "pop and agrifam variation in 'outliers' ")+
  theme_classic()

##distance to municipality seat----
tab_1 %>%  
  filter(nvc_outlier == "outlier" & pop_outlier == "outlier") %>%
  ggplot(aes(y = vari_perc_pop_rural, x = dist_near_sede, color = cat_change))+
  geom_point()+
  geom_smooth(method = "lm")+
  #ylim(-100,200)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  labs(title = "pop and agrifam variation in 'outliers' ")+
  theme_classic()

tab_1 %>%  
  filter(nvc_outlier == "outlier" & pop_outlier == "outlier") %>%
  ggplot(aes(y = vari_perc_nvc, x = dist_near_sede, color = cat_change))+
  geom_point()+
  geom_smooth(method = "lm")+
  #ylim(-100,200)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  labs(title = "pop and agrifam variation in 'outliers' ")+
  theme_classic()

##variation in urban population----
tab_1 %>%  
  filter(nvc_outlier == "outlier" & pop_outlier == "outlier") %>%
  ggplot(aes(y = vari_perc_pop_rural, x = vari_perc_pop_urb
             #, color = cat_change
             ))+
  geom_point()+
  geom_smooth(method = "lm")+
  #ylim(-100,200)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  #labs(title = "pop and agrifam variation in 'outliers' ")+
  theme_classic()

tab_1 %>%  
  filter(nvc_outlier == "outlier" & pop_outlier == "outlier") %>%
  ggplot(aes(y = vari_perc_nvc, x = vari_perc_pop_urb
             #, color = cat_change)
             ))+
  geom_point()+
  geom_smooth(method = "lm")+
  #ylim(-100,200)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  #labs(title = "pop and agrifam variation in 'outliers' ")+
  theme_classic()

##maps----
caat_mun %>%
  select(GEOCODIG_M, geometry) %>% 
  rename(code_muni = GEOCODIG_M,
         geo_mun = geometry) %>% 
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
  mutate(cat_change = if_else(condition = mean_nvc_change<0 & mean_pop_change<0,
                              true = "PP",
                              false = if_else(condition =  mean_nvc_change<0 & mean_pop_change>0,
                                              true = "PG",
                                              false = if_else(mean_nvc_change>0 & mean_pop_change>0,
                                                              true = "GG",
                                                              false = "GP")))) %>% 
  #glimpse
  ggplot()+
  geom_sf(aes(fill=cat_change))+
  scale_fill_viridis_d()-> map_category_change

ggarrange(map_nvc_change, map_pop_change, map_agrifam_change, map_category_change)
