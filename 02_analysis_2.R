# Thu Apr 07 09:26:54 2022 ------------------------------
#script para novas análises

#library----
library(dplyr)
library(ggplot2)
library(sf)
library(ggpubr)

#data----
read.csv(file = "/home/alenc/Documents/Doutorado/tese/cap0/dryfor-HWB/tabela_geral.csv")-> tab_1
#análises exploratorias----
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
