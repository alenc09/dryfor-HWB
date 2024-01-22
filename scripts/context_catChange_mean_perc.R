#script para avaliar mudança nos indicadores de desenvolvimento das paisagens em relação à média geral

#libraries----
library(here)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)

#data----
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
  select(X, code_muni, vari_perc_nvc, vari_perc_pop_rural, perc_area_agrifam_06, perc_area_agrifam_17,
         pop_urb_mun_06, pop_urb_mun_17) %>% 
  left_join(y = select(dbcap1_rma, code_muni, IDHM_L_2000, IDHM_L_2010, expov_2000,
                       expov_2010, gini_2000, gini_2010, u5mort_2000, U5mort_2010),
            by = "code_muni") %>% 
  left_join(y = pop_urb, by = c("code_muni" = "Codmun7")) %>%  
  group_by(code_muni) %>% 
  summarise(mean_change_nvc = mean(vari_perc_nvc),
            mean_change_fpp = mean(vari_perc_pop_rural),
            agrifam_2000 = mean(perc_area_agrifam_06),
            agrifam_2010 = mean(perc_area_agrifam_17),
            pop_urb_2000 = mean(pop_urb_2000),
            pop_urb_2010 = mean(pop_urb_2010),
            IDHM_L_2000 = mean(IDHM_L_2000),
            IDHM_L_2010 = mean(IDHM_L_2010),
            expov_2000 = mean(expov_2000),
            expov_2010 = mean(expov_2010),
            gini_2000 = mean(gini_2000),
            gini_2010 = mean(gini_2010),
            u5mort_2000 = mean(u5mort_2000),
            u5mort_2010 = mean(U5mort_2010)) %>%
  filter(!is.na(.$code_muni)) %>%
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
    )
  ) %>%
  filter(cat_change != "stable") %>%
  pivot_longer(cols = 4:15, names_to = c(".value", "year"), names_pattern = "(.+)_(.+)") %>% 
  mutate(year = as.factor(year),
         cat_change = as.factor(cat_change)) %>% 
  glimpse -> tab_geral_long

tab_geral_long %>%   
  group_by(year) %>%
  summarise(across(.cols = 5:10, .fns = ~ mean(.x, na.rm = T), .names = "{.col}_mean")) %>%
  glimpse -> tab_mean_geral
  
tab_geral_long %>% 
  group_by(cat_change, year) %>% 
  summarise(across(.cols = 4:9, .fns = ~ mean(.x, na.rm = T), .names = "{.col}_group_mean"),
            across(.cols = 4:9, .fns = ~ sd(.x, na.rm = T), .names = "{.col}_group_sd")) %>%
  left_join(y = tab_mean_geral) %>% 
  group_by(cat_change, year) %>%
  summarise(agrifam_diff = ((agrifam_group_mean/agrifam_mean) - 1)*100,
            pop_urb_diff = ((pop_urb_group_mean/pop_urb_mean) -1)*100,
            IDH_L_diff = ((IDHM_L_group_mean/IDHM_L_mean) -1)*100,
            expov_diff = ((expov_group_mean/expov_mean) - 1)*100,
            gini_diff = ((gini_group_mean/gini_mean) - 1)*100,
            u5mort_diff = ((u5mort_group_mean/u5mort_mean) - 1)*100) %>%
  glimpse -> tab_diff

#Figures----
##expov----
tab_diff %>% 
  ggplot(aes(x = year, y = expov_diff, group = cat_change, color = cat_change))+
  geom_line()+
  geom_segment(x = 1, xend = 2, y = 0, yend = 0, color = "darkgrey")+
  annotate(geom = "text", label = "All municipalities", x = 1.5, y = 0.25 ,color = "darkgrey")+
  scale_x_discrete(expand = expansion(add = 0.2), name = "Year")+
  labs(title = "a) Percentage of households in extreme poverty")+
  scale_y_continuous(name = "Difference from the mean (%)")+
  scale_color_manual(name = "Forest-People", values = c("#018571", "#80cdc1", "#dfc27d", "#a6611a"),
                     label = c("gain-gain", "gain-lose", "lose-gain", "lose-lose"))+
  theme_classic()+
  theme(title = element_text(size = 9)) -> expov_diff

##gini----
tab_diff %>% 
  ggplot(aes(x = year, y = gini_diff, group = cat_change, color = cat_change))+
  geom_line()+
  geom_segment(x = 1, xend = 2, y = 0, yend = 0, color = "darkgrey")+
  annotate(geom = "text", label = "All municipalities", x = 1.5, y = 0.25 ,color = "darkgrey")+
  labs(title = "b) Gini income inequality index")+
  scale_y_continuous(name = "Difference from the mean (%)")+
  scale_x_discrete(expand = expansion(add = 0.2), name = "Year")+
  scale_color_manual(name = "Forest-People", values = c("#018571", "#80cdc1", "#dfc27d", "#a6611a"),
                     label = c("gain-gain", "gain-lose", "lose-gain", "lose-lose"))+
  theme_classic()+
  theme(title = element_text(size = 9)) -> gini_diff

##agrifam----
tab_diff %>% 
  ggplot(aes(x = year, y = agrifam_diff, group = cat_change, color = cat_change))+
  geom_line()+
  geom_segment(x = 1, xend = 2, y = 0, yend = 0, color = "darkgrey")+
  annotate(geom = "text", label = "All municipalities", x = 1.5, y = 0.25 ,color = "darkgrey")+
  labs(title = "c) Percentage of farm land under family agriculture")+
  scale_y_continuous(name = "Difference from the mean (%)")+
  scale_x_discrete(expand = expansion(add = 0.2), name = "Year")+
  scale_color_manual(name = "Forest-People", values = c("#018571", "#80cdc1", "#dfc27d", "#a6611a"),
                     label = c("gain-gain", "gain-lose", "lose-gain", "lose-lose"))+
  theme_classic()+
  theme(title = element_text(size = 9)) -> agrifam_diff

##pop_urb----
tab_diff %>% 
  ggplot(aes(x = year, y = pop_urb_diff, group = cat_change, color = cat_change))+
  geom_line()+
  geom_segment(x = 1, xend = 2, y = 0, yend = 0, color = "darkgrey")+
  annotate(geom = "text", label = "All municipalities", x = 1.5, y = 3 ,color = "darkgrey")+
  labs(title = "d) Number of people living in urban areas")+
  scale_y_continuous(name = "Difference from the mean (%)")+
  scale_x_discrete(expand = expansion(add = 0.2), name = "Year")+
  scale_color_manual(name = "Forest-People", values = c("#018571", "#80cdc1", "#dfc27d", "#a6611a"),
                     label = c("gain-gain", "gain-lose", "lose-gain", "lose-lose"))+
  theme_classic()+
  theme(title = element_text(size = 9)) -> pop_urb_diff

##IDH_L----
tab_diff %>% 
  ggplot(aes(x = year, y = IDH_L_diff, group = cat_change, color = cat_change))+
  geom_line()+
  geom_segment(x = 1, xend = 2, y = 0, yend = 0, color = "darkgrey")+
  annotate(geom = "text", label = "All municipalities", x = 1.5, y = 0.15 ,color = "darkgrey")+
  labs(title = "e) Human Development Index - Longevity")+
  scale_y_continuous(name = "Difference from the mean (%)")+
  scale_x_discrete(expand = expansion(add = 0.2), name = "Year")+
  scale_color_manual(name = "Forest-People", values = c("#018571", "#80cdc1", "#dfc27d", "#a6611a"),
                     label = c("gain-gain", "gain-lose", "lose-gain", "lose-lose"))+
  theme_classic()+
  theme(title = element_text(size = 9)) -> IDHL_diff

##u5mort----
tab_diff %>% 
  ggplot(aes(x = year, y = u5mort_diff, group = cat_change, color = cat_change))+
  geom_line(linetype = "dashed")+
  geom_segment(x = 1, xend = 2, y = 0, yend = 0, color = "darkgrey")+
  annotate(geom = "text", label = "All municipalities", x = 1.5, y = 0.25 ,color = "darkgrey")+
  labs(title = "f) Number of under five child death per thousand\n children")+
  scale_y_continuous(name = "Difference from the mean (%)")+
  scale_x_discrete(expand = expansion(add = 0.2), name = "Year")+
  scale_color_manual(name = "Forest-People", values = c("#018571", "#80cdc1", "#dfc27d", "#a6611a"),
                     label = c("gain-gain", "gain-lose", "lose-gain", "lose-lose"))+
  theme_classic()+
  theme(title = element_text(size = 9)) -> u5mort_diff

##Figure panel----
ggarrange(expov_diff, gini_diff, agrifam_diff, pop_urb_diff, IDHL_diff, u5mort_diff,
          common.legend = T) %>% 
  ggsave(filename = here("img/fig_mean_diff_perc.jpg"), bg = "white", width = 12)

         