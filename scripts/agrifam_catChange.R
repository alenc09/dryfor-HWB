# Wed May 11 12:38:24 2022 ------------------------------
#Script para medir o número e área de agricultura familiar por categoria de mudança (floresta/fpp)

#library----
library(dplyr)
library(ggplot2)
library(ggpubr)

#data----
read.csv(file = here("tabela_geral.csv"))-> tab_1

#análises exploratórias----
tab_1 %>% 
  drop_na() %>% 
  glimpse %>% 
  ggplot()+
  geom_boxplot(aes(x = cat_change, y = vari_perc_area_agrifam, color = cat_change))+
  geom_hline(yintercept = 0)+
  ylim(-50,50)+
  xlab("Category of change")+ylab("Variation in area of family agriculture (%)")+
  scale_color_discrete(labels = c("Off-farm livelihoods",
                                  "Land abandonment",
                                  "small-holders settlement",
                                  "Disposession"))+
  theme_classic()+
  theme(legend.title = element_blank()) -> fig.agrifam_catChange

tab_1 %>% 
  drop_na() %>% 
  glimpse %>% 
  ggplot()+
  geom_boxplot(aes(x = cat_change, y = vari_perc_estab_agrifam, color = cat_change))+
  geom_hline(yintercept = 0)+
  xlab("Category of change")+ylab("Variation in number of family agriculture (%)")+
  scale_color_discrete(labels = c("Off-farm livelihoods",
                                  "Land abandonment",
                                  "small-holders settlement",
                                  "Disposession"))+
  theme_classic()+
  theme(legend.title = element_blank()) -> fig.agrifam_numb_catChange

ggarrange(fig.agrifam_catChange, fig.agrifam_numb_catChange, common.legend = T, legend = "bottom") %>% 
  ggsave(plot = ., filename = here("img/fig.agrifam_catChange.jpg"))

