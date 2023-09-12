# Thu May 12 15:44:55 2022 ------------------------------
#script para explorar a relação entre mudança na cobertura florestal e fpp

#library----
library(here)
library(ggplot2)
library(dplyr)
library(ggpubr)

#data----
read.csv(here("data/tabela_geral.csv"))-> tab_geral
tab_geral %>% 
   glimpse

#análises exploratórias----
## figure with all landscapes----
ggplot(data = na.omit(tab_geral)) +
  geom_point(aes(x = vari_perc_nvc, y = vari_perc_pop_rural,
                 colour = cat_change), alpha = 0.3, stroke = 0, size = 2) +
  ylim(-100, 200) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  annotate(geom = "text", label = "n = 1894", x = 30, y = 150, color = "#018571", fontface = "bold")+
  annotate(geom = "text", label = "n = 1138", x = 30, y = -80, color = "#80cdc1", fontface = "bold")+
  annotate(geom = "text", label = "n = 1808", x = -25, y = 150, color = "#dfc27d", fontface = "bold")+
  annotate(geom = "text", label = "n = 888", x = -25, y = -80, color = "#a6611a", fontface = "bold")+
  xlab("Change in forest cover (%)") +
  ylab("Change in FPP (%)") +
  scale_color_manual(values = c("#018571", "#80cdc1", "#dfc27d", "#a6611a"))+
  theme_classic()+
  theme(legend.position = "none",
        panel.background = element_rect(color = "white")) -> pop_nvc_all

##Figure without outliers and landscapes above on SD----
Q <- quantile(tab_geral$vari_perc_nvc, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(tab_geral$vari_perc_nvc, na.rm = T)
up <-  Q[2]+1.5*iqr # Upper Range
low<- Q[1]-1.5*iqr # Lower Range

quantile(tab_geral$vari_perc_pop_rural, probs=c(.25, .75), na.rm = TRUE) -> Q_fpp
IQR(tab_geral$vari_perc_pop_rural, na.rm = T) -> iqr_fpp
Q_fpp[2]+1.5*iqr_fpp -> up_fpp # Upper Range
Q_fpp[1]-1.5*iqr_fpp -> low_fpp # Lower Range

tab_geral %>% 
  select(buff_id, vari_perc_nvc, vari_perc_pop_rural, cat_change) %>% 
  filter(vari_perc_nvc < up &
           vari_perc_nvc > low &
           vari_perc_pop_rural < up_fpp &
           vari_perc_pop_rural > low_fpp) %>% 
  glimpse -> tab_s_outlier

mean(tab_s_outlier$vari_perc_nvc) + sd(tab_s_outlier$vari_perc_nvc)
mean(tab_s_outlier$vari_perc_nvc) - sd(tab_s_outlier$vari_perc_nvc)
mean(tab_s_outlier$vari_perc_pop_rural, na.rm=T) + sd(tab_s_outlier$vari_perc_pop_rural, na.rm=T)
mean(tab_s_outlier$vari_perc_pop_rural, na.rm=T) - sd(tab_s_outlier$vari_perc_pop_rural, na.rm=T)

tab_s_outlier %>%  
  filter(vari_perc_nvc > 6.575527 |
           vari_perc_nvc < -5.839117) %>% 
  filter(vari_perc_pop_rural > 24.18684 |
           vari_perc_pop_rural < -11.30523) %>%
  ggplot(aes(x = vari_perc_nvc, y = vari_perc_pop_rural, color = cat_change))+
  geom_point(alpha = 0.3, stroke = 0, size = 2.5)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  labs(title = "b) Sampled landscapes over one SD (n = 496)")+
  scale_color_manual(values = c("#018571", "#80cdc1", "#dfc27d", "#a6611a"),
                     label = c("gain-gain", "gain-lose", "lose-gain", "lose-lose"))+
  annotate(geom = "text", label = "n = 86", x = 10, y = 15, color = "#018571", fontface = "bold")+
  annotate(geom = "text", label = "n = 143", x = 10, y = -5, color = "#80cdc1", fontface = "bold")+
  annotate(geom = "text", label = "n = 135", x = -10, y = 15, color = "#dfc27d", fontface = "bold")+
  annotate(geom = "text", label = "n = 132", x = -10, y = -5, color = "#a6611a", fontface = "bold")+
  xlab("Change in native vegetation cover (%)") +
  ylab("Change in FPP (%)")+
  theme_classic()+
  theme(legend.title = element_blank(),
        panel.background = element_rect(color = "white")) -> pop_nvc_sd

ggarrange(pop_nvc_all, pop_nvc_sd, common.legend = T, legend = "bottom") %>% 
  ggsave(plot = ., filename = here("img/fig.fpp_nvc.jpg"), dpi = 300,
         bg = "white", width = 10)

##counting outliers----
tab_s_outlier %>%  
  filter(vari_perc_nvc > 6.575527 |
           vari_perc_nvc < -5.839117) %>% 
  filter(vari_perc_pop_rural > 24.18684 |
           vari_perc_pop_rural < -11.30523) %>% 
  group_by(cat_change) %>% 
  summarise(n_land = n()) %>% 
  glimpse
