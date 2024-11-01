# Thu May 12 15:44:55 2022 ------------------------------
#script para explorar a relação entre mudança na cobertura florestal e fpp

#library----
library(readxl)
library(here)
library(ggplot2)
library(dplyr)
library(ggpubr)

#data----
read_xlsx(here("data/table_analysis.xlsx"))-> tab_analysis

tab_analysis %>% 
  glimpse
#Análises----
##figure quadrant hexGrid----
ggplot(data = tab_analysis) +
  geom_point(aes(x = forest_perc_change, y = popRur_perc_change,
                 colour = hexGrid_quad), alpha = 0.3, stroke = 0, size = 2) +
  ylim(-100, 300) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  annotate(geom = "text", label = "n = 7660", x = 70, y = 150, color = "#018571", fontface = "bold")+
  annotate(geom = "text", label = "n = 8111", x = -70, y = 150, color = "#80cdc1", fontface = "bold")+
  annotate(geom = "text", label = "n = 6989", x = 70, y = -80, color = "#dfc27d", fontface = "bold")+
  annotate(geom = "text", label = "n = 6978", x = -70, y = -80, color = "#a6611a", fontface = "bold")+
  annotate(geom = "text", label = "stable = 9880", x = 70, y =300, color = "grey60", fontface = "bold")+
  xlab("Change in forest cover (%)") +
  ylab("Change in FPP (%)") +
  scale_color_manual(values = c("#018571", "#80cdc1", "#dfc27d", "#a6611a", "grey60"))+
  theme_classic() + 
  theme(legend.position = "none",
        panel.background = element_rect(color = "white")) -> pop_forst_hex

##Figure quadrant municipality----
ggplot(data = tab_analysis) +
  geom_point(aes(x = meanMun_forest_change, y = meanMun_popRur_change,
                 colour = mun_quad), alpha = 0.3, stroke = 0, size = 2) +
  ylim(-100, 150) +
  xlim(-50,50) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  annotate(geom = "text", label = "n = 211", x = 40, y = 100, color = "#018571", fontface = "bold")+
  annotate(geom = "text", label = "n = 280", x = -40, y = 100, color = "#80cdc1", fontface = "bold")+
  annotate(geom = "text", label = "n = 103", x = 40, y = -80, color = "#dfc27d", fontface = "bold")+
  annotate(geom = "text", label = "n = 125", x = -40, y = -80, color = "#a6611a", fontface = "bold")+
  annotate(geom = "text", label = "stable = 441", x = 30, y = 150, color = "grey60", fontface = "bold")+
  xlab("Change in forest cover (%)") +
  ylab("Change in FPP (%)") +
  scale_color_manual(values = c("#018571", "#80cdc1", "#dfc27d", "#a6611a", "grey60"))+
  theme_classic() + 
  theme(legend.position = "none",
        panel.background = element_rect(color = "white")) -> pop_forest_mun

  