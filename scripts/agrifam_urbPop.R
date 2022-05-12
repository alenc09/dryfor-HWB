# Thu May 12 11:01:44 2022 ------------------------------
#script para medir relação entre variação na população urbana e agricultura familiar

#library----
library(here)
library(dplyr)
library(ggplot2)
library(ggpubr)

#data----
read.csv(file = here("tabela_geral.csv"))-> tab_geral

#análises exploratórias----
tab_geral %>%
  select(code_muni, vari_perc_pop_urb, vari_perc_area_agrifam) %>% 
  distinct() %>% 
  glimpse %>% 
ggplot() +
  geom_point(aes(x = vari_perc_pop_urb, y = vari_perc_area_agrifam)) +
  geom_vline(xintercept = 0) + 
  geom_hline(yintercept = 0) +
  ylim(-50, 50) +
  xlab("Variation in urban population (%)") +
  ylab("Variation in familty agriculture area (%)") +
  theme_classic() -> fig.agrifam_area_urbPop

tab_geral %>%
  select(code_muni, vari_perc_pop_urb, vari_perc_estab_agrifam) %>% 
  distinct() %>% 
  glimpse %>% 
  ggplot() +
  geom_point(aes(x = vari_perc_pop_urb, y = vari_perc_estab_agrifam)) +
  geom_vline(xintercept = 0) + 
  geom_hline(yintercept = 0) +
  xlab("Variation in urban population (%)") +
  ylab("Variation in number of familty agriculture (%)") +
  theme_classic() -> fig.agrifam_numb_urbPop

ggarrange(fig.agrifam_area_urbPop, fig.agrifam_numb_urbPop) %>% 
  ggsave(plot = ., filename = here("fig.agrifam_urbPop.jpg"))
 