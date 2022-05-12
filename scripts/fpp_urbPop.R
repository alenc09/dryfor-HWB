# Thu May 12 10:39:08 2022 ------------------------------
#script para medir a relação entre variação da população urbana e variação da população rural

#library----
library(here)
library(dplyr)
library(ggplot2)

#data----
read.csv(file = here("tabela_geral.csv"))-> tab_geral

#análises exploratórias----
tab_geral %>% 
  group_by(code_muni) %>% 
  summarise(mean_fpp_change = mean(vari_perc_pop_rural),
            vari_perc_pop_urb = vari_perc_pop_urb) %>% 
  glimpse %>% 
ggplot() +
  geom_point(aes(x = vari_perc_pop_urb, y = mean_fpp_change))+
  ylim(-100, 200) +
  geom_vline(xintercept = 0) + 
  geom_hline(yintercept = 0) +
  geom_smooth(aes(x = vari_perc_pop_urb, y = mean_fpp_change), method = "lm") +
  xlab("Variation in urban population (%)") +
  ylab("Mean variation in FPP (%)") +
  theme_classic() -> fig.fpp_urbPop

ggsave(plot = fig.fpp_urbPop, filename = here("fig.fpp_urbPop.jpg"))