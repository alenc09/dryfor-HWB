# Tue Oct 29 12:05:00 2024 ------------------------------
# Script para avaliar mudança no PIB por quandrante de mudança

#libraries----
library(here)
library(readxl)
library(ggplot2)
library(emmeans)

#data----
read_xlsx(here("data/table_analysis.xlsx")) -> table_analysis

##organization----
table_analysis %>% 
  select(code_mun, pibAgro_perc_change:pibServPub_perc_change, meanMun_forest_change:mun_quad) %>% 
  unique() %>% 
  mutate(across(.cols = where(is.character), .fns = as.factor)) %>% 
  glimpse -> tab_pib_munQuad

#analysis----
aov(data = tab_pib_munQuad, formula =  pibAgro_perc_change ~ mun_quad) -> pibServPub
summary(pibServPub)
emmeans(pibServPub, ~ mun_quad) -> emm.pibServPub
pairs(emm.pibServPub, simple = "mun_quad")

#figure----
tab_pib_munQuad %>% 
  ggplot()+
  geom_boxplot(aes(x = mun_quad, y = pibServPub_perc_change))
