# Script to evaluate changes in socioeconomic conditions per group

#Library----
library(readxl)
library(here)
library(tidyr)
library(dplyr)
library(ggplot2)
#library(rstatix)
#library(ggpubr)
library(WRS2)
library(emmeans)

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
  select(X, code_muni, vari_perc_nvc, vari_perc_pop_rural, agrifam_06, agrifam_17,
         pop_urb_mun_06, pop_urb_mun_17) %>% 
  left_join(y = select(dbcap1_rma, code_muni, IDHM_L_2000, IDHM_L_2010, expov_2000,
                       expov_2010, gini_2000, gini_2010, u5mort_2000, U5mort_2010),
            by = "code_muni") %>% 
  left_join(y = pop_urb, by = c("code_muni" = "Codmun7")) %>%  
  group_by(code_muni) %>% 
  summarise(mean_change_nvc = mean(vari_perc_nvc),
            mean_change_fpp = mean(vari_perc_pop_rural),
            agrifam_06 = mean(agrifam_06),
            agrifam_17 = mean(agrifam_17),
            pop_urb_2000 = mean(pop_urb_2000),
            pop_urb_2010 = mean(pop_urb_2010),
            IDHM_L_2000 = mean(IDHM_L_2000),
            IDHM_L_2010 = mean(IDHM_L_2010),
            expov_2000 = mean(expov_2000),
            expov_2010 = mean(expov_2010),
            gini_2000 = mean(gini_2000),
            gini_2010 = mean(gini_2010),
            u5mort_2000 = mean(u5mort_2000),
            U5mort_2010 = mean(U5mort_2010)) %>%
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
  rename(agrifam_2000 = agrifam_06,
         agrifam_2010 = agrifam_17,
         u5mort_2010 = U5mort_2010) %>% 
  pivot_longer(cols = 4:15, names_to = c(".value", "year"), names_pattern = "(.+)_(.+)") %>% 
  mutate(year = as.factor(year),
         cat_change = as.factor(cat_change)) %>% 
  glimpse -> tab_context


#Analysis----
##expov----
aov(data = tab_context, formula =  expov ~ year*cat_change) -> aov.expov
#plot(aov.expov)
summary(aov.expov)

##gini----
aov(data = tab_context, formula =  gini ~ year*cat_change) -> aov.gini
#plot(aov.gini)
summary(aov.gini)
emmeans(aov.gini, ~ cat_change*year) -> emm.gini
pairs(emm.gini, simple = "cat_change")

##agrifam----
aov(data = tab_context, formula =  agrifam ~ year*cat_change) -> aov.agrifam
plot(aov.agrifam)
t2way(data = tab_context, formula =  agrifam ~ year*cat_change)-> Raov.agrifam
mcp2atm(data = tab_context, formula =  agrifam ~ year*cat_change) -> post.agrifam
post.agrifam$contrasts

##IDHM_L----
aov(data = tab_context, formula =  IDHM_L ~ year*cat_change)-> aov.idhL
#plot(aov.idhL)
summary(aov.idhL)
emmeans(aov.idhL, ~cat_change*year) -> emm.idhL
pairs(emm.idhL, simple = "cat_change")

##u5mort----
aov(data = tab_context, formula =  u5mort ~ year*cat_change) -> aov.u5mort
plot(aov.u5mort)
summary(aov.u5mort)
emmeans(aov.u5mort, ~cat_change*year) -> emm.u5mort
pairs(emm.u5mort, simple = "cat_change")

t2way(data = tab_context, formula =  u5mort ~ year*cat_change)

##pop_urb----
aov(data = tab_context, formula =  pop_urb ~ year*cat_change) -> aov.pop_urb
plot(aov.pop_urb)

t2way(data = tab_context, formula =  pop_urb ~ year*cat_change)-> Raov.pop_urb
mcp2atm(data = tab_context, formula =  pop_urb ~ year*cat_change)-> post.pop_urb
post.pop_urb$contrasts

#figures----
##expov----
tab_context %>% 
ggplot(aes(x=year, y = expov, group = cat_change, color = cat_change, fill = cat_change))+
  geom_smooth(method = "glm", alpha = 0.2)+
  scale_x_discrete(expand = expansion(add = 0.1), name = "Year")+
  scale_y_continuous(name = "Extreme poverty")+
  scale_color_manual(name = "Forest-People", values = c("#018571", "#80cdc1", "#dfc27d", "#a6611a"),
                     label = c("gain-gain", "gain-lose", "lose-gain", "lose-lose"))+
  scale_fill_manual(name = "Forest-People", values = c("#018571", "#80cdc1", "#dfc27d", "#a6611a"),
                    label = c("gain-gain", "gain-lose", "lose-gain", "lose-lose"))+
  theme_classic() -> cat.expov
  

##gini----
tab_context %>% 
  ggplot(aes(x=year, y = gini, group = cat_change, color = cat_change, fill = cat_change))+
  geom_smooth(method = "glm", alpha = 0.2)+
  scale_x_discrete(expand = expansion(add = 0.1), name = "Year")+
  scale_y_continuous(name = "Gini income inequality index")+
  scale_color_manual(name = "Forest-People", values = c("#018571", "#80cdc1", "#dfc27d", "#a6611a"),
                     label = c("gain-gain", "gain-lose", "lose-gain", "lose-lose"))+
  scale_fill_manual(name = "Forest-People", values = c("#018571", "#80cdc1", "#dfc27d", "#a6611a"),
                    label = c("gain-gain", "gain-lose", "lose-gain", "lose-lose"))+
  theme_classic() -> cat.gini
  
##agrifam----
tab_context %>% 
  ggplot(aes(x=year, y = agrifam, group = cat_change, color = cat_change, fill = cat_change))+
  geom_smooth(method = "glm", alpha = 0.2)+
  scale_x_discrete(expand = expansion(add = 0.1), name = "Year")+
  scale_y_continuous(name = "Number of family agriculure farms")+
  scale_color_manual(name = "Forest-People", values = c("#018571", "#80cdc1", "#dfc27d", "#a6611a"),
                     label = c("gain-gain", "gain-lose", "lose-gain", "lose-lose"))+
  scale_fill_manual(name = "Forest-People", values = c("#018571", "#80cdc1", "#dfc27d", "#a6611a"),
                    label = c("gain-gain", "gain-lose", "lose-gain", "lose-lose"))+
  theme_classic() -> cat.agrifam

##IDH_L----
tab_context %>% 
  ggplot(aes(x=year, y = IDHM_L, group = cat_change, color = cat_change, fill = cat_change))+
  geom_smooth(method = "glm", alpha = 0.2)+
  scale_x_discrete(expand = expansion(add = 0.1), name = "Year")+
  scale_y_continuous(name = "HDI - Longevity")+
  scale_color_manual(name = "Forest-People", values = c("#018571", "#80cdc1", "#dfc27d", "#a6611a"),
                     label = c("gain-gain", "gain-lose", "lose-gain", "lose-lose"))+
  scale_fill_manual(name = "Forest-People", values = c("#018571", "#80cdc1", "#dfc27d", "#a6611a"),
                    label = c("gain-gain", "gain-lose", "lose-gain", "lose-lose"))+
  theme_classic() -> cat.IDHL

##u5mort----
tab_context %>% 
  ggplot(aes(x=year, y = u5mort, group = cat_change, color = cat_change, fill = cat_change))+
  geom_smooth(method = "glm", alpha = 0.2)+
  scale_x_discrete(expand = expansion(add = 0.1), name = "Year")+
  scale_y_continuous(name = "Under five mortality")+
  scale_color_manual(name = "Forest-People", values = c("#018571", "#80cdc1", "#dfc27d", "#a6611a"),
                     label = c("gain-gain", "gain-lose", "lose-gain", "lose-lose"))+
  scale_fill_manual(name = "Forest-People", values = c("#018571", "#80cdc1", "#dfc27d", "#a6611a"),
                    label = c("gain-gain", "gain-lose", "lose-gain", "lose-lose"))+
  theme_classic() -> cat.u5mort

##pop_urb----
tab_context %>% 
  ggplot(aes(x=year, y = pop_urb, group = cat_change, color = cat_change, fill = cat_change))+
  geom_smooth(method = "glm", alpha = 0.2)+
  scale_x_discrete(expand = expansion(add = 0.1), name = "Year")+
  scale_y_continuous(name = "Urban population")+
  scale_color_manual(name = "Forest-People", values = c("#018571", "#80cdc1", "#dfc27d", "#a6611a"),
                     label = c("gain-gain", "gain-lose", "lose-gain", "lose-lose"))+
  scale_fill_manual(name = "Forest-People", values = c("#018571", "#80cdc1", "#dfc27d", "#a6611a"),
                    label = c("gain-gain", "gain-lose", "lose-gain", "lose-lose"))+
  theme_classic() -> cat.urb_pop
