# Mon Mar 14 15:24:16 2022 ------------------------------
#script to make figure of chapter 4

#library----
library(ggplot2)
library(dplyr)

#data----
table_analysis


#figures----
##"vulnerability"----
ggplot(data = table_analysis2)+
  geom_jitter(aes(x = buff_class, y = prop_dom_expov))+
  geom_hline(yintercept = median(table_analysis2$prop_dom_expov, na.rm = T))

##forest vs non-forest----
ggplot(data = table_analysis2)+
  geom_density(aes(x = prop_dom_expov, color=buff_class, fill = buff_class, alpha = 0.5))#+
  #geom_vline(xintercept = mean(log(expov+1)), na.rm = T)

ggplot(data = table_analysis2)+
  geom_density(aes(x = prop_dom_Senerg, color=buff_class, fill = buff_class, alpha = 0.5))

ggplot(data = table_analysis2)+
  geom_density(aes(x = prop_dom_Scist, color=buff_class, fill = buff_class, alpha = 0.5))

ggplot(data = table_analysis2)+
  geom_density(aes(x = prop_pop_analf, color=buff_class, fill = buff_class, alpha = 0.5))
