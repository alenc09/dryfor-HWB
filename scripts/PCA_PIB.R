

tab_abs_change_mun %>% 
  select(-1:-6) %>% 
  glimpse %>% 
  prcomp(scale. = T, center = T) -> pca_abs_change

summary(pca_abs_change)
biplot(pca_abs_change)

pca_abs_change$x

ggplot(data = as.data.frame(pca_abs_change$x), aes(x = PC1, y = PC2))+
  geom_point()

tab_abs_change_mun %>% 
  select(mean_change_fpp) %>% 
  cbind(pca_abs_change$x) %>% 
  ggplot()+
  geom_point(aes(x = PC1, y = PC2, color = mean_change_fpp > 0))