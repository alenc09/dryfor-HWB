
read_xlsx(here("data/table_analysis.xlsx")) -> table_analysis
table_analysis %>% 
  dplyr::select(code_mun, pibAgro_perc_change:pibServPub_perc_change, -hexGrid_quad, -starts_with("sd")) %>% 
  unique() %>%
  # na.omit() %>% 
  # mutate(code_mun = as.factor(code_mun),
  #        mun_quad = as.factor(mun_quad)) %>% 
  glimpse -> tab_mun

# Exclude non-numeric columns (e.g., 'category')
numeric_data <- tab_mun[, sapply(tab_mun, is.numeric)]
scaled_data <- scale(numeric_data)

# Perform PCA
pca_result <- prcomp(scaled_data, center = TRUE, scale. = TRUE)

# Print summary to see the proportion of variance explained
summary(pca_result)

# Scree plot to show variance explained by each principal component
plot(pca_result, type = "lines", main = "Scree Plot")

# Biplot to show variables and observations
biplot(pca_result, main = "PCA Biplot")

library(ggplot2)
table_analysis %>% 
  dplyr::select(code_mun, pibAgro_perc_change:pibServPub_perc_change, mun_quad, -hexGrid_quad, -starts_with("sd")) %>% 
  unique() %>%
  # na.omit() %>% 
  # mutate(code_mun = as.factor(code_mun),
  #        mun_quad = as.factor(mun_quad)) %>% 
  glimpse -> ta_mun_cat
# Create a data frame of PC scores
pca_scores <- as.data.frame(pca_result$x)
pca_scores$category <- ta_mun_cat$mun_quad  # Add the category column if needed for grouping

# Plot using ggplot2
ggplot(pca_scores, aes(x = PC1, y = PC2, color = category)) +
  geom_point() +
  ggtitle("PCA Plot of Observations") +
  theme_minimal()



# Standardize the data (recommended)
scaled_data <- scale(numeric_data)
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