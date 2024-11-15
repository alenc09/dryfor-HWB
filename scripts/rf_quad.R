# Fri Nov 15 09:09:35 2024 ------------------------------
#script to analysis data using ranks

#libraries----
library(readxl)
library(here)
library(dplyr)
library(MASS)
library(caret)
library(randomForest)
library(pdp)
library(gridExtra)

#data----
read_xlsx(here("data/table_analysis.xlsx")) -> table_analysis
table_analysis %>% 
  dplyr::select(code_mun, pibAgro_perc_change:mun_quad, -hexGrid_quad) %>% 
  unique() %>%
  # na.omit() %>% 
  mutate(code_mun = as.factor(code_mun),
         mun_quad = as.factor(mun_quad)) %>% 
  glimpse -> tab_mun

#analysis----
##LDA----
lda(data = tab_mun,
    mun_quad ~ pibAgro_perc_change + pibInd_perc_change + pibServPriv_perc_change +
      pibServPub_perc_change + expov_perc_change + gini_change + u5mort_change + 
      sdMun_forest_change + sdMun_popRur_change) -> lda_quad

print(lda_quad)
plot(lda_quad)

predict(lda_quad) -> predict_lda
confusionMatrix(predict_lda$class, as.factor(tab_mun$mun_quad))


##random forest----
set.seed(123)
createDataPartition(tab_mun$mun_quad, p = 0.7, list = F) -> train_index
tab_mun[train_index,] -> train_data
tab_mun[-train_index,] -> test_data

summary(tab_mun)

imputed_data <- missForest(xmis = tab_mun)

tune_grid <- expand.grid(mtry = c(1, 2, 3, 4))
control <- trainControl(method = "cv", number = 5)  # 5-fold cross-validation

rf_tuned <- train(
  as.factor(mun_quad) ~
    pibAgro_perc_change +
    pibInd_perc_change +
    pibServPriv_perc_change +
    pibServPub_perc_change +
    expov_perc_change +
    gini_change +
    u5mort_change,
    data = train_data,
  method = "rf",
  trControl = control,
  tuneGrid = tune_grid,
  ntree = 500,
  na.action = na.omit,
  # You can also tune ntree in a loop
)
 
print(rf_tuned)
print(rf_tuned$bestTune)

randomForest(as.factor(mun_quad) ~ 
               pibAgro_perc_change + 
               pibInd_perc_change + 
               pibServPriv_perc_change +
               pibServPub_perc_change + 
               expov_perc_change +
               gini_change +
               u5mort_change
             , data = train_data, ntree = 1000,
             mtry = 4, importance = TRUE, na.action = na.omit) -> rf_mod

print(rf_mod)
predictions <- predict(rf_mod, newdata = test_data)
confusion_matrix <- confusionMatrix(predictions, as.factor(test_data$mun_quad))
print(confusion_matrix)

importance(rf_mod)
varImpPlot(rf_mod)

partial(rf_mod, pred.var = "pibAgro_perc_change", which.class = "GP", plot = TRUE, probs = T)

pdp_var <- partial(rf_mod, pred.var = c("pibInd_perc_change","u5mort_change"), plot = TRUE)

###figure ----
c("GG", "GP", "PG", "PP", "stable") -> categories
c("pibAgro_perc_change", "pibInd_perc_change", "pibServPriv_perc_change", 
  "pibServPub_perc_change", "expov_perc_change", "gini_change", "u5mort_change") -> predictors

pdp_list <- list()
# Loop through each predictor and category to create PDPs
for (pred in predictors) {
  for (cat in categories) {
    pdp_data <- partial(rf_mod, pred.var = pred, which.class = cat, prob = TRUE)
    
    # Generate the ggplot for each PDP
    pdp_plot <- ggplot(pdp_data, aes(x = pred, y = "yhat")) +
      geom_line() +
      ggtitle(paste("PDP for", cat, "with", pred)) +
      ylab("Predicted Probability (yhat)") +
      xlab(pred) +
      theme_minimal()
    
    # Add the plot to the list
    pdp_list[[paste(pred, cat, sep = "_")]] <- pdp_plot
  }
}

do.call("grid.arrange", c(pdp_list, ncol = 5))
##pca

##contingency table----