library(doParallel)
library(parallel)
library(caret)
library(ggplot2)
library(ggbeeswarm)
library(patchwork)
library(RColorBrewer)

# Register parallelization using all cores
registerDoParallel(cores = detectCores())

cv <- trainControl(method = "repeatedcv",
                   allowParallel = TRUE,
                   seeds = 123) # FIXME ?

x <- movie_clean %>% model.matrix(avg_rating ~ ., data = .)
y <- movie_clean %>% pull(avg_rating)

#####
elastic <- train(x,
                 y,
                 method = "glmnet",
                 trControl = cv)

xgb <- train(x,
             y,
             method = "xgbTree",
             trControl = cv)

knn <- train(x,
             y,
             method = "knn",
             trControl = cv)

mars <- train(x,
              y,
              method = "earth",
              trControl = cv)

rf <- train(x,
            y,
            method = "rf",
            trControl = cv)


pred_elastic <- predict(elastic, x)
cor(pred_elastic, y)^2

pred_xgb <- predict(xgb, x)
cor(pred_xgb, y)^2

pred_knn <- predict(knn, x)
cor(pred_knn, y)^2

pred_mars <- predict(mars, x)
cor(pred_mars, y)^2

pred_rf <- predict(rf, x)
cor(pred_rf, y)^2

(movie_clean$avg_rating - mean(movie_clean$avg_rating)) %>% abs %>% mean()

results <- elastic$results %>%
  select(MAE, Rsquared) %>% 
  mutate(model = "Elastic Net") %>% 
  rbind(xgb$results %>% 
          select(MAE, Rsquared) %>% 
          mutate(model = "XGBoost")) %>% 
  rbind(knn$results %>% 
          select(MAE, Rsquared) %>% 
          mutate(model = "KNN")) %>% 
  rbind(mars$results %>% 
          select(MAE, Rsquared) %>% 
          mutate(model = "MARS")) %>% 
  rbind(rf$results %>% 
          select(MAE, Rsquared) %>% 
          mutate(model = "Random Forest")) %>% 
  group_by(model) %>% 
  mutate(mean_Rsquared = mean(Rsquared),
         mean_MAE = mean(MAE)) %>% 
  ungroup()

p1 <- results %>% 
  ggplot(aes(x = reorder(model, mean_Rsquared), y = Rsquared, color = model)) +
  geom_quasirandom() +
  ggtitle("Cross-validated accuracy measures for different models",
          subtitle = "Using default hyperparameter search by the caret library") +
  theme_bw() +
  theme(legend.position = "none") +
  scale_colour_brewer(type = "qual", palette = "Dark2") +
  scale_y_continuous(limits = c(0, 1)) +
  xlab("")

p2 <- results %>% 
  ggplot(aes(x = reorder(model, mean_Rsquared), y = MAE, color = model)) +
  geom_quasirandom() +
  theme_bw() +
  theme(legend.position = "none") +
  scale_colour_brewer(type = "qual", palette = "Dark2") +
  scale_y_continuous(limits = c(0, max(results$MAE))) +
  xlab("")

p1 / p2