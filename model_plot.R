library(doParallel)
library(parallel)
library(caret)
library(ggplot2)
library(ggbeeswarm)
library(patchwork)
library(RColorBrewer)

# Register parallelization using all cores
registerDoParallel(cores = detectCores())

# Make cross validation object for caret
cv <- trainControl(method = "repeatedcv",
                   allowParallel = TRUE)

# Get features and target
x <- movie_clean %>% model.matrix(avg_rating ~ ., data = .)
y <- movie_clean %>% pull(avg_rating)

# Train different models
set.seed(123)
elastic <- train(x, y,
                 method = "glmnet",
                 trControl = cv)

set.seed(123)
xgb <- train(x, y,
             method = "xgbTree",
             trControl = cv)

set.seed(123)
knn <- train(x, y,
             method = "knn",
             trControl = cv)

set.seed(123)
mars <- train(x, y,
              method = "earth",
              trControl = cv)

set.seed(123)
rf <- train(x, y,
            method = "rf",
            trControl = cv)

# Print MAE of mean prediction
(movie_clean$avg_rating - mean(movie_clean$avg_rating)) %>%
  abs() %>%
  mean()

# Obtain R-squareds and MAEs
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

# Make the first plot with R-squareds
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

# Make the second plot with MAEs
p2 <- results %>% 
  ggplot(aes(x = reorder(model, mean_Rsquared), y = MAE, color = model)) +
  geom_quasirandom() +
  theme_bw() +
  theme(legend.position = "none") +
  scale_colour_brewer(type = "qual", palette = "Dark2") +
  scale_y_continuous(limits = c(0, max(results$MAE))) +
  xlab("")

# Plot vertically using patchwork
p1 / p2
