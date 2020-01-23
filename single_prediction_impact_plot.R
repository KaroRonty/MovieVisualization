detach("package:skimr", unload = TRUE)
# Explaining XGBoost results ----
library(xgboost)
library(tibble)
library(stringr)
library(ggplot2)

x <- movie_clean %>% model.matrix(avg_rating ~ ., data = .)
y <- movie_clean %>% pull(avg_rating)

# Make matrices for training and test data
training_xgb <- xgb.DMatrix(x, label = y)

test_xgb <- xgb.DMatrix(x[which.min(y), , drop = FALSE])

# Train XGBoost model using the same hyperparameters as caret
set.seed(123)
xgb_explain <- xgboost(data = xgb.DMatrix(x,
                                          label = y), 
                       nround = xgb$bestTune$nrounds,
                       max_depth = xgb$bestTune$max_depth, 
                       eta = xgb$bestTune$eta,
                       gamma = xgb$bestTune$gamma,
                       colsample_bytree = xgb$bestTune$colsample_bytree,
                       min_child_weight = xgb$bestTune$min_child_weight,
                       subsample = xgb$bestTune$subsample)

t1 <- predict(xgb_explain, test_xgb, predcontrib = TRUE)

# Transform coefficients to tibble
t2 <- t1 %>%
  as.data.frame() %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column() %>% 
  rename(Predictor = 1,
         value = 2)

languages <- t2 %>% 
  filter(str_detect(Predictor, "language")) %>% 
  select(value) %>% 
  colMeans()

genres <- t2 %>% 
  filter(str_detect(Predictor, "1")) %>% 
  select(value) %>% 
  colMeans()

t3 <- t2 %>% 
  filter(!str_detect(Predictor, "language"),
         !str_detect(Predictor, "1"),
         !str_detect(Predictor, "Intercept")) %>% 
  mutate(Predictor = str_replace(Predictor, "BIAS", "intercept")) %>% 
  add_row(Predictor = c("language", "genre"),
          value = c(languages, genres)) %>% 
  arrange(-abs(value))

t4 <- t3 %>% 
  mutate(id = seq_along(Predictor),
         color = ifelse(value < 0, "#F8766D", "#00BFC4"),
         end = c(head(cumsum(value), -1), sum(value)),
         text_color = "black") %>% 
  mutate(start = c(0, head(end, -1))) %>% 
  add_row(id = max(.$id) + 1,
          Predictor = "PREDICTION",
          value = sum(.$value),
          color = "black",
          text_color = "white",
          start = sum(.$value),
          end = 0) %>% 
  select(id, Predictor, value, color, text_color, start, end) %>% 
  mutate(Predictor = reorder(Predictor, id))

p3 <- ggplot(t4, aes(x = Predictor,
                     xmin = id - 0.45,
                     xmax = id + 0.45,
                     ymin = end,
                     ymax = start)) +
  geom_rect(color = "black",
            fill = t4$color) +
  geom_segment(aes(x = id - 0.45,
                   xend = id + 1.45,
                   y = end,
                   yend = end),
               data = head(t4, -1)) +
  geom_text(aes(x = id,
                y = start + (end - start) / 2,
                label = format(round(value, 2), nsmall = 2)),
            size = 3,
            color = t4$text_color) +
  ylab("Impact on rating") +
  ggtitle("Impact of each predictor for a single observation",
          subtitle = paste("Observation number", which.min(y))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")