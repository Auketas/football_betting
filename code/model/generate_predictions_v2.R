library(keras)

source("code/model/compile_model_v2.R")
source("code/model/data_prep_v2.R")

generate_predictions_v2 <- function() {
  train_raw <- readRDS("data/model/train.rds")
  test_raw  <- readRDS("data/model/test.rds")

  # Enrich both sets with the full three-outcome histories before any formatting.
  # Each dataset is enriched independently so no test data influences the train set.
  train <- enrich_with_all_outcomes(train_raw)
  test  <- enrich_with_all_outcomes(test_raw)

  # Scaler is fit on training data only — no test statistics used.
  scaler <- fit_static_scaler(train)

  X_seq_train    <- format_seq_data_v2(train)
  X_seq_test     <- format_seq_data_v2(test)
  X_static_train <- format_static_data_v2(train, scaler)
  X_static_test  <- format_static_data_v2(test, scaler)
  y_train        <- ifelse(train$payoff > 0, 1, 0)

  # Ensemble over multiple random initialisations to reduce variance.
  runs <- 10
  pcts <- matrix(nrow = runs, ncol = nrow(test))
  for (i in 1:runs) {
    model      <- compile_model_live_v2(X_seq_train, X_static_train, y_train)
    pcts[i, ]  <- model %>% predict(list(X_seq_test, X_static_test))
  }

  test$predicted_pct     <- colMeans(pcts)
  test$predicted_payoff  <- as.numeric(test$predicted_pct) * test$odds - 1

  threshold <- 0.7
  bet_df    <- test[test$predicted_payoff > threshold & test$odds < 10, ]
  bet_df$date <- Sys.Date()

  oldbet <- read.csv("results/bets.csv")
  newbet <- rbind(bet_df, oldbet)
  write.csv(newbet, "results/bets.csv", row.names = FALSE)
}
