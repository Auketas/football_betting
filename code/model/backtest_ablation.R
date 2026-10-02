# Ablation of the v2 model: does the odds-history sequence add anything beyond static/market features?
# Same walk-forward folds as code/model/backtest.R. Variants (predictions saved to results/backtest_ablation/):
#   static_nn   - small dense net on all static features (market prob, log odds, outcome, ndays, daysout, overround, league)
#   logit_static - logistic regression on the non-league static features
#   logit_mkt   - logistic regression on market probability only (recalibrated market)
# Usage (from repo root): Rscript code/model/backtest_ablation.R [runs]

Sys.setenv(RETICULATE_PYTHON = "C:/Users/Auke/tfenv/Scripts/python.exe")
suppressMessages({
  library(dplyr)
  library(keras)
  library(tensorflow)
})
source("code/model/data_prep_v2.R")

args <- commandArgs(trailingOnly = TRUE)
runs <- if (length(args) >= 1) as.integer(args[1]) else 3
dir.create("results/backtest_ablation", recursive = TRUE, showWarnings = FALSE)

all <- readRDS("data/model/train.rds")
all$date <- as.Date(substr(all$id, 1, 10))
all <- all[order(all$date, all$id, all$daysout, match(all$final_result, c("1", "X", "2"))), ]
game_dates <- all[!duplicated(all$id), ]$date
cuts <- as.Date(quantile(as.numeric(game_dates), c(0.5, 0.67, 0.83), type = 1), origin = "1970-01-01")
cuts <- c(cuts, max(game_dates) + 1)

static_nn <- function(X_train, y_train) {
  inp <- layer_input(shape = ncol(X_train))
  out <- inp %>%
    layer_dense(32, activation = "relu") %>% layer_dropout(0.2) %>%
    layer_dense(16, activation = "relu") %>% layer_dropout(0.2) %>%
    layer_dense(1, activation = "sigmoid")
  m <- keras_model(inp, out)
  m %>% compile(optimizer = optimizer_adam(learning_rate = 1e-3), loss = "binary_crossentropy")
  m %>% fit(X_train, y_train, epochs = 100, batch_size = 64, validation_split = 0.1, verbose = 0,
            callbacks = list(callback_early_stopping(monitor = "val_loss", patience = 5, restore_best_weights = TRUE),
                             callback_reduce_lr_on_plateau(monitor = "val_loss", factor = 0.5, patience = 3)))
  m
}

for (k in 1:3) {
  train_rows <- all[all$date < cuts[k], ]
  test_pool  <- all[all$date >= cuts[k] & all$date < cuts[k + 1], ]
  test_rows <- test_pool %>%
    group_by(id, final_result) %>%
    slice_min(order_by = daysout, n = 1, with_ties = FALSE) %>%
    ungroup() %>% as.data.frame()
  test_rows <- test_rows[order(test_rows$date, test_rows$id), ]
  fold_df <- rbind(train_rows, test_rows)
  start <- nrow(train_rows) + 1
  end <- nrow(fold_df)
  d <- generate_train_test_data_v2(fold_df, start, end)
  Xtr <- d$X_static_train; Xte <- d$X_static_test
  cat(sprintf("Fold %d: train %d rows, test %d rows, static features %d\n", k, nrow(Xtr), nrow(Xte), ncol(Xtr)))

  base <- test_rows[, c("id", "date", "league", "daysout", "final_result", "odds", "payoff", "ndays")]
  base$fold <- k

  # --- static neural net, ensemble of runs ---
  pc <- matrix(NA_real_, runs, nrow(Xte))
  for (i in seq_len(runs)) {
    set_random_seed(1000 * k + i)
    m <- static_nn(Xtr, d$y_train)
    pc[i, ] <- m %>% predict(Xte, verbose = 0)
    k_clear_session()
  }
  o <- base; o$p <- colMeans(pc); saveRDS(o, sprintf("results/backtest_ablation/static_nn_fold%d.rds", k))

  # --- logistic regressions ---
  nl <- !grepl("^league", colnames(Xtr))
  Xtr_df <- as.data.frame(Xtr[, nl]); Xte_df <- as.data.frame(Xte[, nl])
  Xtr_df$is_draw <- NULL; Xte_df$is_draw <- NULL  # drop one outcome dummy (collinear with intercept)
  Xtr_df$y <- d$y_train
  g1 <- glm(y ~ ., data = Xtr_df, family = binomial())
  o <- base; o$p <- as.numeric(predict(g1, newdata = Xte_df, type = "response"))
  saveRDS(o, sprintf("results/backtest_ablation/logit_static_fold%d.rds", k))

  g2 <- glm(y ~ mkt_prob, data = Xtr_df, family = binomial())
  o <- base; o$p <- as.numeric(predict(g2, newdata = Xte_df, type = "response"))
  saveRDS(o, sprintf("results/backtest_ablation/logit_mkt_fold%d.rds", k))
}
