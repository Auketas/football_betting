# Walk-forward backtest of the v1 and v2 models on data/model/train.rds.
#
# Games are split by date (expanding window): each fold trains on every game-day row of
# games before the cutoff and is tested on games after it. As in live use, a test game is
# represented by its latest observation (smallest daysout) for each of the three outcomes.
# Predictions are saved to results/backtest/<model>_fold<k>.rds; strategies are evaluated
# in code/model/backtest_analysis.R.
#
# Usage (from repo root): Rscript code/model/backtest.R <v1|v2> [runs] [folds]

Sys.setenv(RETICULATE_PYTHON = "C:/Users/Auke/tfenv/Scripts/python.exe")
suppressMessages({
  library(dplyr)
  library(keras)
  library(tensorflow)
})
source("code/model/compile_model.R")
source("code/model/data_prep.R")
source("code/model/compile_model_v2.R")
source("code/model/data_prep_v2.R")

args <- commandArgs(trailingOnly = TRUE)
model_version <- if (length(args) >= 1) args[1] else "v2"
runs          <- if (length(args) >= 2) as.integer(args[2]) else 3
folds_to_run  <- if (length(args) >= 3) as.integer(strsplit(args[3], ",")[[1]]) else 1:3

dir.create("results/backtest", recursive = TRUE, showWarnings = FALSE)

all <- readRDS("data/model/train.rds")
all$date <- as.Date(substr(all$id, 1, 10))
all <- all[order(all$date, all$id, all$daysout, match(all$final_result, c("1", "X", "2"))), ]

# Fold cutoffs on game dates: train < cut_k, test in [cut_k, cut_k+1)
game_dates <- all[!duplicated(all$id), ]$date
cuts <- as.Date(quantile(as.numeric(game_dates), c(0.5, 0.67, 0.83), type = 1), origin = "1970-01-01")
cuts <- c(cuts, max(game_dates) + 1)
cat("Fold cutoffs:", format(cuts), "\n")

for (k in folds_to_run) {
  train_rows <- all[all$date < cuts[k], ]
  test_pool  <- all[all$date >= cuts[k] & all$date < cuts[k + 1], ]
  test_rows <- test_pool %>%
    group_by(id, final_result) %>%
    slice_min(order_by = daysout, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    as.data.frame()
  test_rows <- test_rows[order(test_rows$date, test_rows$id), ]
  fold_df <- rbind(train_rows, test_rows)
  start <- nrow(train_rows) + 1
  end   <- nrow(fold_df)
  cat(sprintf("Fold %d (%s): train rows %d (%d games), test rows %d (%d games)\n", k, model_version,
              nrow(train_rows), length(unique(train_rows$id)), nrow(test_rows), length(unique(test_rows$id))))

  if (model_version == "v1") {
    d <- generate_train_test_data(fold_df, start, end)
  } else {
    d <- generate_train_test_data_v2(fold_df, start, end)
  }

  pcts <- matrix(NA_real_, nrow = runs, ncol = nrow(test_rows))
  for (i in seq_len(runs)) {
    t0 <- Sys.time()
    set_random_seed(1000 * k + i)
    if (model_version == "v1") {
      model <- compile_model_live(d$X_seq_train, d$X_static_train, d$y_train)
    } else {
      model <- compile_model_live_v2(d$X_seq_train, d$X_static_train, d$y_train)
    }
    pcts[i, ] <- model %>% predict(list(d$X_seq_test, d$X_static_test), verbose = 0)
    k_clear_session()
    cat(sprintf("  run %d done in %.1f min\n", i, as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  }

  out <- test_rows[, c("id", "date", "league", "daysout", "final_result", "odds", "payoff", "ndays")]
  out$p_runs_sd <- apply(pcts, 2, sd)
  out$p <- colMeans(pcts)
  out$fold <- k
  saveRDS(out, sprintf("results/backtest/%s_fold%d.rds", model_version, k))
}
