# Live prediction step: train the v2 model on all finished games, predict the upcoming games,
# apply the betting rule and append to the bet / prediction logs.
#
# Betting rule (see README):
#   * only snapshots at most `max_daysout` days before kickoff
#   * bet an outcome when model probability - market probability > `min_edge` and odds < `max_odds`
#   * at most one bet per game (the outcome with the largest edge), and never again on a game already in the bet log
#   * flat stake of 1 unit
# Market probability = (1/odds) normalised over the three outcomes (best odds across bookmakers).
#
# Outputs (appended, never overwritten):
#   results/bets_log.csv         one row per suggested bet; result/payoff filled in later by update_bet_results.R
#   results/predictions_log.csv  every eligible prediction (all three outcomes), whether or not it was bet on
#
# Usage: source this file and call generate_predictions_v2(); normally run via code/model/run_daily.R.

# TensorFlow lives in a Python virtualenv. Use RETICULATE_PYTHON if set, else ~/tfenv.
local({
  if (Sys.getenv("RETICULATE_PYTHON") == "") {
    home <- Sys.getenv(if (.Platform$OS.type == "windows") "USERPROFILE" else "HOME")
    candidates <- c(file.path(home, "tfenv", "Scripts", "python.exe"), file.path(home, "tfenv", "bin", "python"))
    found <- candidates[file.exists(candidates)]
    if (length(found) > 0) Sys.setenv(RETICULATE_PYTHON = found[1])
  }
})
suppressMessages({
  library(dplyr)
  library(keras)
  library(tensorflow)
})

source("code/model/compile_model_v2.R")
source("code/model/data_prep_v2.R")

MODEL_VERSION <- "v2-outcome-aware"
BET_RULE <- list(min_edge = 0.05, max_odds = 10, max_daysout = 1)
BETS_FILE <- "results/bets_log.csv"
PREDICTIONS_FILE <- "results/predictions_log.csv"

read_log <- function(path) {
  if (!file.exists(path)) return(NULL)
  read.csv(path, stringsAsFactors = FALSE, colClasses = c(id = "character", result = "character"))
}

generate_predictions_v2 <- function(runs = 10, rule = BET_RULE, run_date = Sys.Date(),
                                    bets_file = BETS_FILE, predictions_file = PREDICTIONS_FILE,
                                    train_path = "data/model/train.rds", test_path = "data/model/test.rds",
                                    allow_stale = FALSE) {
  train_raw <- readRDS(train_path)
  test_raw  <- readRDS(test_path)
  stopifnot(nrow(test_raw) > 0)

  # The scrape date of a snapshot is game date - days out. The newest one tells us how fresh the data is.
  data_as_of <- max(as.Date(substr(test_raw$id, 1, 10)) - test_raw$daysout)
  if (!allow_stale && data_as_of < run_date - 1) {
    stop(sprintf("Latest scraped data is from %s (run date %s). Pull the newest scrape (git pull) or set allow_stale = TRUE.",
                 data_as_of, run_date))
  }

  # Enrich each set independently so no test data influences the train set; scaler uses training data only.
  train <- enrich_with_all_outcomes(train_raw)
  test  <- enrich_with_all_outcomes(test_raw)
  scaler <- fit_static_scaler(train)

  X_seq_train    <- format_seq_data_v2(train)
  X_seq_test     <- format_seq_data_v2(test)
  X_static_train <- format_static_data_v2(train, scaler)
  X_static_test  <- format_static_data_v2(test, scaler)
  y_train        <- ifelse(train$payoff > 0, 1, 0)

  # Ensemble over random initialisations to reduce variance (seeded by date for reproducibility)
  pcts <- matrix(NA_real_, nrow = runs, ncol = nrow(test))
  for (i in seq_len(runs)) {
    set_random_seed(as.integer(format(run_date, "%Y%m%d")) + i)
    model <- compile_model_live_v2(X_seq_train, X_static_train, y_train)
    pcts[i, ] <- model %>% predict(list(X_seq_test, X_static_test), verbose = 0)
    k_clear_session()
    cat(sprintf("Ensemble run %d/%d done\n", i, runs))
  }

  pred <- data.frame(
    run_date     = as.character(run_date),
    data_as_of   = as.character(data_as_of),
    id           = test$id,
    league       = test$league,
    game_date    = substr(test$id, 1, 10),
    outcome      = test$final_result,
    odds         = test$odds,
    daysout      = test$daysout,
    ndays        = test$ndays,
    p_model      = colMeans(pcts),
    stringsAsFactors = FALSE
  )
  pred <- pred %>%
    group_by(id) %>%
    mutate(p_market = (1 / odds) / sum(1 / odds)) %>%
    ungroup() %>%
    as.data.frame()
  pred$edge <- pred$p_model - pred$p_market
  pred$ev   <- pred$p_model * pred$odds - 1

  eligible <- pred[pred$daysout <= rule$max_daysout, ]
  passes <- eligible$edge > rule$min_edge & eligible$odds < rule$max_odds

  # One bet per game: best outcome among those passing the rule, and never a game already bet on
  existing_bets <- read_log(bets_file)
  already_bet <- if (is.null(existing_bets)) character(0) else existing_bets$id
  cand <- eligible[passes & !(eligible$id %in% already_bet), ]
  new_bets <- cand %>% group_by(id) %>% slice_max(order_by = edge, n = 1, with_ties = FALSE) %>% ungroup() %>% as.data.frame()
  if (nrow(new_bets) > 0) {
    new_bets$model_version <- MODEL_VERSION
    new_bets$n_ensemble <- runs
    new_bets$rule <- sprintf("edge>%.2f;odds<%g;daysout<=%d", rule$min_edge, rule$max_odds, rule$max_daysout)
    new_bets$stake <- 1
    new_bets$result <- NA_character_
    new_bets$payoff <- NA_real_
  }

  # Prediction log: replace any rows from an earlier run on the same date, flag which rows were bets
  eligible$bet <- as.integer(paste(eligible$id, eligible$outcome) %in% paste(new_bets$id, new_bets$outcome))
  eligible$model_version <- MODEL_VERSION
  eligible$result <- NA_character_
  eligible$payoff <- NA_real_
  old_pred <- read_log(predictions_file)
  if (!is.null(old_pred)) old_pred <- old_pred[old_pred$run_date != as.character(run_date), ]
  dir.create(dirname(predictions_file), recursive = TRUE, showWarnings = FALSE)
  write.csv(bind_rows(old_pred, eligible), predictions_file, row.names = FALSE)

  if (nrow(new_bets) > 0) {
    dir.create(dirname(bets_file), recursive = TRUE, showWarnings = FALSE)
    write.csv(bind_rows(existing_bets, new_bets), bets_file, row.names = FALSE)
  }

  cat(sprintf("\nData as of %s. Eligible games (daysout <= %d): %d. New bets: %d\n",
              data_as_of, rule$max_daysout, length(unique(eligible$id)), nrow(new_bets)))
  if (nrow(new_bets) > 0) {
    print(new_bets[order(-new_bets$edge), c("game_date", "league", "id", "outcome", "odds", "p_model", "p_market", "edge", "daysout")], row.names = FALSE)
  }
  invisible(new_bets)
}
