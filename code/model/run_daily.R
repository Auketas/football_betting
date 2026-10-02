# Daily live pipeline. Run from the repo root after pulling the latest scrape:
#   git pull
#   Rscript code/model/run_daily.R [runs]        # runs = ensemble size, default 10
#
# Steps: 1) rebuild train/test from data/new/V2   2) settle earlier bets with new results
#        3) retrain the model, predict upcoming games, log suggested bets
# Then commit results/bets_log.csv and results/predictions_log.csv (and data/model/*.rds if you want them tracked).

args <- commandArgs(trailingOnly = TRUE)
runs <- if (length(args) >= 1) as.integer(args[1]) else 10

suppressMessages(source("code/scrape/scraping.R"))
source("code/model/update_bet_results.R")
source("code/model/generate_predictions_v2.R")

cat("== 1/3 Building train/test data ==\n")
write_to_train_test("V2")

cat("\n== 2/3 Settling earlier bets ==\n")
update_bet_results()

cat("\n== 3/3 Training and predicting ==\n")
generate_predictions_v2(runs = runs)
