# Fill in results and payoffs for logged bets / predictions once the games have finished, and print a summary.
# Results come from the `result` column of the scraped league files (data/new/<version>/<league>.csv).
#
# Usage: source this file and call update_bet_results(); also run by code/model/run_daily.R.

suppressMessages(library(dplyr))

settle_log <- function(path, version) {
  if (!file.exists(path)) return(NULL)
  text_cols <- c("run_date", "data_as_of", "id", "league", "game_date", "outcome", "model_version", "rule", "result")
  log <- suppressWarnings(read.csv(path, stringsAsFactors = FALSE, colClasses = setNames(rep("character", length(text_cols)), text_cols)))
  log$payoff <- as.numeric(log$payoff)
  todo <- is.na(log$result) | log$result == ""
  for (lg in unique(log$league[todo])) {
    f <- sprintf("data/new/%s/%s.csv", version, lg)
    if (!file.exists(f)) next
    res <- read.csv(f, colClasses = c(id = "character", result = "character"))[, c("id", "result")]
    res <- res[!is.na(res$result) & res$result != "" & !duplicated(res$id), ]
    rows <- which(todo & log$league == lg)
    m <- match(log$id[rows], res$id)
    ok <- !is.na(m)
    log$result[rows[ok]] <- res$result[m[ok]]
  }
  settled <- !is.na(log$result) & log$result != ""
  log$payoff[settled] <- ifelse(log$result[settled] == log$outcome[settled], log$odds[settled] - 1, -1)
  write.csv(log, path, row.names = FALSE)
  log
}

update_bet_results <- function(bets_file = "results/bets_log.csv", predictions_file = "results/predictions_log.csv", version = "V2") {
  settle_log(predictions_file, version)
  bets <- settle_log(bets_file, version)
  if (is.null(bets)) { cat("No bets logged yet.\n"); return(invisible(NULL)) }
  done <- bets[!is.na(bets$payoff), ]
  cat(sprintf("\nBet log: %d bets, %d settled, %d pending\n", nrow(bets), nrow(done), nrow(bets) - nrow(done)))
  if (nrow(done) > 0) {
    cat(sprintf("Settled: hit rate %.1f%%, ROI %.1f%% (flat 1-unit stakes), profit %.2f units\n",
                100 * mean(done$payoff > 0), 100 * mean(done$payoff), sum(done$payoff)))
    cat(sprintf("Mean model p of settled bets %.3f vs market p %.3f vs observed win rate %.3f\n",
                mean(done$p_model), mean(done$p_market), mean(done$payoff > 0)))
  }
  invisible(bets)
}
