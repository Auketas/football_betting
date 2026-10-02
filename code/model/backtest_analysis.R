# Evaluate betting strategies on the predictions saved by code/model/backtest.R.
# Flat 1-unit stakes; ROI = mean payoff per bet. Usage: Rscript code/model/backtest_analysis.R

suppressMessages(library(dplyr))

files <- list.files("results/backtest", pattern = "^v[12]_fold\\d+\\.rds$", full.names = TRUE)
preds <- bind_rows(lapply(files, function(f) {
  d <- readRDS(f); d$model <- sub("_fold.*", "", basename(f)); d
}))

# Market-implied probability: normalise 1/odds over the three outcomes of each game
preds <- preds %>%
  group_by(model, id) %>%
  mutate(mkt_p = (1 / odds) / sum(1 / odds)) %>%
  ungroup()
preds$won <- as.numeric(preds$payoff > 0)
preds$ev <- preds$p * preds$odds - 1

set.seed(1)
summarise_bets <- function(b) {
  n <- nrow(b)
  if (n == 0) return(data.frame(n = 0, hit = NA, roi = NA, lo = NA, hi = NA))
  # bootstrap over games (bets on the same game are correlated)
  g <- split(b$payoff, b$id)
  boots <- replicate(500, {
    s <- sample(length(g), replace = TRUE)
    mean(unlist(g[s]))
  })
  data.frame(n = n, hit = mean(b$won), roi = mean(b$payoff),
             lo = unname(quantile(boots, 0.025)), hi = unname(quantile(boots, 0.975)))
}

cat("\n=== Probability quality on the test rows (lower is better) ===\n")
print(preds %>% group_by(model) %>%
  summarise(logloss_model = -mean(won * log(p) + (1 - won) * log(1 - p)),
            logloss_market = -mean(won * log(mkt_p) + (1 - won) * log(1 - mkt_p)),
            brier_model = mean((p - won)^2), brier_market = mean((mkt_p - won)^2), n = n()) %>%
  as.data.frame(), digits = 4)

cat("\n=== Baselines (all test folds pooled; same rows for every model) ===\n")
base <- preds[preds$model == unique(preds$model)[1], ]
print(cbind(strategy = "bet everything", summarise_bets(base)), digits = 3)
print(cbind(strategy = "bet favourite per game", summarise_bets(base %>% group_by(id) %>% slice_min(odds, n = 1, with_ties = FALSE) %>% ungroup())), digits = 3)
print(cbind(strategy = "bet longshots (odds>=4)", summarise_bets(base[base$odds >= 4, ])), digits = 3)

cat("\n=== Model strategies: bet when predicted EV > threshold (odds cap 10) ===\n")
res <- list()
for (m in unique(preds$model)) for (cap in c(10, 5)) for (t in c(0, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7)) {
  b <- preds[preds$model == m & preds$ev > t & preds$odds < cap, ]
  res[[length(res) + 1]] <- cbind(model = m, rule = paste0("ev>", t, " odds<", cap), summarise_bets(b))
}
print(bind_rows(res), digits = 3, row.names = FALSE)

cat("\n=== Same, per fold (ev>0.1, odds<10) ===\n")
res <- list()
for (m in unique(preds$model)) for (f in sort(unique(preds$fold))) {
  b <- preds[preds$model == m & preds$fold == f & preds$ev > 0.1 & preds$odds < 10, ]
  res[[length(res) + 1]] <- cbind(model = m, fold = f, summarise_bets(b))
}
print(bind_rows(res), digits = 3, row.names = FALSE)

cat("\n=== Edge over market: bet when model p exceeds market p by delta (odds<10) ===\n")
res <- list()
for (m in unique(preds$model)) for (d in c(0.02, 0.05, 0.1)) {
  b <- preds[preds$model == m & preds$p - preds$mkt_p > d & preds$odds < 10, ]
  res[[length(res) + 1]] <- cbind(model = m, rule = paste0("p-mkt>", d), summarise_bets(b))
}
print(bind_rows(res), digits = 3, row.names = FALSE)

cat("\n=== Calibration of the models (predicted p bucket vs observed win rate) ===\n")
print(preds %>% mutate(bucket = cut(p, c(0, .1, .2, .3, .4, .5, .6, .8, 1))) %>%
  group_by(model, bucket) %>% summarise(n = n(), mean_p = mean(p), win_rate = mean(won), mean_mkt = mean(mkt_p), .groups = "drop") %>%
  as.data.frame(), digits = 3)
