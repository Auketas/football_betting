# Compare the ablation variants (results/backtest_ablation) with the full v2 and v1 backtests.
# Usage: Rscript code/model/backtest_ablation_analysis.R
suppressMessages(library(dplyr))

load_set <- function(dir, pattern) bind_rows(lapply(list.files(dir, pattern = pattern, full.names = TRUE), function(f) {
  d <- readRDS(f); d$model <- sub("_fold.*", "", basename(f)); d
}))
preds <- bind_rows(load_set("results/backtest_ablation", "^[a-z_]+_fold\\d[.]rds$"),
                   load_set("results/backtest", "^v[12]_fold\\d[.]rds$")[, c("id","date","league","daysout","final_result","odds","payoff","ndays","fold","p","model")])
preds <- preds %>% group_by(model, id) %>% mutate(mkt_p = (1 / odds) / sum(1 / odds)) %>% ungroup()
preds$won <- as.numeric(preds$payoff > 0)
preds$model <- recode(preds$model, v1 = "v1_lstm", v2 = "v2_full_lstm")

set.seed(4)
boot <- function(b) { g <- split(b$payoff, b$id); x <- replicate(500, mean(unlist(g[sample(length(g), replace = TRUE)]))); unname(quantile(x, c(.025, .975))) }
row <- function(b) { if (nrow(b) == 0) return(data.frame(n = 0, roi = NA, lo = NA, hi = NA)); ci <- boot(b); data.frame(n = nrow(b), roi = mean(b$payoff), lo = ci[1], hi = ci[2]) }

cat("=== Log loss / Brier (market = normalised bookmaker odds) ===\n")
print(preds %>% group_by(model) %>% summarise(logloss = -mean(won * log(p) + (1 - won) * log(1 - p)),
        brier = mean((p - won)^2), logloss_market = -mean(won * log(mkt_p) + (1 - won) * log(1 - mkt_p)), n = n()) %>% as.data.frame(), digits = 4)

for (delta in c(0.03, 0.05)) {
  cat(sprintf("\n=== Rule: bet when p - market p > %.2f, odds < 10 ===\n", delta))
  print(bind_rows(lapply(unique(preds$model), function(m) cbind(model = m, row(preds[preds$model == m & preds$p - preds$mkt_p > delta & preds$odds < 10, ])))), digits = 3, row.names = FALSE)
}
cat("\n=== Same rule (delta 0.05), per fold: ROI (n) ===\n")
print(preds[preds$p - preds$mkt_p > 0.05 & preds$odds < 10, ] %>% group_by(model, fold) %>%
        summarise(n = n(), roi = mean(payoff), .groups = "drop") %>% as.data.frame(), digits = 3)
cat("\n=== EV rule: bet when p*odds - 1 > 0.05, odds < 10 ===\n")
print(bind_rows(lapply(unique(preds$model), function(m) cbind(model = m, row(preds[preds$model == m & preds$p * preds$odds - 1 > 0.05 & preds$odds < 10, ])))), digits = 3, row.names = FALSE)
cat("\n=== Overlap: share of v2_full bets (delta 0.05) that the static_nn also bets on ===\n")
a <- preds[preds$model == "v2_full_lstm" & preds$p - preds$mkt_p > 0.05 & preds$odds < 10, ]
b <- preds[preds$model == "static_nn" & preds$p - preds$mkt_p > 0.05 & preds$odds < 10, ]
cat("v2_full bets:", nrow(a), " static_nn bets:", nrow(b), " overlap:", sum(paste(a$id, a$final_result) %in% paste(b$id, b$final_result)), "\n")
