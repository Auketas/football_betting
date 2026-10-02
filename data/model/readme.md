# data/model

Model-ready data, built from `data/new/V2/*.csv` by `write_to_train_test("V2")` (`code/scrape/scraping.R`).
Rebuilt on every run of `code/model/run_daily.R`.

- `train.rds` — finished games. One row per game × days-out × outcome (`final_result` = `1`, `X`, `2`) for every day on which
  all three odds were observed. Columns: `id`, `league`, `daysout`, `outcome` (the game's actual result), `odds_history` and
  `sd_history` (21-step list columns, index 1 = 20 days out … 21 = game day, `NA` = not observed), `final_result`, `payoff`
  (odds − 1 if that outcome won, else −1), `odds` (best odds on that day), `ndays` (observed days so far), `saveid`.
- `test.rds` — upcoming games dated today or later. Same columns without `saveid`; only the latest observation of each game
  and outcome is kept.

See the main README for how these are used.
