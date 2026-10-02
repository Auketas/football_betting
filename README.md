# football_betting

Scrapes football (soccer) 1X2 betting odds from [betexplorer.com](https://www.betexplorer.com) every day, builds a
history of how the odds of each upcoming game move in the days before kickoff, and trains a model to find bets whose
true win probability is higher than the bookmakers' odds imply.

> **Status:** experimental. Backtests (see below) show a possible edge for the v2 model, but it has not been
> validated out of sample. The live pipeline exists to collect that evidence. Nothing here is betting advice.

## Overview

```
GitHub Actions (twice daily)          local machine (daily)
┌──────────────────────────┐          ┌────────────────────────────────────────────────┐
│ code/scrape/             │  commit  │ code/model/run_daily.R                         │
│   daily_scraper.R        │ ───────► │  1. build train/test sets from data/new/V2     │
│ → data/new/V2/*.csv      │          │  2. settle earlier bets with new results       │
│ → data/log/scraper_log   │          │  3. retrain v2 model, predict, apply bet rule  │
└──────────────────────────┘          │ → results/bets_log.csv, predictions_log.csv    │
                                      └────────────────────────────────────────────────┘
```

## 1. Scraping (`code/scrape/`)

`.github/workflows/scrape.yml` runs `code/scrape/daily_scraper.R` at 03:00 and 15:00 UTC and commits the new data.

- **Leagues:** the ~115 leagues in `data/hold/leaguelist.csv` (league URL + timezone).
- **Upcoming games:** for each game within 21 days, the match page is opened (headless Chrome via `chromote`) and the
  odds of every bookmaker are read. Per outcome (home / draw / away) the scraper stores the **best (highest) odds**, the
  **standard deviation across bookmakers**, the **number of bookmakers offering the best price** and **which bookmaker**
  when there is only one.
- **One CSV per league** in `data/new/V2/<Country>_<league>.csv`, one row per game (`id` = `date_home_away`). Columns are
  suffixed `_l20 … _l0` = days before the game, so each daily run fills in that day's column and a game accumulates up to
  21 days of odds history. The `result` column (`1`, `X`, `2`) is filled in when the game has finished.
  The V2 files have 256 columns; the old `data/new/*.csv` (V1, 128 columns, no leader columns) and `data/old` are legacy.
- **Checks:** `run_fatal_checks` blocks writing a league file if columns are inconsistent, implied probabilities do not sum
  to 0.9–1.1, ids are duplicated or the row count falls. Each run appends a summary to `data/log/scraper_log.xlsx`.

Known data limitations: games often have few observations (the fixtures page only lists games a few days ahead; about 19%
of finished games have a single observation, concentrated in some leagues), ~10% of past games never get a result, and
most data is from March 2026 onwards.

## 2. Building the model data (`write_to_train_test()` in `code/scrape/scraping.R`)

`write_to_train_test("V2")` reads `data/new/V2/*.csv` (columns selected **by name**) and writes two files to `data/model/`:

| File | Content |
|---|---|
| `train.rds` | finished games: one row per game × days-out × outcome (`1`, `X`, `2`) for every day on which all three odds were observed |
| `test.rds`  | upcoming games (dated today or later): latest observation of each game, one row per outcome |

Row fields: `id`, `league`, `daysout`, `outcome` (final result), `odds_history` / `sd_history` (21-step vectors, `NA` = not
observed), `final_result` (the outcome this row is a bet on), `payoff` (odds − 1 if it wins, else −1), `odds`, `ndays`
(number of observed days). Duplicate game ids are dropped.

## 3. Models (`code/model/`)

Both predict, for one row (a bet on one outcome of one game on one day), the probability that the bet wins.

- **v1** (`compile_model.R`, `data_prep.R`): LSTM over the bet's *own* odds and SD history (2 features × 21 days) + a small
  dense branch for static features (days out, number of observations, league).
- **v2** (`compile_model_v2.R`, `data_prep_v2.R`): larger LSTM over six features per day, ordered **relative to the bet**
  (own log-odds, the other two outcomes' log-odds, and the three bookmaker SDs), with early stopping. Static features:
  scaled number of observations, days out, mean overround, outcome one-hot, log odds, market-implied probability and league
  (scaler fitted on training data only). v2 originally gave identical inputs to the three outcomes of a game and so
  predicted ~1/3 for everything; the outcome-aware ordering and static features fixed that.

## 4. Live pipeline (`code/model/run_daily.R`)

```bash
git pull                          # get the latest scrape
Rscript code/model/run_daily.R    # optional argument: ensemble size (default 10), ~4-5 min per run on CPU
```

1. `write_to_train_test("V2")` rebuilds `train.rds` and `test.rds` from all data scraped so far.
2. `update_bet_results()` (`update_bet_results.R`) looks up results of earlier bets and fills in `result` and `payoff`, then
   prints hit rate, ROI and profit of settled bets.
3. `generate_predictions_v2()` (`generate_predictions_v2.R`) **retrains the model from scratch** on all finished games
   (ensemble of random initialisations, averaged), predicts the upcoming games and applies the betting rule.
   It stops if the newest scrape is more than a day old (`git pull` first, or `allow_stale = TRUE`).

**Betting rule** (set in `BET_RULE` in `generate_predictions_v2.R`; decide changes in advance, not after seeing results):

- only snapshots with `daysout <= 1` (the backtest bets were on snapshots 0–1 days before kickoff for ~86% of games);
- bet an outcome when `p_model − p_market > 0.05` and `odds < 10`, where `p_market` is `1/odds` normalised over the three
  outcomes (best odds across bookmakers);
- **at most one bet per game** (the outcome with the largest edge), and **never another bet on a game already in the bet
  log** — repeated daily signals on the same game are highly correlated and would only inflate exposure;
- flat stake of 1 unit.

**Outputs** (appended on every run, commit them to keep the history):

| File | Content |
|---|---|
| `results/bets_log.csv` | every suggested bet: `run_date`, `data_as_of`, `id`, `league`, `game_date`, `outcome`, `odds`, `daysout`, `ndays`, `p_model`, `p_market`, `edge`, `ev`, `model_version`, `n_ensemble`, `rule`, `stake`, and later `result`, `payoff` |
| `results/predictions_log.csv` | every eligible prediction (all three outcomes of each game with `daysout <= 1`), flagged `bet = 1/0`, with `result`/`payoff` filled in later — allows evaluating alternative rules without waiting for new data |

**Automation:** `.github/workflows/predict.yml` ("Daily Predictions") runs this pipeline on GitHub Actions right after the
afternoon (15:00 UTC) scrape finishes, with an ensemble of 5, and commits the two log files. It can also be started manually
from the Actions tab (**Run workflow**). Bets are only logged; nothing is placed. The workflow installs CPU TensorFlow in
the runner (≈10 min setup plus ≈25 min training). The 03:00 scrape does not trigger it, so a game's bet is decided from the
afternoon snapshot.

Running twice on the same day does not duplicate bets (existing games are skipped; the prediction log replaces that day's rows).
The model is **not frozen**: it is retrained each run, as it would be live. `model_version` and `n_ensemble` record which
procedure produced each row.

### Setup

R packages: `dplyr`, `keras`, `tensorflow`, plus the scraping packages (`rvest`, `chromote`, `httr`, `lubridate`,
`openxlsx`, `assertthat`, `tictoc`, `R.utils`). TensorFlow runs from a Python virtualenv; the scripts use `RETICULATE_PYTHON`
if set, otherwise `~/tfenv` (`%USERPROFILE%\tfenv\Scripts\python.exe` on Windows). Create it with
`python -m venv ~/tfenv && ~/tfenv/Scripts/python -m pip install "tensorflow-cpu==2.15.0" "numpy<2"`
(use a short path on Windows, long paths break the TensorFlow install). The scrape workflow (`scrape.yml`) only scrapes;
the separate `predict.yml` workflow does the training and logging.

## 5. Backtesting (`code/model/backtest*.R`, results in `results/backtest*`)

- `backtest.R <v1|v2> [runs] [folds]` — walk-forward backtest with three expanding-window folds by game date (train on
  earlier games, test on later ones, one snapshot per game = its latest observation). Predictions go to `results/backtest/`.
- `backtest_analysis.R` — ROI, confidence intervals (bootstrap over games), baselines, calibration for different rules.
- `backtest_ablation.R` / `backtest_ablation_analysis.R` — same folds for models without the odds-history sequence
  (static net, logistic regressions) to see whether the history adds anything.

Findings so far (about 8,700 finished games, flat stakes, best-bookmaker odds):

- Betting everything loses ~7.5% (overround ≈ 5%); betting favourites loses ~4%.
- v1 is overconfident and no better than the market (log loss 0.598 vs 0.583 for the market); no profitable rule.
- v2 matches the market on log loss (0.583) and is well calibrated. The rule "model p exceeds market p by more than 5
  points, odds < 10" gave +8.7% ROI on 907 bets (95% CI +1.7% to +16.5%), positive in 2 of 3 folds. Among each model's
  most extreme-edge bets, v2 beat a static-feature-only model by a few ROI points at every cutoff, but not significantly.
- Caveats: the rule was chosen after seeing the results, the interval is wide, the backtest used each game's final
  snapshot (known only in hindsight), and the best-bookmaker odds may not always be obtainable. The live logs exist to test
  the rule out of sample.

## Repository layout

```
.github/workflows/scrape.yml   daily scrape (03:00 and 15:00 UTC)
.github/workflows/predict.yml  daily predictions after the 15:00 scrape (logs bets, places nothing)
code/scrape/                   scraper (scraping.R, daily_scraper.R) and dataset builder
code/model/                    models v1/v2, live pipeline, backtests, diagnostic plots
data/hold/                     league list
data/new/V2/                   current scraped data (one CSV per league)   data/new, data/old: legacy
data/model/                    train.rds / test.rds (built by write_to_train_test)
data/log/scraper_log.xlsx      scraper run log
results/                       bets_log.csv, predictions_log.csv, backtest outputs, plots
```
