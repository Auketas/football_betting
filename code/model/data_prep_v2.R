enrich_with_all_outcomes <- function(allgames) {
  # For each row, attach the home/draw/away odds and sd histories from the
  # matching rows (same id x daysout) so the sequence input sees the full market.
  key    <- paste0(allgames$id, "___", allgames$daysout)
  h_idx  <- which(allgames$final_result == "1")
  d_idx  <- which(allgames$final_result == "X")
  a_idx  <- which(allgames$final_result == "2")

  h_rows <- h_idx[match(key, key[h_idx])]
  d_rows <- d_idx[match(key, key[d_idx])]
  a_rows <- a_idx[match(key, key[a_idx])]

  allgames$h_odds_hist <- allgames$odds_history[h_rows]
  allgames$h_sd_hist   <- allgames$sd_history[h_rows]
  allgames$d_odds_hist <- allgames$odds_history[d_rows]
  allgames$d_sd_hist   <- allgames$sd_history[d_rows]
  allgames$a_odds_hist <- allgames$odds_history[a_rows]
  allgames$a_sd_hist   <- allgames$sd_history[a_rows]

  allgames
}

to_sequence_matrix <- function(hist_list, log_transform = FALSE) {
  max_timesteps <- 21
  mask_value    <- -9999

  do.call(rbind, lapply(hist_list, function(vec) {
    x   <- rep(mask_value, max_timesteps)
    len <- min(length(vec), max_timesteps)
    if (len == 0) return(x)
    vals <- vec[1:len]
    if (log_transform) vals <- log(pmax(vals, 1e-6))
    x[1:len] <- ifelse(is.na(vals) | !is.finite(vals), mask_value, vals)
    x
  }))
}

format_seq_data_v2 <- function(allgames) {
  # 6 features per timestep:
  #   1-3: log(home/draw/away best odds)  — log scale stabilises LSTM gradients
  #   4-6: home/draw/away bookmaker SD    — consensus signal
  max_timesteps <- 21

  H    <- to_sequence_matrix(allgames$h_odds_hist, log_transform = TRUE)
  D    <- to_sequence_matrix(allgames$d_odds_hist, log_transform = TRUE)
  A    <- to_sequence_matrix(allgames$a_odds_hist, log_transform = TRUE)
  H_sd <- to_sequence_matrix(allgames$h_sd_hist)
  D_sd <- to_sequence_matrix(allgames$d_sd_hist)
  A_sd <- to_sequence_matrix(allgames$a_sd_hist)

  array(
    c(H, D, A, H_sd, D_sd, A_sd),
    dim = c(nrow(H), max_timesteps, 6)
  )
}

compute_mean_overround <- function(allgames) {
  # Average of (1/h + 1/d + 1/a) across observed timesteps.
  # Measures bookmaker margin; thin markets tend to have higher overround.
  mapply(function(h, d, a) {
    if (is.null(h) || is.null(d) || is.null(a)) return(NA_real_)
    ov <- 1/h + 1/d + 1/a
    ov <- ov[!is.na(ov) & is.finite(ov)]
    if (length(ov) == 0) NA_real_ else mean(ov)
  }, allgames$h_odds_hist, allgames$d_odds_hist, allgames$a_odds_hist,
  SIMPLIFY = TRUE)
}

fit_static_scaler <- function(train) {
  # Compute all scaling parameters from training data only so nothing
  # from the test set leaks into normalisation.
  overround_vals <- compute_mean_overround(train)
  league_cols    <- colnames(model.matrix(~ league - 1, data = train))

  list(
    ndays_mean     = mean(train$ndays,    na.rm = TRUE),
    ndays_sd       = sd(train$ndays,      na.rm = TRUE),
    daysout_mean   = mean(train$daysout,  na.rm = TRUE),
    daysout_sd     = sd(train$daysout,    na.rm = TRUE),
    overround_mean = mean(overround_vals, na.rm = TRUE),
    overround_sd   = sd(overround_vals,   na.rm = TRUE),
    league_cols    = league_cols
  )
}

format_static_data_v2 <- function(allgames, scaler) {
  ndays_scaled     <- (allgames$ndays   - scaler$ndays_mean)   / scaler$ndays_sd
  daysout_scaled   <- (allgames$daysout - scaler$daysout_mean) / scaler$daysout_sd

  overround_vals   <- compute_mean_overround(allgames)
  overround_scaled <- (overround_vals - scaler$overround_mean) / scaler$overround_sd
  overround_scaled[is.na(overround_scaled)] <- 0

  # Build league dummies and align columns to those seen in training.
  # New leagues in test get all-zero columns; leagues only in test are dropped.
  league_mat <- model.matrix(~ league - 1, data = allgames)
  missing    <- setdiff(scaler$league_cols, colnames(league_mat))
  for (col in missing) league_mat <- cbind(league_mat, setNames(matrix(0, nrow(league_mat), 1), col))
  league_mat <- league_mat[, scaler$league_cols, drop = FALSE]

  cbind(ndays_scaled, daysout_scaled, overround_scaled, league_mat)
}

generate_train_test_data_v2 <- function(allgames, start, end) {
  allgames <- enrich_with_all_outcomes(allgames)

  X_seq  <- format_seq_data_v2(allgames)

  scaler   <- fit_static_scaler(allgames[1:(start - 1), ])
  X_static <- format_static_data_v2(allgames, scaler)

  y <- ifelse(allgames$payoff > 0, 1, 0)

  list(
    X_seq_train    = X_seq[1:(start - 1), , ],
    X_seq_test     = X_seq[start:end, , ],
    X_static_train = X_static[1:(start - 1), ],
    X_static_test  = X_static[start:end, ],
    y_train        = y[1:(start - 1)],
    y_test         = y[start:end],
    scaler         = scaler
  )
}
