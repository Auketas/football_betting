plot_performance_vs_ndays <- function(betmat){
  actual <- betmat$payoff
  pred <- betmat$predicted_payoff
  gap <- actual-pred
  npoints <- betmat$ndays
  png(file="results/plots/perf_gap_vs_ndays.png")
  print(plot(gap,npoints,xlab="Actual-predicted payoff",ylab="Number of days in dataset"))
  dev.off()
  png(file="results/plots/pred_vs_ndays.png")
  print(plot(pred,npoints,xlab="Predicted payoff",ylab="Number of days in dataset"))
  dev.off()
}

plot_performance_vs_odds <- function(betmat){
  actual <- betmat$payoff
  pred <- betmat$predicted_payoff
  gap <- actual-pred
  png(file="results/plots/perf_gap_vs_odds.png")
  print(plot(gap,betmat$odds,xlab="Actual-predicted payoff",ylab="Odds"))
  dev.off()
  png(file="results/plots/pred_vs_odds.png")
  print(plot(pred,betmat$odds,xlab="Predicted payoff",ylab="Odds"))
  dev.off()
}

table_win_vs_loose <- function(betmat){
  actual_bin <- as.factor(ifelse(betmat$payoff>0,"Win","Loss"))
  pred_bin <- as.factor(ifelse(betmat$predicted_payoff>0,"Predicted win","Predicted loss"))
  png(file="results/plots/win_vs_loss_table.png")
  table(actual_bin,pred_bin)
  dev.off()
}

plot_pred_vs_actual_prob <- function(betmat){
  pcts <- betmat$predicted_pcts
  win <- ifelse(betmat$payoff>0,1,0)
  df <- betmat %>%
    mutate(
      bin = ntile(pcts, 50)  # deciles
    )
  calibration <- df %>%
    group_by(bin) %>%
    summarise(
      mean_pred = mean(pcts),
      obs_win   = mean(win),
      n         = n(),
      .groups = "drop"
    )
  png(file="results/plots/actual_vs_pred_pct.png")
  plot(
    calibration$mean_pred,
    calibration$obs_win,
    xlim = c(0, 1),
    ylim = c(0, 1),
    xlab = "Predicted probability",
    ylab = "Observed probability",
    pch = 19
  )
  
  abline(0, 1, col = "red", lty = 2)
  dev.off()
  
}

diagnostic_plots <- function(betmat){
  plot_performance_vs_ndays(betmat)
  plot_performance_vs_odds(betmat)
  table_win_vs_loose(betmat)
  plot_pred_vs_actual_prob(betmat)
}