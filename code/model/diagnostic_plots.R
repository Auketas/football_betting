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

table_win_vs_loose <- function(testgames){
  library(gridExtra)
  actual_bin <- as.factor(ifelse(testgames$payoff>0,"Win","Loss"))
  pred_bin <- as.factor(ifelse(testgames$predicted_payoff>0,"Predicted win","Predicted loss"))
  png(file="results/plots/win_vs_loss_table.png")
  grid.table(table(actual_bin, pred_bin))
  dev.off()
}

plot_pred_vs_actual_prob <- function(testgames){
  bucketsto <- c(1:20)*0.05
  bucketsfrom <- c(0:19)*0.05
  pcts <- c()
  for(i in 1:20){
    from <- bucketsfrom[i]
    to <- bucketsto[i]
    bets <- testgames[testgames$predicted_pct>from&testgames$predicted_pct<to,]
    winpct <- sum(bets$payoff>0)/nrow(bets)
    pcts <- c(pcts,winpct)
  }
  
  png(file="results/plots/pred_vs_actual_prob.png")
  print(plot(bucketsto,pcts,xlab="predicted probability",ylab="observed probability"))
  abline(a = 0, b = 1, col = "red", lwd = 2)
  dev.off()
}

plot_prediction_vs_result_per_outcome <- function(betmat){
  actual <- betmat$payoff
  pred <- betmat$predicted_payoff
  png(file="results/plots/pred_vs_result_overall.png")
  print(plot(actual,pred,xlab="Payoff",ylab="Predicted payoff"))
  dev.off()
  
  home <- betmat[betmat$outcome=="1",]
  actual <- home$payoff
  pred <- home$predicted_payoff
  png(file="results/plots/pred_vs_result_home.png")
  print(plot(actual,pred,xlab="Payoff",ylab="Predicted payoff"))
  dev.off()
  
  draw <- betmat[betmat$outcome=="X",]
  actual <- draw$payoff
  pred <- draw$predicted_payoff
  png(file="results/plots/pred_vs_result_draw.png")
  print(plot(actual,pred,xlab="Payoff",ylab="Predicted payoff"))
  dev.off()
  
  away <- betmat[betmat$outcome=="2",]
  actual <- away$payoff
  pred <- away$predicted_payoff
  png(file="results/plots/pred_vs_result_away.png")
  print(plot(actual,pred,xlab="Payoff",ylab="Predicted payoff"))
  dev.off()
  
}

diagnostic_plots <- function(betmat,testgames){
  plot_performance_vs_ndays(betmat)
  plot_performance_vs_odds(betmat)
  table_win_vs_loose(testgames)
  plot_pred_vs_actual_prob(betmat)
  plot_prediction_vs_result_per_outcome(betmat)
}
