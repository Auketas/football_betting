source("code/model/compile_model.R")
source("code/model/data_prep.R")
generate_predictions <- function(){
  train <- readRDS("data/model/train.rds")
  test <- readRDS("data/model/test.rds")
  
  runs <- 10
  pcts <- matrix(nrow=runs,ncol=nrow(test))
  for(i in 1:runs){
    X_seq_train <- format_seq_data(train)
    X_seq_test <- format_seq_data(test)
    X_static_train <- format_static_data(train)
    X_static_test <- format_static_data(test)
    y_train <- ifelse(train$payoff>0,1,0)
    model <- compile_model_live(X_seq_train,X_static_train,y_train)
    pcts[i,] <- model %>% predict(
      list(X_seq_test, X_static_test)
    )
  }
  test$predicted_pct <- colMeans(pcts)
  test$predicted_payoff <- as.numeric(test$predicted_pct)*test$odds-1
  threshold <- 0.7
  bet_df <- test[test$predicted_payoff > threshold, ]
  bet_df <- bet_df[bet_df$odds<10,]
  bet_df$date <- sys.Date()
  oldbet <- read.csv("results/bets.csv")
  newbet <- rbind(bet_df,oldbet)
  write.csv(newbet,"results/bets.csv")
}