format_seq_data <- function(allgames){
  max_timesteps <- 21
  mask_value <- -99  # A value that cannot occur in real odds
  
  # Convert list-column of vectors into matrix
  X_list <- lapply(allgames$odds_history, function(vec) {
    x <- rep(mask_value, max_timesteps)
    len <- length(vec)
    x[1:len] <- ifelse(is.na(vec), mask_value, vec)
    return(x)
  })
  X <- do.call(rbind, X_list)
  
  # Preprocess sd_history (same logic)
  SD_list <- lapply(allgames$sd_history, function(vec) {
    x <- rep(mask_value, max_timesteps)
    len <- length(vec)
    x[1:len] <- ifelse(is.na(vec), mask_value, vec)
    return(x)
  })
  SD <- do.call(rbind, SD_list)
  
  # Combine both features into one 3D array: (samples, timesteps, features)
  X_array <- array(
    c(X, SD),
    dim = c(nrow(X), max_timesteps, 2)  # 2 features: odds_history + sd_history
  )
  return(X_array)
}

format_static_data <- function(allgames){
  ndays <- allgames$ndays
  daysout <- allgames$daysout
  ndays <- scale(ndays)
  daysout <- scale(daysout)
  league_mat <- model.matrix(~ league - 1, data = allgames)
  X_static <- cbind(ndays, daysout, league_mat)
  return(X_static)
}

generate_train_test_data <- function(allgames,start,end){
  X_seq <- format_seq_data(allgames)
  X_static <- format_static_data(allgames)
  y <- ifelse(allgames$payoff==-1,0,1)
  #X_train <- X_array[1:(start-1),,]
  y_train <- y[1:(start-1)]
  #X_test <- X_array[start:end,,]
  y_test <- y[start:end]
  
  X_static_train <- X_static[1:(start-1), ]
  X_static_test  <- X_static[start:end, ]
  X_seq_train <- X_seq[1:(start-1),,]
  X_seq_test  <- X_seq[start:end,,]
  return(list(y_train=y_train,y_test=y_test,X_static_train=X_static_train,X_static_test=X_static_test,X_seq_train=X_seq_train,X_seq_test=X_seq_test))
}

