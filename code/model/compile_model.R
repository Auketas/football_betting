compile_model <- function(X_seq_train,X_seq_test,X_static_train,X_static_test,y_train,y_test){
  max_timesteps <- 21
  mask_value <- -99  # A value that cannot occur in real odds
  
  
  seq_input <- layer_input(
    shape = c(max_timesteps, 2),
    name = "sequence_input"
  )
  
  seq_branch <- seq_input %>%
    layer_masking(mask_value = mask_value) %>%
    layer_lstm(units = 32) %>%
    layer_dense(units = 16, activation = "relu")
  
  static_input <- layer_input(
    shape = ncol(X_static_train),
    name = "static_input"
  )
  
  static_branch <- static_input %>%
    layer_dense(units = 8, activation = "relu")
  
  
  combined <- layer_concatenate(list(seq_branch, static_branch)) %>%
    layer_dense(units = 16, activation = "relu") %>%
    layer_dense(units = 1, activation = "sigmoid")
  
  model <- keras_model(
    inputs = list(seq_input, static_input),
    outputs = combined
  )
  
  model %>% compile(
    optimizer = optimizer_adam(),
    loss = "binary_crossentropy",
    metrics = "accuracy"
  )
  
  history <- model %>% fit(
    x = list(
      sequence_input = X_seq_train,
      static_input = X_static_train
    ),
    y = y_train,
    epochs = 30,
    batch_size = 32,
    validation_data = list(
      list(X_seq_test, X_static_test),
      y_test
    )
  )
  return(model)
}

compile_model_live <- function(X_seq_train,X_static_train,y_train){
  max_timesteps <- 21
  mask_value <- -99  # A value that cannot occur in real odds
  
  
  seq_input <- layer_input(
    shape = c(max_timesteps, 2),
    name = "sequence_input"
  )
  
  seq_branch <- seq_input %>%
    layer_masking(mask_value = mask_value) %>%
    layer_lstm(units = 32) %>%
    layer_dense(units = 16, activation = "relu")
  
  static_input <- layer_input(
    shape = ncol(X_static_train),
    name = "static_input"
  )
  
  static_branch <- static_input %>%
    layer_dense(units = 8, activation = "relu")
  
  
  combined <- layer_concatenate(list(seq_branch, static_branch)) %>%
    layer_dense(units = 16, activation = "relu") %>%
    layer_dense(units = 1, activation = "sigmoid")
  
  model <- keras_model(
    inputs = list(seq_input, static_input),
    outputs = combined
  )
  
  model %>% compile(
    optimizer = optimizer_adam(),
    loss = "binary_crossentropy",
    metrics = "accuracy"
  )
  
  history <- model %>% fit(
    x = list(
      sequence_input = X_seq_train,
      static_input = X_static_train
    ),
    y = y_train,
    epochs = 30,
    batch_size = 32
  )
  return(model)
}