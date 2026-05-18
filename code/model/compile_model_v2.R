compile_model_v2 <- function(X_seq_train, X_seq_test, X_static_train, X_static_test, y_train, y_test) {
  max_timesteps  <- 21
  n_seq_features <- 6
  mask_value     <- -9999

  seq_input <- layer_input(shape = c(max_timesteps, n_seq_features), name = "sequence_input")

  seq_branch <- seq_input %>%
    layer_masking(mask_value = mask_value) %>%
    layer_lstm(units = 64, dropout = 0.2, recurrent_dropout = 0.2) %>%
    layer_dense(units = 32, activation = "relu") %>%
    layer_dropout(rate = 0.3)

  static_input <- layer_input(shape = ncol(X_static_train), name = "static_input")

  static_branch <- static_input %>%
    layer_dense(units = 16, activation = "relu") %>%
    layer_dropout(rate = 0.2)

  combined <- layer_concatenate(list(seq_branch, static_branch)) %>%
    layer_dense(units = 32, activation = "relu") %>%
    layer_dropout(rate = 0.2) %>%
    layer_dense(units = 1, activation = "sigmoid")

  model <- keras_model(
    inputs  = list(seq_input, static_input),
    outputs = combined
  )

  model %>% compile(
    optimizer = optimizer_adam(learning_rate = 1e-3),
    loss      = "binary_crossentropy",
    metrics   = "accuracy"
  )

  model %>% fit(
    x = list(sequence_input = X_seq_train, static_input = X_static_train),
    y = y_train,
    epochs          = 100,
    batch_size      = 64,
    validation_data = list(list(X_seq_test, X_static_test), y_test),
    callbacks = list(
      callback_early_stopping(
        monitor             = "val_loss",
        patience            = 5,
        restore_best_weights = TRUE
      ),
      callback_reduce_lr_on_plateau(
        monitor = "val_loss",
        factor  = 0.5,
        patience = 3
      )
    )
  )

  model
}

compile_model_live_v2 <- function(X_seq_train, X_static_train, y_train) {
  max_timesteps  <- 21
  n_seq_features <- 6
  mask_value     <- -9999

  seq_input <- layer_input(shape = c(max_timesteps, n_seq_features), name = "sequence_input")

  seq_branch <- seq_input %>%
    layer_masking(mask_value = mask_value) %>%
    layer_lstm(units = 64, dropout = 0.2, recurrent_dropout = 0.2) %>%
    layer_dense(units = 32, activation = "relu") %>%
    layer_dropout(rate = 0.3)

  static_input <- layer_input(shape = ncol(X_static_train), name = "static_input")

  static_branch <- static_input %>%
    layer_dense(units = 16, activation = "relu") %>%
    layer_dropout(rate = 0.2)

  combined <- layer_concatenate(list(seq_branch, static_branch)) %>%
    layer_dense(units = 32, activation = "relu") %>%
    layer_dropout(rate = 0.2) %>%
    layer_dense(units = 1, activation = "sigmoid")

  model <- keras_model(
    inputs  = list(seq_input, static_input),
    outputs = combined
  )

  model %>% compile(
    optimizer = optimizer_adam(learning_rate = 1e-3),
    loss      = "binary_crossentropy",
    metrics   = "accuracy"
  )

  # Hold out the most-recent 10 % of training rows for early stopping.
  # With temporally ordered data this means the validation set is always
  # the most recent games, which is the most relevant signal.
  model %>% fit(
    x = list(sequence_input = X_seq_train, static_input = X_static_train),
    y = y_train,
    epochs           = 100,
    batch_size       = 64,
    validation_split = 0.1,
    callbacks = list(
      callback_early_stopping(
        monitor              = "val_loss",
        patience             = 5,
        restore_best_weights = TRUE
      ),
      callback_reduce_lr_on_plateau(
        monitor  = "val_loss",
        factor   = 0.5,
        patience = 3
      )
    )
  )

  model
}
