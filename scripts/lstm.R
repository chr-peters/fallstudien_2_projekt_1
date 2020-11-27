library(tidyverse)
library(tibble)
library(keras)
library(abind)
library(data.table)
library(FRACTION)

setwd("~/fallstudien_2_projekt_1/data_raw/campus")
ul_filenames <- dir("~/fallstudien_2_projekt_1/data_raw/campus") %>% 
  grep("ul", ., value=TRUE) 
providers <- c("o2", "vodafone", "tmobile")
ul_data <- data.frame()
for (provider in providers){
  ul_data <- ul_filenames %>% grep(provider, ., value=TRUE) %>%
    lapply(read.csv2, header=TRUE, sep=",", dec=".") %>% do.call(rbind, .) %>% 
    mutate(provider=provider) %>% rbind(ul_data) %>% na.omit()
}

# Scale data
features <- c("throughput_mbits", "payload_mb")
train_size <- 500 
test_size <- 20

# Divide data into test and train
ul_train <- ul_data[1:train_size, features]
ul_test <- ul_data[(train_size+1):(train_size+test_size), features]
ul_train_scaled <- scale(ul_train)
ul_test_scaled <- scale(ul_test, center = attr(ul_train_scaled, "scaled:center"), 
                        scale = attr(ul_train_scaled, "scaled:scale"))

# Save transformation
center_history <- attr(ul_train_scaled, "scaled:center")
scale_history <- attr(ul_train_scaled, "scaled:scale")

# include lag
nlag <- 2 # nlag >= test_size
ul_train_scaled <- cbind(
  ul_train_scaled, 
  "throughput_lag" = shift(ul_train_scaled[,"throughput_mbits"], nlag)
  )
ul_test_scaled <- cbind(
  ul_test_scaled, 
  "throughput_lag" = ul_train_scaled[-(1:(nrow(ul_train_scaled)-2)), "throughput_lag"]
  )

# drop nas
ul_train_scaled <- na.omit(ul_train_scaled)

# transform data into expected dimensions (r, c, h):
# - rows: number of samples
# - cols: timesteps (tsteps_in)
# - height: number of features

tsteps_in <- 3
tsteps_out <- 2

create_matrices <- function (data, tsteps_in, tsteps_out, y_col = "throughput_mbits", 
                      n_features = ncol(data)-1){
  
  X <- array(dim = c(0, tsteps_in, n_features))
  y <- array(dim = c(0, tsteps_out))
  
  for (i in 1:nrow(data)){
    end_in <- i+tsteps_in-1
    end_out <- end_in+(tsteps_out-1)
    
    seq_x <- array(
      data[i:end_in, colnames(data) != y_col], 
      dim = c(1, tsteps_in, n_features)
    )
    X <- abind(X, seq_x, along = 1)
    
    # end_out>=end_in -> no need to check end_in > nrow(data)
    if (end_out > nrow(data)) break
    
    seq_y <- array(
      data[((tsteps_in-1)+i):end_out, y_col], 
      dim = c(1, tsteps_out)
    )
    y <- abind(y, seq_y, along = 1)
  }
  
  return(list("X" = X, "y" = y))
}

train <- create_matrices(data = ul_train_scaled, tsteps_in = tsteps_in, 
                         tsteps_out = tsteps_out)
train$X <- train$X[1:nrow(train$y), , ]
test <- create_matrices(data = ul_test_scaled, tsteps_in = tsteps_in, 
                        tsteps_out = tsteps_out)

nrow(train$X)
nrow(test$X)

# 5.1.6 LSTM Model
# batch size must be divisor of nrow(traindata) and nrow(testdata)
batch_size = gcd(nrow(train$X), nrow(test$X))
epochs = 100

model <- keras_model_sequential()

model %>%
  layer_lstm(units            = 50,
             input_shape      = c(tsteps_in, dim(train$X)[3]),
             batch_size       = batch_size,
             return_sequences = TRUE, 
             stateful         = TRUE) %>% 
  layer_lstm(units            = 50,
             return_sequences = FALSE, 
             stateful         = TRUE) %>% 
  layer_dense(units = tsteps_out)

model %>% 
  compile(loss = 'mae', optimizer = 'adam')

# 5.1.7 Fitting LSTM
for (i in 1:epochs) {
  model %>% fit(x          = train$X, 
                y          = train$y, 
                batch_size = batch_size,
                epochs     = 1, 
                verbose    = 1, 
                shuffle    = FALSE)
  
  model %>% reset_states()
  cat("Epoch: ", i)
  
}

# Make Predictions
pred_out <- model %>% predict(test$X, batch_size)

pred_out
ul_test_scaled[,"throughput_mbits"]




