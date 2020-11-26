library(tidyverse)
library(tibble)
library(keras)
library(abind)

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
features <- c("throughput_mbits", "payload_mb", "f_mhz")
train_size <- ceiling(nrow(ul_data_cropped)*0.9)
test_size <- floor(nrow(ul_data_cropped)*0.10)
# Divide data into test and train
ul_train <- ul_data[1:train_size, features]
ul_test <- ul_data[(train_size+1):(train_size+test_size), features]
ul_train_scaled <- data.frame(scale(ul_train))
ul_test_scaled <- scale(ul_test, center = attr(ul_train_scaled, "scaled:center"), 
                        scale = attr(ul_train_scaled, "scaled:scale"))

tsteps_in <- 7
tsteps_out <- 72

X_train <- array(dim=c(0,tsteps_in,ncol(ul_train_scaled)-1)) # minus throughput
X_test <- array(dim=c(0,tsteps_in,ncol(ul_data_cropped)-1)) # minus throughput
y_train <- array(dim=c(0, 72))
y_test <- array(dim=c(0, 72))
for (i in 1:nrow(ul_train_scaled)){
  end_in <- i+tsteps_in-1
  end_out <- i+tsteps_out-1
  if (end_out > (nrow(ul_train_scaled)-tsteps_in)) break
  seq_x <- array(
    ul_train_scaled[i:end_in, 
            names(ul_train_scaled) != "throughput_mbits"], 
    dim=c(1, tsteps_in, ncol(ul_train_scaled)-1))
  seq_y <- array(
    ul_train_scaled[i:end_out, "throughput_mbits"], 
    dim=c(1, tsteps_out))
  X_test <- abind(X, seq_x, along = 1)
  y_test <- abind(y, seq_y, along = 1)
}


# 5.1.6 LSTM Model
batch_size = 2
epochs = 50

model <- keras_model_sequential()

model %>%
  layer_lstm(units            = 50, 
             #activation       = "relu", 
             input_shape      = c(tsteps_in, dim(X)[3]),
             batch_size       = batch_size,
             return_sequences = TRUE, 
             stateful         = TRUE) %>% 
  layer_lstm(units            = 50, 
             #activation       = "relu",
             return_sequences = FALSE, 
             stateful         = TRUE) %>% 
  layer_dense(units = 2)

model %>% 
  compile(loss = 'mae', optimizer = 'adam')

# 5.1.7 Fitting LSTM
for (i in 1:epochs) {
  model %>% fit(x          = X[1:, , ], 
                y          = y[1:198, ], 
                batch_size = batch_size,
                epochs     = 1, 
                verbose    = 1, 
                shuffle    = FALSE)
  
  model %>% reset_states()
  cat("Epoch: ", i)
  
}

# Make Predictions
pred_out <- model %>% 
  predict(X[199:200, , ], batch_size) #%>%
  #.[,1] 
pred_out
y[199:200,]

ggplot(data.frame(actual=y[181:200], predict=pred_out), aes(x=actual, y=predict)) + 
         geom_point()

