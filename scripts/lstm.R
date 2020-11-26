library(tidyverse)
library(tibble)
library(lubridate)
library(recipes)
library(hablar)
library(timetk)
library(rsample)
library(tibbletime)
library(keras)
#library(mapfuser)
library(anytime)
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


# create variable day to include different drives
ul_data$day <- ul_data$timestamp_ms %>% anytime() %>% day

ul_data_cropped <- ul_data[, c("throughput_mbits", "payload_mb", "")]
ul_data_cropped[c("throughput_mbits", "payload_mb")] <- 
  scale(ul_data_cropped[c("throughput_mbits", "payload_mb")])
ul_data_cropped <- dummy_cols(ul_data_cropped)
t_steps <- 7
steps_out <- 2

X <- array(dim=c(0,t_steps,ncol(ul_data_cropped)-1))
y <- array(dim=c(0, 2))
for (i in 1:(194+t_steps)){
  end_in <- i+t_steps-1
  end_out <- i+steps_out-1
  if (end_out > (194+t_steps)) break
  seq_x <- array(
    ul_data_cropped[i:(end_in), 
            names(ul_data_cropped) != "throughput_mbits" & names(ul_data_cropped) != "provider"], 
    dim=c(1, t_steps, ncol(ul_data_cropped)-1))
  seq_y <- array(ul_data_cropped[i:end_out, "throughput_mbits"], 
                 dim=c(1, steps_out, 1))
  X <- abind(X, seq_x, along = 1)
  y <- abind(y, seq_y, along = 1)
}


# 5.1.6 LSTM Model
batch_size = 2
epochs = 50

model <- keras_model_sequential()

model %>%
  layer_lstm(units            = 50, 
             activation       = "relu", 
             input_shape      = c(t_steps, dim(X)[3]),
             batch_size       = batch_size,
             return_sequences = TRUE, 
             stateful         = TRUE) %>% 
  layer_lstm(units            = 50, 
             activation       = "relu",
             return_sequences = FALSE, 
             stateful         = TRUE) %>% 
  layer_dense(units = 2)

model %>% 
  compile(loss = 'mae', optimizer = 'adam')

# 5.1.7 Fitting LSTM
for (i in 1:epochs) {
  model %>% fit(x          = X[1:198, , ], 
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
  predict(X[181:200, , ], batch_size) #%>%
  #.[,1] 
pred_out
y[181:200]

ggplot(data.frame(actual=y[181:200], predict=pred_out), aes(x=actual, y=predict)) + 
         geom_point()

