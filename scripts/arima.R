source("C:/Users/Laura/Documents/GitHub/fallstudien_2_projekt_1/scripts/arima_helpers.R")
library(forecast)
library(fastDummies)
library(dplyr)
library(anytime)
library(ggplot2)
library(Metrics)
library(corrplot)
library(grid)
library(ggcorrplot)

setwd("~/GitHub/fallstudien_2_projekt_1/datasets")
ul_data <- read.csv2("dataset_ul.csv", header=TRUE, sep=",", dec=".")

# Divide data by provider
vodafone <- ul_data[ul_data$provider == "vodafone", ]
vodafone$provider <- "Vodafone"
tmobile <- ul_data[ul_data$provider == "tmobile", ]
tmobile$provider <- "T-Mobile"
o2 <- ul_data[ul_data$provider == "o2", ]
o2$provider <- "O2"
providers <- list("vodafone" = vodafone, "tmobile" = tmobile, "o2" = o2)

# Sort Data by timestamp
providers <- lapply(providers, function(provider) provider[order(provider$timestamp), ])

# add timestamp and index
for (i in 1:length(providers)){
  providers[[i]]$timestamp <- providers[[i]]$timestamp_ms %>% anytime()
  providers[[i]]$index <- 1:nrow(providers[[i]])
}

# divide in train und test
train <- lapply(providers, function(provider) 
  provider[provider["drive_id"] != 9 & provider["drive_id"] != 10, ])
test <- lapply(providers, function(provider) 
  provider[provider["drive_id"] == 9 | provider["drive_id"] == 10, ])

# Graphs
# Throughput
#lapply(providers, function(provider)
#  ggplot(provider, 
#         aes(x=format(timestamp, "%Y-%m-%d"), y=format(timestamp, "%H:%M:%S"))) + 
#           geom_line() + facet_wrap(~scenario) + 
#           ggtitle(unique(provider$provider)))

#ggplot(providers[[1]], aes(x=format(timestamp, "%m-%d"), y=format(timestamp, "%H:%M:%S"))) + 
#  geom_line() + facet_wrap(~scenario)

lapply(train, function(provider) 
  ggplot(provider, 
         aes(x=timestamp, y=throughput_mbits)
         ) + 
    geom_line()  + 
    facet_wrap(~scenario) +
    ggtitle(unique(provider$provider))
  )

lapply(train, function(provider) 
  ggplot(provider, 
         aes(x=index, y=throughput_mbits)
         ) + 
    geom_line()  + 
    facet_wrap(~scenario) + 
    ggtitle(unique(provider$provider))
)


# Acf und pAcf
throughputs <- list(vodafone = train$vodafone$throughput_mbits, 
                    tmobile = train$tmobile$throughput_mbits, 
                    o2 = train$o2$throughput_mbits)
plot_acf(throughputs, type="acf")
plot_acf(throughputs, type="pacf")

# Cross Correlation
features <- c("throughput_mbits", "payload_mb", "f_mhz", 
              "rsrp_dbm", "rsrq_db", "ta", "velocity_mps")
lapply(train, function(provider) plot_ccf(provider, features, lag.max = 10))

# Select features
features <- c("throughput_mbits", "payload_mb", "f_mhz", 
              "rsrp_dbm", "rsrq_db", "ta", "velocity_mps")
numeric_features <- features[as.vector(unlist(lapply(train[[1]][, features], is.numeric)))]
ycol <- "throughput_mbits"

# TODO: facet wrap
lapply(
  train,
  function(provider) ggcorrplot(cor_pmat(provider[, numeric_features]), title=unique(provider$provider))
)

# Check if chr columns in data
chr_features <- features[as.vector(unlist(lapply(train[[1]][, features], is.character)))]

X <- lapply(train, function(provider) provider[, features])

if(length(chr_features) > 0){
  lapply(train, 
         function(provider) dummy_cols(provider, 
                                       remove_first_dummy = TRUE, 
                                       remove_selected_columns = TRUE)) 
}

name_mapping <- list("Vodafone" = "vodafone", 
                     "T-Mobile" = "tmobile", 
                     "O2" = "o2")

ntrain <- lapply(X, function(provider) floor(nrow(provider)*0.8))
ntest <- list(vodafone = nrow(train$vodafone)-ntrain$vodafone, 
              tmobile = nrow(train$tmobile)-ntrain$tmobile, 
              o2 = nrow(train$o2)-ntrain$o2)

splits <- list()
for (provider in names(train)){
  splits[[paste(provider)]] <- prepare_data(data = train[[provider]], 
                                          ntrain = ntrain[[provider]], 
                                          ntest = ntest[[provider]])
}

# Model
if (any(features=="throughput_mbits")) 
  features <- features[-which(features=="throughput_mbits")]
models <- list()

for (provider in names(train)){
  splits[[provider]][["Xtrain"]][, features] <- sapply(
    splits[[provider]][["Xtrain"]][, features], as.numeric )
  models[[paste(provider)]] <- auto.arima(
    splits[[provider]][["ytrain"]], 
    xreg = splits[[provider]][["Xtrain"]][, features])
}

arimaorder(model)

# Prediction
prediction <- forecast(model, xreg = Xtest)

# Reverse scaling
pred_values <- as.numeric(prediction$mean * scale_history["throughput_mbits"] + center_history["throughput_mbits"])
actual <- X[(ntrain+1):(ntrain+ntest), ycol]

plot_data <- data.frame(actual = actual, predict = pred_values)
ggplot(
  data = plot_data, aes(x=actual, y=pred_values)
  ) + 
  geom_point() + 
  xlim(min(plot_data), max(plot_data)) + 
  ylim(min(plot_data), max(plot_data))

plot_data <- rbind(data.frame(value=pred_values, type="predict", index=1:length(pred_values)), 
                  data.frame(value=actual, type="actual", index=1:length(actual)))

ggplot(plot_data, aes(x=index, y=value, color=type)) + geom_line()

rmse(actual, pred_values)
mae(actual, pred_values)  
cor(actual, pred_values)^2
