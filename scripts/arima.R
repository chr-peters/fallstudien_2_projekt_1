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
library(lubridate)

setwd("~/GitHub/fallstudien_2_projekt_1/datasets")
ul_data <- read.csv2("dataset_ul.csv", header=TRUE, sep=",", dec=".")

# Divide data by provider
vodafone <- ul_data[ul_data$provider == "vodafone", ]
tmobile <- ul_data[ul_data$provider == "tmobile", ]
o2 <- ul_data[ul_data$provider == "o2", ]
providers <- list("vodafone" = vodafone, "tmobile" = tmobile, "o2" = o2)

# Features
features <- c("throughput_mbits", "payload_mb", "f_mhz", "rsrp_dbm", "rsrq_db",
              "cqi", "ta", "velocity_mps", "scenario")

# in Training und Test aufteilen
train <- lapply(providers, function(provider) 
  provider[
    provider["drive_id"] != 8 & provider["drive_id"] != 9 & provider["drive_id"] != 10, 
    features])
test <- lapply(providers, function(provider) 
  provider[
    provider["drive_id"] == 8 |provider["drive_id"] == 9 | provider["drive_id"] == 10, 
    features])


# ACF und pACF von throughput
throughputs <- list(vodafone = train$vodafone$throughput_mbits, 
                    tmobile = train$tmobile$throughput_mbits, 
                    o2 = train$o2$throughput_mbits)
plot_acf(throughputs, type = "acf")
plot_acf(throughputs, type = "pacf")


for (provider in providers)
# Scale Data
ntrain <- floor(nrow(X)*0.9)
ntest <- nrow(X) - ntrain
scaled <- scale(X[1:ntrain, numeric_features])
center_history = attr(scaled, "scaled:center")
scale_history = attr(scaled, "scaled:scale")
traindata <- X[1:ntrain, ]
traindata[numeric_features] <- scaled
testdata <- X[(ntrain+1):(ntrain+ntest), ]
testdata[numeric_features] <- scale(testdata[numeric_features], 
                                    center = center_history, 
                                    scale = scale_history)

residuals <- list()
for (provider in providers){
  #str(provider)
  provider$scenario <- factor(provider$scenario, levels = c("campus", "highway", "urban", "suburban"))
  lmfit <- lm(throughput_mbits ~ payload_mb + f_mhz, rsrp_dbm, rsrq_db + cqi + 
                ta + velocity_mps, data = provider)
  residuals[paste(provider$provider)] <- lmfit$residuals
}
lmfit_vo <- lm(throughput_mbits~payload_mb + f_mhz + rsrp_dbm + rsrq_db + scenario, 
            data = vodafone)

ggAcf(lmfit$residuals)
pacf(lmfit$residuals)

# Sort Data by timestamp
providers <- lapply(providers, function(provider) provider[order(provider$timestamp), ])

# add timestamp and index
for (i in 1:length(providers)){
  providers[[i]]$timestamp <- providers[[i]]$timestamp_ms %>% as.POSIXct(origin="1970-01-01", tz="GMT")
  providers[[i]]$index <- 1:nrow(providers[[i]])
}

# Graphs
# Throughput
#lapply(providers, function(provider)
#  ggplot(provider, 
#         aes(x=format(timestamp, "%Y-%m-%d"), y=format(timestamp, "%H:%M:%S"))) + 
#           geom_line() + facet_wrap(~scenario) + 
#           ggtitle(unique(provider$provider)))

#ggplot(providers[[1]], aes(x=format(timestamp, "%m-%d"), y=format(timestamp, "%H:%M:%S"))) + 
#  geom_line() + facet_wrap(~scenario)

drive_id_1 <- vodafone[vodafone["drive_id"] == 1 & vodafone["scenario"] == "highway",
                       c("timestamp_ms", "throughput_mbits")]

ggplot(
  drive_id_1, 
  aes(x=timestamp_ms, y=throughput_mbits, group=1)
  ) + geom_line(size=2) + 
  ggtitle("Szenario 'Highway': Erste Testfahrt") + 
  xlab("Zeitpunkt in ms") + ylab("Datenübertragungsrate in Mbit/s") +
  theme(
    axis.text = element_text(size=20), 
    axis.title = element_text(size=30), 
    plot.title = element_text(size = 40, face = "bold"))

ggplot(
  vodafone, 
  aes(x=timestamp, y=throughput_mbits)
) + geom_point() + facet_wrap(~scenario)



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

numeric_features <- features[as.vector(unlist(lapply(train[[1]][, features], is.numeric)))]
lapply(train, function(provider) plot_ccf(provider, features, lag.max = 10))

# TODO: facet wrap
lapply(
  train,
  function(provider) ggcorrplot(cor_pmat(provider[, numeric_features]), title=unique(provider$provider))
)

lapply(
  train,
  function(provider) ggcorrplot(cor(provider[, numeric_features]), title=unique(provider$provider))
)

# Select features
features <- c("throughput_mbits", "payload_mb", "f_mhz", 
              "rsrp_dbm", "rsrq_db", "scenario")
numeric_features <- features[as.vector(unlist(lapply(train[[1]][, features], is.numeric)))]
ycol <- "throughput_mbits"

# Check if chr columns in data
chr_features <- features[as.vector(unlist(lapply(train[[1]][, features], is.character)))]

X <- lapply(train, function(provider) provider[, features])

if(length(chr_features) > 0){
  X$vodafone <- dummy_cols(X$vodafone, remove_first_dummy = TRUE, 
                                   remove_selected_columns = TRUE)
  X$tmobile <- dummy_cols(X$tmobile, remove_first_dummy = TRUE, 
                                 remove_selected_columns = TRUE)
  X$o2 <- dummy_cols(X$o2, remove_first_dummy = TRUE, 
                                  remove_selected_columns = TRUE)
}

# ARMA parameter grid
max_ar <- 5
max_ma <- 5
nrow = (max_ar+1)*(max_ma+1)
grid <- matrix(data = c(rep(0:max_ar, each=max_ma+1), rep(0, nrow), rep(0:max_ma, max_ar+1)), 
               nrow = nrow, ncol = 3)
# TODO: colnames colnames(matrix) <- c("AR", "I", "MA")

farima <- function(y, h, xreg, order) {
  x <- xreg[1:length(y), ]
  if(NROW(xreg) < length(y) + h)
    stop("Not enough xreg data for forecasting")
  xnew <- matrix(xreg[length(y) + (1:h), ], ncol = ncol(xreg))
  fit <- Arima(y, xreg = x, order = order, method = "CSS")
  forecast(fit, xreg = xnew)
  }

y <- ts(X$vodafone$throughput_mbits)[1:50]
xreg <- sapply(X$vodafone[1:100, colnames(X$vodafone)[-which(colnames(X$vodafone)=="throughput_mbits")]], as.numeric)
results <- tsCV(y = ts(X$vodafone$throughput_mbits)[1:100], 
     forecastfunction = farima,
     h = 10,
     #window = 50,
     xreg = xreg, 
     order = grid[15, ])

results[1,1]


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
