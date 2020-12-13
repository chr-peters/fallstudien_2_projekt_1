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
library(regclass)
library(sugrrants)
library(tseries)

setwd("~/GitHub/fallstudien_2_projekt_1/datasets")
ul_data <- read.csv2("dataset_ul.csv", header=TRUE, sep=",", dec=".")
ul_data <- na.omit(ul_data)

ul_data[ul_data["drive_id"]==8 & ul_data["provider"]=="vodafone" & ul_data["scenario"]=="campus", "throughput_mbits"]

# Divide data by provider
vodafone <- ul_data[ul_data$provider == "vodafone", ]
tmobile <- ul_data[ul_data$provider == "tmobile", ]
o2 <- ul_data[ul_data$provider == "o2", ]
providers <- list("vodafone" = vodafone, "tmobile" = tmobile, "o2" = o2)

# Features
features <- c("throughput_mbits", "payload_mb", "f_mhz", "rsrp_dbm", "rsrq_db",
              "cqi", "ta", "velocity_mps", "scenario", "drive_id")
lm_features <- c("throughput_mbits", "payload_mb", "f_mhz", "rsrp_dbm", "rsrq_db",
                 "cqi", "ta", "velocity_mps", "scenario")

# in Training und Test aufteilen
train <- lapply(providers, function(provider) 
  provider[
    provider["drive_id"] != 8 & provider["drive_id"] != 9 & 
      provider["drive_id"] != 10, features])
test <- lapply(providers, function(provider) 
  provider[
    provider["drive_id"] == 8 | provider["drive_id"] == 9 | 
      provider["drive_id"] == 10, features])

numeric_features <- lm_features[as.vector(unlist(lapply(train[[1]][, lm_features], 
                                                     is.numeric)))]

# ACF und pACF von throughput
throughputs <- list(vodafone = train$vodafone$throughput_mbits, 
                    tmobile = train$tmobile$throughput_mbits, 
                    o2 = train$o2$throughput_mbits)
plot_acf(throughputs, type = "acf", 
         title = "Autokorrelationsfunktionen der Variable 'throughput_mbits'")
plot_acf(throughputs, type = "pacf", 
         title = "partielle Autokorrelationsfunktionen der Variable 'throughput_mbits'")


# Test auf Stationarität: Augmented Dickey-Fuller Test
for (j in c("vodafone", "o2", "tmobile")){
  for (i in lm_features[-which(lm_features == "scenario")]){
    adf.test(train[[j]][,i])$p.value
    print(adf.test(train[[j]][,i])$p.value)
  }
}
# alle p-Werte < 0.05, dh alle Variablen sind stationär

# Scale data
for (provider in c("vodafone", "tmobile", "o2")){
  scaled <- scale(train[[provider]][, numeric_features])
  train[[provider]][, numeric_features] <- scaled
  attr(train[[provider]], "scaled:center") <- attr(scaled, "scaled:center")
  attr(train[[provider]], "scaled:scale") <- attr(scaled, "scaled:center")
  test[[provider]][, numeric_features] <- scale(test[[provider]][, numeric_features], 
                                                center = attr(scaled, "scaled:center"), 
                                                scale = attr(scaled, "scaled:scale"))
}


# Multikollinearität

train[["vodafone"]]$scenario <- factor(train[["vodafone"]]$scenario)
lm_vodafone <- lm(throughput_mbits ~ ., data = train[["vodafone"]][, lm_features])
VIF(lm_vodafone)

train[["tmobile"]]$scenario <- factor(train[["tmobile"]]$scenario)
lm_tmobile <- lm(throughput_mbits ~ ., data = train[["tmobile"]][, lm_features])
VIF(lm_tmobile)

train[["o2"]]$scenario <- factor(train[["o2"]]$scenario)
lm_o2 <- lm(throughput_mbits ~ ., data = train[["o2"]][, lm_features])
VIF(lm_o2)

# ohne RSRQ ohne Frequenz
lm_features <- c("throughput_mbits", "payload_mb", "rsrp_dbm", 
              "cqi", "ta", "velocity_mps", "scenario")

#train[["vodafone"]] <- train[["vodafone"]][, features]
lm_vodafone <- lm(throughput_mbits ~ ., data = train[["vodafone"]][, lm_features])
VIF(lm_vodafone)
res_vodafone <- data.frame(res=rstandard(lm_vodafone), provider="Vodafone")

train[["tmobile"]] <- train[["tmobile"]][, features]
lm_tmobile <- lm(throughput_mbits ~ ., data = train[["tmobile"]][, lm_features])
VIF(lm_tmobile)
res_tmobile <- data.frame(res=rstandard(lm_tmobile), provider="T-Mobile")

train[["o2"]] <- train[["o2"]][, features]
lm_o2 <- lm(throughput_mbits ~ ., data = train[["o2"]][, lm_features])
VIF(lm_o2)
res_o2 <- data.frame(res=rstandard(lm_o2), provider="O2")

## qq-Plots
res_data <- rbind(res_vodafone, res_tmobile, res_o2)
ggplot(res_data, aes(sample=res)) + geom_qq() + 
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1, alpha = 0.8) + 
  facet_wrap(~provider) + ggtitle("QQ-Plots Normalverteilung") + 
  xlab("theoretische Quantile") + ylab("Quantile der Residuen")

# Histogramm
ggplot(res_data, aes(x = res)) + geom_histogram(color="black", fill="pink") + 
  facet_wrap(~ provider) + ggtitle("Histogramme der Residuen") + 
  xlab("Residuen") + ylab("Anzahl")

# Plot ACF und pACF
plot_data <- list(vodafone = lm_vodafone$residuals, 
                        tmobile = lm_tmobile$residuals, 
                        o2 = lm_o2$residuals)
plot_acf(plot_data, type="acf", 
         title = "Autokorrelationsfunktionen der Residuen")
plot_acf(plot_data, type="pacf", 
         title = "partielle Autokorrelationsfunktionen der Residuen")

# Vodafone
# PACF 0 - 2
# ACF 0 - 5

max_ar <- 2
max_ma <- 5
nrow = (max_ar+1)*(max_ma+1)
grid_vodafone <- matrix(data = c(rep(0:max_ar, each=max_ma+1), rep(0, nrow), rep(0:max_ma, max_ar+1)), 
                        nrow = nrow, ncol = 3)


# O2
# PACF 0 - 6
# ACF 0 - 5

max_ar <- 6
max_ma <- 5
nrow = (max_ar+1)*(max_ma+1)
grid_o2 <- matrix(data = c(rep(0:max_ar, each=max_ma+1), rep(0, nrow), rep(0:max_ma, max_ar+1)), 
                        nrow = nrow, ncol = 3)

# TMobile
# PACF 0 - 3
# ACF 0 - 10

max_ar <- 3
max_ma <- 10
nrow = (max_ar+1)*(max_ma+1)
grid_tmobile <- matrix(data = c(rep(0:max_ar, each=max_ma+1), rep(0, nrow), rep(0:max_ma, max_ar+1)), 
                        nrow = nrow, ncol = 3)

grids <- list("vodafone" = grid_vodafone, 
              "tmobile" = grid_tmobile, 
              "o2" = grid_o2)

vodafone_kennzahlen <- list("mse" = data.frame(), 
                            "mae" = data.frame(), 
                            "rsquared" = data.frame())
tmobile_kennzahlen <- list("mse" = data.frame(), 
                           "mae" = data.frame(), 
                           "rsquared" = data.frame())
o2_kennzahlen <- list("mse" = data.frame(), 
                      "mae" = data.frame(), 
                      "rsquared" = data.frame())
kennzahlen <- list("vodafone" = vodafone_kennzahlen, 
                   "tmobile" = tmobile_kennzahlen, 
                   "o2" = o2_kennzahlen)
for (provider in c("vodafone", "tmobile", "o2")){
  cv_train <- train[[provider]][
      train[[provider]]["drive_id"] == 1 | train[[provider]]["drive_id"] == 2, 
      lm_features
    ]
  
  all_mse <- data.frame(
    matrix(rep(NA, 5*nrow(grids[[provider]])), nrow=nrow(grids[[provider]])), 
    row.names = as.character(1:nrow(grids[[provider]]))
  )
  colnames(all_mse) <- c(paste("test_id", as.character(3:7), sep="_"))
  
  all_mae <- data.frame(
    matrix(rep(NA, 5*nrow(grids[[provider]])), nrow=nrow(grids[[provider]])), 
    row.names = as.character(1:nrow(grids[[provider]]))
  )
  colnames(all_mae) <- c(paste("test_id", as.character(3:7), sep="_"))
  
  all_rsquared <- data.frame(
    matrix(rep(NA, 5*nrow(grids[[provider]])), nrow=nrow(grids[[provider]])), 
    row.names = as.character(1:nrow(grids[[provider]]))
  )
  colnames(all_rsquared) <- c(paste("test_id", as.character(3:7), sep="_"))
  
  for (test_id in 3:7){
    
    if(test_id > 3){
      cv_train <- rbind(cv_train, 
                        train[[provider]][
                          train[[provider]]["drive_id"] == test_id-1, lm_features
                        ])
    }
    cv_test <- train[[provider]][train[[provider]]["drive_id"] == test_id, lm_features]
    
    for (row in 1:nrow(grids[[provider]])){
      # fit model
      y <- ts(cv_train[, "throughput_mbits"])
      xreg <- cv_train[, lm_features[-which(lm_features=="throughput_mbits")]]
      xreg <- dummy_cols(xreg, remove_first_dummy = TRUE, remove_selected_columns = TRUE)
      xreg <- data.matrix(xreg)
      arima_fit <- Arima(y=y, order=grids[[provider]][row,], xreg=xreg, method="ML")
      # predict
      y <- ts(cv_test[, "throughput_mbits"])
      xreg <- cv_test[, lm_features[-which(lm_features=="throughput_mbits")]]
      xreg <- dummy_cols(xreg, remove_first_dummy = TRUE, remove_selected_columns = TRUE)
      xreg <- data.matrix(xreg)
      pred <- forecast(arima_fit, xreg = xreg)
      #res <- unclass(y) - unclass(pred$mean)
      all_mse[row, paste("test_id", test_id, sep="_")] <- mse(unclass(y), unclass(pred$mean))
      all_mae[row, paste("test_id", test_id, sep="_")] <- mse(unclass(y), unclass(pred$mean))
      all_rsquared[row, paste("test_id", test_id, sep="_")] <- cor(unclass(y), unclass(pred$mean))^2
    }
    kennzahlen[[provider]]$mse <- all_mse
    kennzahlen[[provider]]$mae <- all_mae
    kennzahlen[[provider]]$rsquared <- all_rsquared
  }
}
grids[["vodafone"]][which.min(rowMeans(kennzahlen$vodafone$mae))[[1]], ]
grids[["vodafone"]][which.min(rowMeans(kennzahlen$vodafone$mse))[[1]], ]
param_vodafone <- grids[["vodafone"]][which.min(rowMeans(kennzahlen$vodafone$rsquared))[[1]], ]

grids[["tmobile"]][which.min(rowMeans(kennzahlen$tmobile$mae))[[1]], ]
grids[["tmobile"]][which.min(rowMeans(kennzahlen$tmobile$mse))[[1]], ]
param_tmobile <- grids[["tmobile"]][which.min(rowMeans(kennzahlen$tmobile$rsquared))[[1]], ]

grids[["o2"]][which.min(rowMeans(kennzahlen$o2$mae))[[1]], ]
grids[["o2"]][which.min(rowMeans(kennzahlen$o2$mse))[[1]], ]
param_o2 <- grids[["o2"]][which.min(rowMeans(kennzahlen$o2$rsquared))[[1]], ]
parameter <- list("vodafone" = param_vodafone, 
                  "tmobile" = param_tmobile, 
                  "o2" = param_o2)

# Modell für den kompletten Trainingsdatensatz fitten und Test predicted
kennzahlen_final <- list("vodafone" = list(), 
                   "tmobile" = list(), 
                   "o2" = list())
predictions <- list("vodafone" = list(), 
                    "tmobile" = list(), 
                    "o2" = list())
for (provider in c("vodafone", "tmobile", "o2")){
  y <- ts(train[[provider]][, "throughput_mbits"])
  xreg <- train[[provider]][, lm_features[-which(lm_features=="throughput_mbits")]]
  xreg <- dummy_cols(xreg, remove_first_dummy = TRUE, remove_selected_columns = TRUE)
  xreg <- data.matrix(xreg)
  arima_fit <- Arima(y=y, order=parameter[[provider]], xreg=xreg, method="ML")
  # predict
  y <- ts(test[[provider]][, "throughput_mbits"])
  xreg <- test[[provider]][, lm_features[-which(lm_features=="throughput_mbits")]]
  xreg <- dummy_cols(xreg, remove_first_dummy = TRUE, remove_selected_columns = TRUE)
  xreg <- data.matrix(xreg)
  predictions[[provider]] <- forecast(arima_fit, xreg = xreg)

  kennzahlen_final[[provider]]$mse <- mse(unclass(y), unclass(predictions[[provider]]$mean))
  kennzahlen_final[[provider]]$mae <- mae(unclass(y), unclass(predictions[[provider]]$mean))
  kennzahlen_final[[provider]]$rsquared <- cor(unclass(y), unclass(predictions[[provider]]$mean))^2
}

# Vorhersagen zurücktransformieren
predictions <- lapply(predictions, function(provider) 
  provider$mean * attr(scaled, "scaled:scale")["throughput_mbits"] + 
    attr(scaled, "scaled:center")["throughput_mbits"]) 

# Plot
for (provider in c("vodafone", "tmobile", "o2")){
  actual <- data.frame(
    value = ul_data[(ul_data["drive_id"]==8 | ul_data["drive_id"]==9 | ul_data["drive_id"]==10) & ul_data["provider"] == provider, 
                    "throughput_mbits"], 
    type = "actual", 
    timestamp = ul_data[(ul_data["drive_id"]==8 | ul_data["drive_id"]==9 | ul_data["drive_id"]==10) & ul_data["provider"] == provider, 
                        "timestamp_ms"], 
    drive_id = ul_data[(ul_data["drive_id"]==8 | ul_data["drive_id"]==9 | ul_data["drive_id"]==10) & ul_data["provider"] == provider, 
                        "drive_id"], 
    scenario = ul_data[(ul_data["drive_id"]==8 | ul_data["drive_id"]==9 | ul_data["drive_id"]==10) & ul_data["provider"] == provider, 
                       "scenario"])
  
  vorhersage <- data.frame(
    value = predictions[[provider]], 
    type = "predict", 
    timestamp = ul_data[(ul_data["drive_id"]==8 | ul_data["drive_id"]==9 | ul_data["drive_id"]==10) & ul_data["provider"] == provider, 
                        "timestamp_ms"], 
    drive_id = ul_data[(ul_data["drive_id"]==8 | ul_data["drive_id"]==9 | ul_data["drive_id"]==10) & ul_data["provider"] == provider, 
                       "drive_id"], 
    scenario = ul_data[(ul_data["drive_id"]==8 | ul_data["drive_id"]==9 | ul_data["drive_id"]==10) & ul_data["provider"] == provider, 
                       "scenario"])
  plot_data <- rbind(actual, vorhersage)
  
  name_mapping = list(
    "vodafone" = "Vodafone", 
    "tmobile" = "T-Mobile", 
    "o2" = "O2"
  )
  
  ggplot(
    plot_data, 
    aes(x=timestamp, y=value, color=type)
    ) + 
    geom_line() + 
    facet_wrap(drive_id~scenario, scales = "free", ncol = 4) + 
    ggtitle(name_mapping[[provider]]) + 
    xlab("Zeit") + 
    ylab("Datenübertragungsrate in MBit/s")
  
  plot_data <- data.frame(actual = actual$value, 
                          predict = vorhersage$value)
  plot_data <- cbind(plot_data, 
        ul_data[(ul_data["drive_id"]==8 | ul_data["drive_id"]==9 | ul_data["drive_id"]==10) & ul_data["provider"] == provider, 
                           c("drive_id", "scenario")])
  
  ggplot(
    plot_data, 
    aes(x=actual, y=predict)
  ) + 
    geom_point() + 
    geom_abline(color="red", intercept = 0, slope = 1) +
    facet_wrap(drive_id~scenario, scales = "free", ncol = 4) + 
    ggtitle(paste("Scatterplot der Beobachtungen und der Vorhersagen:", name_mapping[[provider]])) + 
    xlab("Beobachtungen") + 
    ylab("Vorhersage")
}

# TODO: Zeitachse, labels, legende auf deutsch



