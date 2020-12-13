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
library(car)
library(sugrrants)

setwd("~/GitHub/fallstudien_2_projekt_1/datasets")
ul_data <- read.csv2("dataset_ul.csv", header=TRUE, sep=",", dec=".")
ul_data <- na.omit(ul_data)

# Divide data by provider
vodafone <- ul_data[ul_data$provider == "vodafone", ]
tmobile <- ul_data[ul_data$provider == "tmobile", ]
o2 <- ul_data[ul_data$provider == "o2", ]
providers <- list("vodafone" = vodafone, "tmobile" = tmobile, "o2" = o2)

# Features
features <- c("throughput_mbits", "payload_mb", "f_mhz", "rsrp_dbm", "rsrq_db",
              "cqi", "ta", "velocity_mps", "scenario")
numeric_features <- features[as.vector(unlist(lapply(train[[1]][, features], 
                                                     is.numeric)))]

# in Training und Test aufteilen
train <- lapply(providers, function(provider) 
  provider[
    provider["drive_id"] != 8 & provider["drive_id"] != 9 & 
      provider["drive_id"] != 10, features])
test <- lapply(providers, function(provider) 
  provider[
    provider["drive_id"] == 8 | provider["drive_id"] == 9 | 
      provider["drive_id"] == 10, features])


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
  for (i in c(features)){
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
lm_vodafone <- lm(throughput_mbits ~ ., data = train[["vodafone"]])
VIF(lm_vodafone)

train[["tmobile"]]$scenario <- factor(train[["tmobile"]]$scenario)
lm_tmobile <- lm(throughput_mbits ~ ., data = train[["tmobile"]])
VIF(lm_tmobile)

train[["o2"]]$scenario <- factor(train[["o2"]]$scenario)
lm_o2 <- lm(throughput_mbits ~ ., data = train[["o2"]])
VIF(lm_o2)

# ohne RSRQ ohne Frequenz
features <- c("throughput_mbits", "payload_mb", "rsrp_dbm", 
              "cqi", "ta", "velocity_mps", "scenario")

train[["vodafone"]] <- train[["vodafone"]][, features]
lm_vodafone <- lm(throughput_mbits ~ ., data = train[["vodafone"]])
VIF(lm_vodafone)
res_vodafone <- data.frame(res=rstandard(lm_vodafone), provider="Vodafone")

train[["tmobile"]] <- train[["tmobile"]][, features]
lm_tmobile <- lm(throughput_mbits ~ ., data = train[["tmobile"]])
VIF(lm_tmobile)
res_tmobile <- data.frame(res=rstandard(lm_tmobile), provider="T-Mobile")

train[["o2"]] <- train[["o2"]][, features]
lm_o2 <- lm(throughput_mbits ~ ., data = train[["o2"]])
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

numeric_features <- features[as.vector(unlist(lapply(train[[1]][, features], 
                                                     is.numeric)))]
cv_train <- lapply(train, function(provider) 
  provider[provider["drive_id"] == 1 | provider["drive_id"] == 2, lm_features])

for (test_id in 3:7){
  
  if(test_id > 3){
    cv_train <- lapply(train, function(provider) 
      rbind(cv_train, provider[provider["drive_id"] == test_id-1, lm_features]))
  }
  cv_test <- lapply(train, function(provider) 
    provider[provider["drive_id"] == test_id, lm_features])
  
  vodafone_kennzahlen <- list()
  tmobile_kennzahlen <- list()
  o2_kennzahlen <- list()
  for (provider in c("vodafone", "tmobile", "o2")){
    all_mse <- data.frame(NA, row.names = as.character())
    
    # fit model
    y <- ts(cv_train[[provider]][, "throughput_mbits"])
    xreg <- cv_train[[provider]][, lm_features[-which(lm_features=="throughput_mbits")]]
    xreg <- dummy_cols(xreg, remove_first_dummy = TRUE, remove_selected_columns = TRUE)
    xreg <- data.matrix(xreg)
    arima_fit <- Arima(y=y, order=c(2,0,2), xreg=xreg)
    # predict
    y <- ts(cv_test[[provider]][, "throughput_mbits"])
    xreg <- cv_test[[provider]][, lm_features[-which(lm_features=="throughput_mbits")]]
    xreg <- dummy_cols(xreg, remove_first_dummy = TRUE, remove_selected_columns = TRUE)
    xreg <- data.matrix(xreg)
    pred <- forecast(arima_fit, xreg = xreg)
    res <- unclass(y) - unclass(pred$mean)
    mse[[paste(test_id)]] <- mse(unclass(y), unclass(pred$mean))
  }
}

