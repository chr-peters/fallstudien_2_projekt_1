source("C:/Users/Alina/Documents/GitHub/fallstudien_2_projekt_1/scripts/arima_helpers.R")
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
dl_data <- read.csv2("dataset_dl.csv", header=TRUE, sep=",", dec=".") 
dl_data <- na.omit(dl_data)


## Teile Daten nach Provider auf

vodafone <- dl_data[dl_data$provider == "vodafone", ]
tmobile <- dl_data[dl_data$provider == "tmobile", ]
o2 <- dl_data[dl_data$provider == "o2", ]
providers <- list("vodafone" = vodafone, "tmobile" = tmobile, "o2" = o2)

## Features
# Nehme Features aus dem Paper (Vergleichbarkeit)

features <- c("throughput_mbits", "payload_mb", "f_mhz", "rsrp_dbm", "rsrq_db",
              "cqi", "ta", "velocity_mps", "scenario", "drive_id", "ci")

# Drive_Id geh�rt nicht in das Modell

lm_features <- c("throughput_mbits", "payload_mb", "f_mhz", "rsrp_dbm", "rsrq_db",
                 "cqi", "ta", "velocity_mps", "scenario", "ci")

## Aufteilung der Daten in Training und Test

train <- lapply(providers, function(provider) 
  provider[
    provider["drive_id"] != 8 & provider["drive_id"] != 9 & 
      provider["drive_id"] != 10, features])
test <- lapply(providers, function(provider) 
  provider[
    provider["drive_id"] == 8 | provider["drive_id"] == 9 | 
      provider["drive_id"] == 10, features])

## numerische Features

numeric_features <- lm_features[as.vector(unlist(lapply(train[[1]][, lm_features], 
                                                     is.numeric)))]

## ACF und PACF von "throughput_mbits"
throughputs <- list(vodafone = train$vodafone$throughput_mbits, 
                    tmobile = train$tmobile$throughput_mbits, 
                    o2 = train$o2$throughput_mbits)
plot_acf(throughputs, type = "acf", 
         title = "Autokorrelationsfunktionen der Variable 'throughput_mbits' - Downlink")
plot_acf(throughputs, type = "pacf", 
         title = "partielle Autokorrelationsfunktionen der Variable 'throughput_mbits' - Downlink")


## Test auf Stationarit�t: Augmented Dickey-Fuller Test

for (j in c("vodafone", "o2", "tmobile")){
  for (i in lm_features[-which(lm_features == "scenario" | lm_features == "ci")]){
    adf.test(train[[j]][,i])$p.value
    print(adf.test(train[[j]][,i])$p.value)
  }
}
# alle p-Werte < 0.05, dh alle Variablen sind station�r - ARMA Modell
# keine Kointegration n�tig

## Skalieren der Daten
for (provider in c("vodafone", "tmobile", "o2")){
  scaled <- scale(train[[provider]][, numeric_features])
  train[[provider]][, numeric_features] <- scaled
  attr(train[[provider]], "scaled:center") <- attr(scaled, "scaled:center")
  attr(train[[provider]], "scaled:scale") <- attr(scaled, "scaled:scale") 
  test[[provider]][, numeric_features] <- scale(test[[provider]][, numeric_features], 
                                                center = attr(scaled, "scaled:center"), 
                                                scale = attr(scaled, "scaled:scale"))
}


## Multikollinearit�t

train[["vodafone"]]$scenario <- factor(train[["vodafone"]]$scenario)
lm_vodafone <- lm(throughput_mbits ~ ., data = train[["vodafone"]][, lm_features])
VIF(lm_vodafone)

train[["tmobile"]]$scenario <- factor(train[["tmobile"]]$scenario)
lm_tmobile <- lm(throughput_mbits ~ ., data = train[["tmobile"]][, lm_features])
VIF(lm_tmobile)

train[["o2"]]$scenario <- factor(train[["o2"]]$scenario)
lm_o2 <- lm(throughput_mbits ~ ., data = train[["o2"]][, lm_features])
VIF(lm_o2)

# Korrelation zwischen Frequenz und Scenario bei o2 und tmobile sehr hoch
# RSRQ l�sst sich aus RSRP berechnen - Abh�ngigkeit

# f�r Konsistenz: entferne f_mhz aus allen Modellen

## ohne RSRQ und ohne Frequenz

lm_features <- c("throughput_mbits", "payload_mb", "rsrp_dbm", 
              "cqi", "ta", "velocity_mps", "scenario", "ci")

## Fitte nochmals die Modelle, ohne die beiden Features
# res_provider beinhaltet die Residuen des Modells des jeweiligen Anbieters


lm_vodafone <- lm(throughput_mbits ~ ., data = train[["vodafone"]][, lm_features])
VIF(lm_vodafone)
res_vodafone <- data.frame(res = rstandard(lm_vodafone), provider = "Vodafone")


lm_tmobile <- lm(throughput_mbits ~ ., data = train[["tmobile"]][, lm_features])
VIF(lm_tmobile)
res_tmobile <- data.frame(res = rstandard(lm_tmobile), provider = "T-Mobile")


lm_o2 <- lm(throughput_mbits ~ ., data = train[["o2"]][, lm_features])
VIF(lm_o2)
res_o2 <- data.frame(res = rstandard(lm_o2), provider = "O2")

## �berpr�fen der Normalverteilungsannahme der Residuen

# qq-Plots

res_data <- rbind(res_vodafone, res_tmobile, res_o2)
ggplot(res_data, aes(sample=res)) + geom_qq() + 
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1, alpha = 0.8) + 
  facet_wrap(~provider) + ggtitle("QQ-Plots Normalverteilung - Downlink") + 
  xlab("theoretische Quantile") + ylab("Quantile der Residuen")


# Histogramme

ggplot(res_data, aes(x = res)) + geom_histogram(color="black", fill="pink") + 
  facet_wrap(~ provider) + ggtitle("Histogramme der Residuen - Downlink") + 
  xlab("Residuen") + ylab("Anzahl")


# Ergebnisse: o2, tmobile sehen gut aus, Vodafone hat viele Ausrei�er
# Kein Test verwendet, da gro�e Stichproben dazu f�hren, dass die Tests zu schnell ablehnen
# Kolmogorov-Smirnov/Shapiro-Wilk


## Plot ACF und PACF der Residuen
# Bestimmen der Parameter f�r das ARMA Modell

plot_data <- list(vodafone = lm_vodafone$residuals, 
                        tmobile = lm_tmobile$residuals, 
                        o2 = lm_o2$residuals)
plot_acf(plot_data, type = "acf", 
         title = "Autokorrelationsfunktionen der Residuen - Downlink")
plot_acf(plot_data, type = "pacf", 
         title = "partielle Autokorrelationsfunktionen der Residuen - Downlink")

## Bestimme Parameterrange aus ACF und PACF Plots f�r das Grid zum tunen der Parmaeter p,q

# Vodafone
# PACF 0 - 7
# ACF 0 - 7

max_ar <- 7
max_ma <- 7
nrow = (max_ar+1)*(max_ma+1)
grid_vodafone <- matrix(data = c(rep(0:max_ar, each=max_ma+1), rep(0, nrow), rep(0:max_ma, max_ar+1)), 
                        nrow = nrow, ncol = 3)

# O2
# PACF 0 - 2
# ACF 0 - 2

max_ar <- 2
max_ma <- 2
nrow = (max_ar+1)*(max_ma+1)
grid_o2 <- matrix(data = c(rep(0:max_ar, each=max_ma+1), rep(0, nrow), rep(0:max_ma, max_ar+1)), 
                        nrow = nrow, ncol = 3)

# TMobile
# PACF 0 - 2
# ACF 0 - 5

max_ar <- 2
max_ma <- 5
nrow = (max_ar+1)*(max_ma+1)
grid_tmobile <- matrix(data = c(rep(0:max_ar, each=max_ma+1), rep(0, nrow), rep(0:max_ma, max_ar+1)), 
                        nrow = nrow, ncol = 3)

grids <- list("vodafone" = grid_vodafone, 
              "tmobile" = grid_tmobile, 
              "o2" = grid_o2)

## Kennzahlen: MSE, MAE, Rsquared

vodafone_kennzahlen <- list("mse" = data.frame(), 
                            "mae" = data.frame(), 
                            "rsquared" = data.frame(),
                            "aic" = data.frame())

tmobile_kennzahlen <- list("mse" = data.frame(), 
                           "mae" = data.frame(), 
                           "rsquared" = data.frame(),
                           "aic" = data.frame())
o2_kennzahlen <- list("mse" = data.frame(), 
                      "mae" = data.frame(), 
                      "rsquared" = data.frame(),
                      "aic" = data.frame())
kennzahlen <- list("vodafone" = vodafone_kennzahlen, 
                   "tmobile" = tmobile_kennzahlen, 
                   "o2" = o2_kennzahlen,
                   "aic" = data.frame())

# Erzeugen der Kennzahlen f�r die verschiedenen Provider und Testfahrten mit Zeitreihenkreuzvalidierung
# Fahrten 3:7 jeweils Test - 1 -> 1:(test_id-1) Training


for (provider in c("vodafone", "tmobile", "o2")){
  cv_train <- train[[provider]][
      train[[provider]]["drive_id"] == 1 | train[[provider]]["drive_id"] == 2, 
      lm_features
    ] # erster Trainingsdatensatz - Erweitere diesen dann immer um den Testdatensatz
  
  all_mse <- data.frame(
    matrix(rep(NA, 5*nrow(grids[[provider]])), nrow = nrow(grids[[provider]])), 
    row.names = as.character(1:nrow(grids[[provider]]))  
    # 5 Spalten (Anzahl Testsets der CV), nrow(grids[[provider]]) Zeilen (Anzahl Kombinationen) Zeilen
  )
  colnames(all_mse) <- c(paste("test_id", as.character(3:7), sep="_"))
  # Spaltennamen: aktuelle Test Id
  
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
  
  all_aic <- data.frame(
    matrix(rep(NA, 5*nrow(grids[[provider]])), nrow=nrow(grids[[provider]])), 
    row.names = as.character(1:nrow(grids[[provider]]))
  )
  colnames(all_aic) <- c(paste("test_id", as.character(3:7), sep="_"))
  
  for (test_id in 3:7){
    
    if(test_id > 3){
      cv_train <- rbind(cv_train, 
                        train[[provider]][
                          train[[provider]]["drive_id"] == test_id-1, lm_features
                        ])
    }
    cv_test <- train[[provider]][train[[provider]]["drive_id"] == test_id, lm_features]
    
    
    
    for (row in 1:nrow(grids[[provider]])){ # f�r jede Kombination aus dem Grid
      ## fit model
      y <- ts(cv_train[, "throughput_mbits"]) # konstruiere Zeitreihe
      xreg <- cv_train[, lm_features[-which(lm_features == "throughput_mbits")]] 
      xreg <- dummy_cols(xreg, remove_first_dummy = TRUE, remove_selected_columns = TRUE)
      # Dummy codierung wenn n�tig 
      xreg <- data.matrix(xreg)
      # konvertiere alle variablen zu numerischen variablen und 
      # Zusammenf�hren zu Spalten einer Matrix
      arima_fit <- Arima(y = y, order = grids[[provider]][row,], xreg = xreg, method = "ML")
      # fitte ein Arima Modell (wobei d = 0)
      
      
      ## predict
      y <- ts(cv_test[, "throughput_mbits"])
      xreg <- cv_test[, lm_features[-which(lm_features == "throughput_mbits")]]
      xreg <- dummy_cols(xreg, remove_first_dummy = TRUE, remove_selected_columns = TRUE)
      xreg <- data.matrix(xreg)
      pred <- forecast(arima_fit, xreg = xreg)
      #res <- unclass(y) - unclass(pred$mean)
      all_mse[row, paste("test_id", test_id, sep = "_")] <- mse(unclass(y), unclass(pred$mean))
      all_mae[row, paste("test_id", test_id, sep = "_")] <- mae(unclass(y), unclass(pred$mean))
      all_rsquared[row, paste("test_id", test_id, sep = "_")] <- 1 - sum((unclass(pred$mean)-unclass(y))^2)/sum((mean(unclass(y))-unclass(y))^2)
      all_aic[row, paste("test_id", test_id, sep = "_")] <- pred$model$aic
      # cor(unclass(y), unclass(pred$mean))^2
      # yq <- mean(y.test), R2 <- sum((pred.cv-yq)^2)/sum((y.test-yq)^2)
    }
    kennzahlen[[provider]]$mse <- all_mse
    kennzahlen[[provider]]$mae <- all_mae
    kennzahlen[[provider]]$rsquared <- all_rsquared
    kennzahlen[[provider]]$aic <- all_aic
    
  }
}

# Suche f�r jeden Provider die Kombination heraus, welche die besten Kennzahlen erzeugt

grids[["vodafone"]][which.min(rowMeans(kennzahlen$vodafone$mae))[[1]], ]
grids[["vodafone"]][which.min(rowMeans(kennzahlen$vodafone$mse))[[1]], ]
grids[["vodafone"]][which.max(rowMeans(kennzahlen$vodafone$rsquared))[[1]], ]
grids[["vodafone"]][which.min(rowMeans(kennzahlen$vodafone$aic))[[1]], ]
param_vodafone <- grids[["vodafone"]][which.min(rowMeans(kennzahlen$vodafone$mse))[[1]], ]

grids[["tmobile"]][which.min(rowMeans(kennzahlen$tmobile$mae))[[1]], ]
grids[["tmobile"]][which.min(rowMeans(kennzahlen$tmobile$mse))[[1]], ]
grids[["tmobile"]][which.max(rowMeans(kennzahlen$tmobile$rsquared))[[1]], ]
grids[["tmobile"]][which.min(rowMeans(kennzahlen$tmobile$aic))[[1]], ]
param_tmobile <- grids[["tmobile"]][which.min(rowMeans(kennzahlen$tmobile$mse))[[1]], ]

grids[["o2"]][which.min(rowMeans(kennzahlen$o2$mae))[[1]], ]
grids[["o2"]][which.min(rowMeans(kennzahlen$o2$mse))[[1]], ]
grids[["o2"]][which.max(rowMeans(kennzahlen$o2$rsquared))[[1]], ]
grids[["o2"]][which.min(rowMeans(kennzahlen$o2$aic))[[1]], ]
param_o2 <- grids[["o2"]][which.min(rowMeans(kennzahlen$o2$mse))[[1]], ]

parameter <- list("vodafone" = param_vodafone, 
                  "tmobile" = param_tmobile, 
                  "o2" = param_o2)
print(parameter)

## Modell f�r den kompletten Trainingsdatensatz fitten und f�r Test predicten
## Predictions zur�cktransformieren

kennzahlen_final <- list("vodafone" = list(), 
                   "tmobile" = list(), 
                   "o2" = list())
predictions <- list("vodafone" = list(), 
                    "tmobile" = list(), 
                    "o2" = list())

for (provider in c("vodafone", "tmobile", "o2")){
  y <- ts(train[[provider]][, "throughput_mbits"])
  xreg <- train[[provider]][, lm_features[-which(lm_features == "throughput_mbits")]]
  xreg <- dummy_cols(xreg, remove_first_dummy = TRUE, remove_selected_columns = TRUE)
  xreg <- data.matrix(xreg)
  arima_fit <- Arima(y = y, order = parameter[[provider]], xreg = xreg, method = "ML")
  # predict
  y <- ts(test[[provider]][, "throughput_mbits"])
  xreg <- test[[provider]][, lm_features[-which(lm_features == "throughput_mbits")]]
  xreg <- dummy_cols(xreg, remove_first_dummy = TRUE, remove_selected_columns = TRUE)
  xreg <- data.matrix(xreg)
  predictions[[provider]] <- forecast(arima_fit, xreg = xreg) 
  predictions[[provider]]$rescaled_forecast <- predictions[[provider]]$mean * attr(train[[provider]], "scaled:scale")["throughput_mbits"] + 
    attr(train[[provider]], "scaled:center")["throughput_mbits"]
  predictions[[provider]]$rescaled_y <- y * attr(train[[provider]], "scaled:scale")["throughput_mbits"] + 
    attr(train[[provider]], "scaled:center")["throughput_mbits"]
  rescaled_y <- unclass(predictions[[provider]]$rescaled_y)
  rescaled_forecast <- unclass(predictions[[provider]]$rescaled_forecast)
  kennzahlen_final[[provider]]$mse <- mse(rescaled_y, rescaled_forecast)
  kennzahlen_final[[provider]]$mae <- mae(rescaled_y, rescaled_forecast)
  kennzahlen_final[[provider]]$rsquared <- 1 - sum((rescaled_forecast-rescaled_y)^2)/
                                                 sum((mean(rescaled_y)-rescaled_y)^2) 
}


## Plot

provider <- "o2"
  
############################# Zeitreihenplot  
  
actual <- data.frame(
  value = dl_data[(dl_data["drive_id"] == 8 | dl_data["drive_id"] == 9 | dl_data["drive_id"] == 10) & dl_data["provider"] == provider, 
                  "throughput_mbits"], 
  type = "actual", 
  timestamp = anytime(dl_data[(dl_data["drive_id"] == 8 | dl_data["drive_id"] == 9 | dl_data["drive_id"] == 10) & dl_data["provider"] == provider, 
                              "timestamp_ms"]),
  drive_id = dl_data[(dl_data["drive_id"] == 8 | dl_data["drive_id"] == 9 | dl_data["drive_id"] == 10) & dl_data["provider"] == provider, 
                     "drive_id"], 
  scenario = dl_data[(dl_data["drive_id"] == 8 | dl_data["drive_id"] == 9 | dl_data["drive_id"] == 10) & dl_data["provider"] == provider, 
                     "scenario"])

vorhersage <- data.frame(
  value = predictions[[provider]]$rescaled_forecast, 
  type = "predict", 
  timestamp = anytime(dl_data[(dl_data["drive_id"]==8 | dl_data["drive_id"] == 9 | dl_data["drive_id"] == 10) & dl_data["provider"] == provider, 
                              "timestamp_ms"]),
  drive_id = dl_data[(dl_data["drive_id"]==8 | dl_data["drive_id"] == 9 | dl_data["drive_id"] == 10) & dl_data["provider"] == provider, 
                     "drive_id"], 
  scenario = dl_data[(dl_data["drive_id"]==8 | dl_data["drive_id"] == 9 | dl_data["drive_id"] == 10) & dl_data["provider"] == provider, 
                     "scenario"])
plot_data <- rbind(actual, vorhersage)

name_mapping = list(
  "vodafone" = "Vodafone", 
  "tmobile" = "T-Mobile", 
  "o2" = "O2"
)

ggplot(
  plot_data, 
  aes(x = timestamp, y = value, color = type)
) + 
  geom_line() + 
  facet_wrap(drive_id~scenario, scales = "free", ncol = 4) + 
  ggtitle(name_mapping[[provider]], "- Downlink") + 
  xlab("Zeit") + 
  ylab("Daten�bertragungsrate in MBit/s") +
  theme(legend.title = element_blank()) +
  scale_color_hue(labels = c("Beobachtung", "Vorhersage"))
  
######################### Scatterplot
  
plot_data <- data.frame(actual = actual$value, 
                        predict = vorhersage$value)
plot_data <- cbind(plot_data, 
                   dl_data[(dl_data["drive_id"] == 8 | dl_data["drive_id"] == 9 | dl_data["drive_id"] == 10) & dl_data["provider"] == provider, 
                           c("drive_id", "scenario")])

ggplot(
  plot_data, 
  aes(x = actual, y = predict)
) + 
  geom_point() + 
  geom_abline(color = "red", intercept = 0, slope = 1) +
  facet_wrap(drive_id ~ scenario, scales = "free", ncol = 4) + 
  ggtitle(paste("Scatterplot der Beobachtungen und der Vorhersagen:", name_mapping[[provider]]), "- Downlink") + 
  xlab("Beobachtungen") + 
  ylab("Vorhersage")
  
######################## Barplot Vergleich Kennzahlen 
  
  
df <- data.frame(provider = rep(c("vodafone", "tmobile", "o2"), each = 3),
                 kennzahl = rep(c("MSE", "MAE", "R�"), 3),
                 value = c(kennzahlen_final$vodafone$mse, kennzahlen_final$vodafone$mae,
                     kennzahlen_final$vodafone$rsquared,
                     kennzahlen_final$tmobile$mse, kennzahlen_final$tmobile$mae,
                     kennzahlen_final$tmobile$rsquared,
                     kennzahlen_final$o2$mse, kennzahlen_final$o2$mae,
                     kennzahlen_final$o2$rsquared))

ggplot(data = df, aes(x = kennzahl, y = value, fill = provider)) +
  geom_bar(stat = "identity", position = position_dodge()) + 
  facet_wrap(~ kennzahl, scales = "free") +
  theme(legend.title = element_blank()) +
  scale_fill_hue(labels = c("O2", "T-Mobile", "Vodafone")) + 
  ggtitle("Vergleich der Kennzahlen der verschiedenen Provider - Downlink") + 
  xlab("Kennzahlen") + 
  ylab("Wert") 
  
  
# TODO: Warum schl�gt der MSE und MAE bei o2 so aus?



