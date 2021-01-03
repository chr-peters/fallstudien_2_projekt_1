path <- paste("C:/Users/", 
              Sys.getenv("USERNAME"), 
              "/Documents/GitHub/fallstudien_2_projekt_1/scripts/arima_helpers.R", 
              sep = "")
source(path)
library(forecast)
#library(fastDummies)
#library(tidyverse)
library(anytime)
library(ggplot2)
library(Metrics)
#library(corrplot)
library(grid)
#library(ggcorrplot)
#library(lubridate)
library(regclass)
#library(sugrrants)
library(tseries)

setwd("~/GitHub/fallstudien_2_projekt_1/datasets")
dl_data <- read.csv2("dataset_dl.csv", header=TRUE, sep=",", dec=".") 
dl_data <- na.omit(dl_data)
dl_data$scenario <- factor(dl_data$scenario)


## Teile Daten nach Provider auf

vodafone <- dl_data[dl_data$provider == "vodafone", ]
tmobile <- dl_data[dl_data$provider == "tmobile", ]
o2 <- dl_data[dl_data$provider == "o2", ]
providers <- list("vodafone" = vodafone, "tmobile" = tmobile, "o2" = o2)

## Features
# Nehme Features aus dem Paper (Vergleichbarkeit)

features <- c("throughput_mbits", "payload_mb", "f_mhz", "rsrp_dbm", "rsrq_db", "rssnr_db", #"scenario"
              "cqi", "ta", "velocity_mps", "drive_id", "enodeb")

# Drive_Id soll nicht in das Modell, wird aber später gebraucht

lm_features <- c("throughput_mbits", "payload_mb", "f_mhz", "rsrp_dbm", "rsrq_db", "rssnr_db", #"scenario"
                 "cqi", "ta", "velocity_mps", "enodeb")


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


## Test auf Stationarität: Augmented Dickey-Fuller Test

for (j in c("vodafone", "o2", "tmobile")){
  for (i in numeric_features){
    adf.test(train[[j]][,i])$p.value
    print(adf.test(train[[j]][,i])$p.value)
  }
}
# alle p-Werte < 0.05, dh alle Variablen sind stationär - ARMA Modell
# keine Kointegration nötig

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


## Multikollinearität

lm_vodafone <- lm(throughput_mbits ~ ., data = train[["vodafone"]][, lm_features])
VIF(lm_vodafone)


lm_tmobile <- lm(throughput_mbits ~ ., data = train[["tmobile"]][, lm_features])
VIF(lm_tmobile)


lm_o2 <- lm(throughput_mbits ~ ., data = train[["o2"]][, lm_features])
VIF(lm_o2)


## Überprüfen der Normalverteilungsannahme der Residuen

res_tmobile <- data.frame(res = rstandard(lm_tmobile), 
                          provider = "T-Mobile", 
                          id = 1:length(rstandard(lm_tmobile)))
res_vodafone <- data.frame(res = rstandard(lm_vodafone), 
                           provider = "Vodafone", 
                           id = 1:length(rstandard(lm_vodafone)))
res_o2 <- data.frame(res = rstandard(lm_o2), 
                     provider = "O2", 
                     id = 1:length(rstandard(lm_o2)))

res_data <- rbind(res_vodafone, res_tmobile, res_o2)


# Scatterplot 
ggplot(res_data, aes(x = id, y = res, color = provider)) + geom_point() + 
  geom_abline(slope = 0, color = "black", size = 1, alpha = 0.8) +
  facet_wrap(~provider, scales = "free_x") + 
  ggtitle("Scatterplot der Residuen - Downlink") + 
  xlab("") + ylab("Residuen") +
  theme_grey(base_size = 20) + 
  theme(legend.position = "none")

ggsave(filename="res_scatter.png", 
       plot=last_plot(),
       path = paste(
         "C:/Users/", 
         Sys.getenv("USERNAME"), 
         "/Documents/GitHub/fallstudien_2_projekt_1/presentation/plots/arima/downlink",
         sep = ""),
       width = 30, height = 20, 
       units = "cm",
       dpi = 200
)

# qq-Plots

ggplot(res_data, aes(sample=res, color = provider)) + 
  geom_qq() + 
  geom_abline(intercept = 0, slope = 1, color = "black", size = 1, alpha = 0.8) + 
  facet_wrap(~provider) + 
  ggtitle("QQ-Plots Normalverteilung - Downlink") + 
  xlab("theoretische Quantile") + 
  ylab("Quantile der Residuen" ) +
  theme_grey(base_size = 20) + 
  theme(legend.position = "none")

ggsave(filename="res_qq.png", 
       plot=last_plot(),
       path = paste(
         "C:/Users/", 
         Sys.getenv("USERNAME"), 
         "/Documents/GitHub/fallstudien_2_projekt_1/presentation/plots/arima/downlink",
         sep = ""),
       width = 30, height = 20, 
       units = "cm",
       dpi = 200
)

# Histogramme

ggplot(res_data, aes(x = res, color = provider, fill = provider)) + 
  geom_histogram() + 
  facet_wrap(~ provider) + 
  ggtitle("Histogramme der Residuen - Downlink") + 
  xlab("Residuen") + ylab("Anzahl") +
  theme_grey(base_size = 20) + 
  theme(legend.position = "none")

ggsave(filename="res_histogram.png", 
       plot=last_plot(),
       path = paste(
         "C:/Users/", 
         Sys.getenv("USERNAME"), 
         "/Documents/GitHub/fallstudien_2_projekt_1/presentation/plots/arima/downlink",
         sep = ""),
       width = 30, height = 20, 
       units = "cm",
       dpi = 200
)


## Scatterplot

# ggplot(res_data, aes(x = 1:nrow(res_data), y = res)) + geom_point() + 
#   facet_wrap(~provider, scales = "free_x")
# 
# # qq-Plots
# 
# ggplot(res_data, aes(sample=res)) + geom_qq() + 
#   geom_abline(intercept = 0, slope = 1, color = "red", size = 1, alpha = 0.8) + 
#   facet_wrap(~provider) + ggtitle("QQ-Plots Normalverteilung - Downlink") + 
#   xlab("theoretische Quantile") + ylab("Quantile der Residuen")
# 
# 
# # Histogramme
# 
# ggplot(res_data, aes(x = res)) + geom_histogram(color="black", fill="pink") + 
#   facet_wrap(~ provider) + ggtitle("Histogramme der Residuen - Downlink") + 
#   xlab("Residuen") + ylab("Anzahl")


# Ergebnisse: o2, tmobile sehen gut aus, Vodafone hat viele Ausreißer
# Kein Test verwendet, da große Stichproben dazu führen, dass die Tests zu schnell ablehnen
# Kolmogorov-Smirnov/Shapiro-Wilk


## Plot ACF und PACF der Residuen
# Bestimmen der Parameter für das ARMA Modell

plot_data <- list(vodafone = lm_vodafone$residuals, 
                        tmobile = lm_tmobile$residuals, 
                        o2 = lm_o2$residuals)
plot_acf(plot_data, type = "acf", 
         title = "Autokorrelationsfunktionen der Residuen - Downlink")
plot_acf(plot_data, type = "pacf", 
         title = "partielle Autokorrelationsfunktionen der Residuen - Downlink")

## Bestimme Parameterrange aus ACF und PACF Plots für das Grid zum tunen der Parmaeter p,q

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
# ACF 0 - 10

max_ar <- 2
max_ma <- 10
nrow = (max_ar+1)*(max_ma+1)
grid_o2 <- matrix(data = c(rep(0:max_ar, each=max_ma+1), rep(0, nrow), rep(0:max_ma, max_ar+1)), 
                        nrow = nrow, ncol = 3)

# TMobile
# PACF 0 - 1
# ACF 0 - 2

max_ar <- 1
max_ma <- 2
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

# Erzeugen der Kennzahlen für die verschiedenen Provider und Testfahrten mit Zeitreihenkreuzvalidierung
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
    
    
    
    for (row in 1:nrow(grids[[provider]])){ # für jede Kombination aus dem Grid
      ## fit model
      y <- ts(cv_train[, "throughput_mbits"]) # konstruiere Zeitreihe
      xreg <- cv_train[, lm_features[-which(lm_features == "throughput_mbits")]] 
      #xreg <- dummy_cols(xreg, remove_first_dummy = TRUE, remove_selected_columns = TRUE)
      # Dummy codierung wenn nötig 
      xreg <- data.matrix(xreg)
      # konvertiere alle variablen zu numerischen variablen und 
      # Zusammenführen zu Spalten einer Matrix
      arima_fit <- Arima(y = y, order = grids[[provider]][row,], xreg = xreg, method = "ML")
      # fitte ein Arima Modell (wobei d = 0)
      
      
      ## predict
      y <- ts(cv_test[, "throughput_mbits"])
      xreg <- cv_test[, lm_features[-which(lm_features == "throughput_mbits")]]
      #xreg <- dummy_cols(xreg, remove_first_dummy = TRUE, remove_selected_columns = TRUE)
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

# Suche für jeden Provider die Kombination heraus, welche die besten Kennzahlen erzeugt

grids[["vodafone"]][which.min(rowMeans(kennzahlen$vodafone$mae))[[1]], ]
grids[["vodafone"]][which.min(rowMeans(kennzahlen$vodafone$mse))[[1]], ]
grids[["vodafone"]][which.max(rowMeans(kennzahlen$vodafone$rsquared))[[1]], ]
grids[["vodafone"]][which.min(rowMeans(kennzahlen$vodafone$aic))[[1]], ]
param_vodafone <- grids[["vodafone"]][which.min(rowMeans(kennzahlen$vodafone$mae))[[1]], ]

grids[["tmobile"]][which.min(rowMeans(kennzahlen$tmobile$mae))[[1]], ]
grids[["tmobile"]][which.min(rowMeans(kennzahlen$tmobile$mse))[[1]], ]
grids[["tmobile"]][which.max(rowMeans(kennzahlen$tmobile$rsquared))[[1]], ]
grids[["tmobile"]][which.min(rowMeans(kennzahlen$tmobile$aic))[[1]], ]
param_tmobile <- grids[["tmobile"]][which.min(rowMeans(kennzahlen$tmobile$mae))[[1]], ]

grids[["o2"]][which.min(rowMeans(kennzahlen$o2$mae))[[1]], ]
grids[["o2"]][which.min(rowMeans(kennzahlen$o2$mse))[[1]], ]
grids[["o2"]][which.max(rowMeans(kennzahlen$o2$rsquared))[[1]], ]
grids[["o2"]][which.min(rowMeans(kennzahlen$o2$aic))[[1]], ]
param_o2 <- grids[["o2"]][which.min(rowMeans(kennzahlen$o2$mae))[[1]], ]

parameter <- list("vodafone" = param_vodafone, 
                  "tmobile" = param_tmobile, 
                  "o2" = param_o2)
print(parameter) 
# vodafone 106
# tmobile 000
# o2 000

## Modell für den kompletten Trainingsdatensatz fitten und für Test predicten
## Predictions zurücktransformieren

kennzahlen_final <- list("vodafone" = list(), 
                   "tmobile" = list(), 
                   "o2" = list())
predictions <- list("vodafone" = list(), 
                    "tmobile" = list(), 
                    "o2" = list())
coeff <- list("vodafone" = list(), 
                     "tmobile" = list(), 
                     "o2" = list())

for (provider in c("tmobile", "o2", "vodafone")){
  y <- ts(train[[provider]][, "throughput_mbits"])
  xreg <- train[[provider]][, lm_features[-which(lm_features == "throughput_mbits")]]
  #xreg <- dummy_cols(xreg, remove_first_dummy = TRUE, remove_selected_columns = TRUE)
  xreg <- data.matrix(xreg)
  arima_fit <- Arima(y = y, order = parameter[[provider]], xreg = xreg, method = "ML")
  coeff[[provider]] <- arima_fit$coef[c("intercept",lm_features[-which(lm_features == "throughput_mbits")])]
  # predict
  y <- ts(test[[provider]][, "throughput_mbits"])
  xreg <- test[[provider]][, lm_features[-which(lm_features == "throughput_mbits")]]
  #xreg <- dummy_cols(xreg, remove_first_dummy = TRUE, remove_selected_columns = TRUE)
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

provider <- "vodafone"
  
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
                     "scenario"],
  upper = unclass(predictions[[provider]]$upper[,"80%"] * attr(train[[provider]], "scaled:scale")["throughput_mbits"] + 
                    attr(train[[provider]], "scaled:center")["throughput_mbits"]),
  lower = unclass(predictions[[provider]]$lower[,"80%"] * attr(train[[provider]], "scaled:scale")["throughput_mbits"] + 
                    attr(train[[provider]], "scaled:center")["throughput_mbits"]))

vorhersage <- data.frame(
  value = predictions[[provider]]$rescaled_forecast, 
  type = "predict", 
  timestamp = anytime(dl_data[(dl_data["drive_id"]==8 | dl_data["drive_id"] == 9 | dl_data["drive_id"] == 10) & dl_data["provider"] == provider, 
                              "timestamp_ms"]),
  drive_id = dl_data[(dl_data["drive_id"]==8 | dl_data["drive_id"] == 9 | dl_data["drive_id"] == 10) & dl_data["provider"] == provider, 
                     "drive_id"], 
  scenario = dl_data[(dl_data["drive_id"]==8 | dl_data["drive_id"] == 9 | dl_data["drive_id"] == 10) & dl_data["provider"] == provider, 
                     "scenario"],
  upper = unclass(predictions[[provider]]$upper[,"80%"])* attr(train[[provider]], "scaled:scale")["throughput_mbits"] + 
    attr(train[[provider]], "scaled:center")["throughput_mbits"],
  lower = unclass(predictions[[provider]]$lower[,"80%"])* attr(train[[provider]], "scaled:scale")["throughput_mbits"] + 
    attr(train[[provider]], "scaled:center")["throughput_mbits"])
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
  geom_line(size=1) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), colour = NA, alpha = 0.2) +
  facet_wrap(drive_id~scenario, 
             scales = "free", 
             ncol = 4, 
             labeller = label_wrap_gen(multi_line=FALSE)) + 
  ggtitle(paste(name_mapping[[provider]], "- Downlink", sep = " ")) + 
  xlab("Zeit") + 
  ylab("Datenübertragungsrate in MBit/s") +
  theme_grey(base_size = 14) +
  theme(legend.position="bottom", 
        legend.title = element_blank()) +
  scale_color_hue(labels = c("Beobachtung", "Vorhersage mit 80% KI"))
  
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
  facet_wrap(drive_id ~ scenario, 
             scales = "free", 
             ncol = 4, 
             labeller = label_wrap_gen(multi_line=FALSE)) + 
  ggtitle(paste("Scatterplot der Beobachtungen und der Vorhersagen:", name_mapping[[provider]], "- Downlink")) + 
  xlab("Beobachtungen") + 
  ylab("Vorhersage") +
  theme_grey(base_size = 14)

######################## Barplot Vergleich Kennzahlen 
  
  
df <- data.frame(provider = rep(c("vodafone", "tmobile", "o2"), each = 2),
                 kennzahl = rep(c("MAE", "R²"), 3),
                 value = c(kennzahlen_final$vodafone$mae,
                     kennzahlen_final$vodafone$rsquared,
                     kennzahlen_final$tmobile$mae,
                     kennzahlen_final$tmobile$rsquared,
                     kennzahlen_final$o2$mae,
                     kennzahlen_final$o2$rsquared))

ggplot(data = df, aes(x = kennzahl, y = value, fill = provider)) +
  geom_bar(stat = "identity", position = position_dodge()) + 
  facet_wrap(~ kennzahl, scales = "free") +
  theme_grey(base_size = 18) +
  theme(legend.title = element_blank(), 
        legend.position = "bottom") +
  scale_fill_hue(labels = c("O2", "T-Mobile", "Vodafone")) + 
  ggtitle("Vergleich der Kennzahlen der verschiedenen Provider - Downlink") + 
  xlab("Kennzahlen") + 
  ylab("Wert")

############################################# Predictions to csv

data <- dl_data[c(dl_data$drive_id == 8 | dl_data$drive_id == 9 | dl_data$drive_id == 10),]
data$prediction_arima <- NA
data$arima_lower_80 <- NA
data$arima_lower_95 <- NA
data$arima_upper_80 <- NA
data$arima_upper_95 <- NA

for (provider in c("vodafone", "tmobile", "o2")){
  data[data$provider == provider,"prediction_arima"] <- predictions[[provider]]$rescaled_forecast
  data[data$provider == provider, "arima_lower_80"] <- predictions[[provider]]$lower[, "80%"]* attr(train[[provider]], "scaled:scale")["throughput_mbits"] + 
    attr(train[[provider]], "scaled:center")["throughput_mbits"]
  data[data$provider == provider, "arima_lower_95"] <- predictions[[provider]]$lower[, "95%"]* attr(train[[provider]], "scaled:scale")["throughput_mbits"] + 
    attr(train[[provider]], "scaled:center")["throughput_mbits"]
  data[data$provider == provider, "arima_upper_80"] <- predictions[[provider]]$upper[, "80%"]* attr(train[[provider]], "scaled:scale")["throughput_mbits"] + 
    attr(train[[provider]], "scaled:center")["throughput_mbits"]
  data[data$provider == provider, "arima_upper_95"] <- predictions[[provider]]$upper[, "95%"]* attr(train[[provider]], "scaled:scale")["throughput_mbits"] + 
    attr(train[[provider]], "scaled:center")["throughput_mbits"]
  
}
write.csv(data, "C:/Users/Laura/Documents/GitHub/fallstudien_2_projekt_1/prediction_results/prediction_arima_dl.csv", row.names = FALSE)


#--------------------FEATURE IMPORTANCE----------------------------------------#

setwd("~/GitHub/fallstudien_2_projekt_1/predicition_results")
dldata <- read.csv("feature_importance_xgboost_dl.csv", header = TRUE)


# df_dl <- data.frame(provider = rep(c(" ", "  ", "   "), each = 9),
#                     #features = uldata$feature[-which(c(data$feature == "enodeb"))],
#                     features = dldata$feature,
#                     #value = uldata$Gain[-which(c(data$feature == "enodeb"))])
#                     value = abs(dldata$Permutation))
# 
# 
# df1 <- data.frame(provider = rep(c(" ", "  ", "   "), each = 9),
#                   features = rep(lm_features[-which(lm_features == "throughput_mbits")], 3),
#                   value = abs(c(coeff$o2[-which(names(coeff$o2) == "intercept")], 
#                                 coeff$tmobile[-which(names(coeff$tmobile) == "intercept")],
#                                 coeff$vodafon[-which(names(coeff$vodafone) == "intercept")])))
# 
# name_mapping = c(
#   " " = "O2", 
#   "  " = "T-Mobile", 
#   "   " = "Vodafone"
# )
# 
# ggplot(data = df_dl, aes(x = reorder_within(features, -value, provider, sep = " "), y = value, fill = provider)) +
#   geom_bar(stat = "identity" ) + 
#   facet_wrap(~ provider, scales = "free", labeller = as_labeller(name_mapping)) +
#   theme_grey(base_size = 18) +
#   theme(legend.title = element_blank(), axis.text.x = element_text(angle = -45, hjust = 0, vjust = 0.5),
#         legend.position = "none") +
#   ggtitle("Feature Importance der verschiedenen Provider - Downlink") + 
#   xlab("Features") + 
#   ylab("Koeffizienten")

# Vergleich der Modelle 

for (provider in c("o2", "tmobile", "vodafone")){
  
  dldata[dldata$provider == provider,]$Permutation <- abs(dldata[dldata$provider == provider,]$Permutation)/
    sum(abs(dldata[dldata$provider == provider,]$Permutation))
  
}

coeff$o2[-which(names(coeff$o2) == "intercept")] <- abs(coeff$o2[-which(names(coeff$o2) == "intercept")])/
  sum(abs(coeff$o2[-which(names(coeff$o2) == "intercept")]))

coeff$tmobile[-which(names(coeff$tmobile) == "intercept")] <- abs(coeff$tmobile[-which(names(coeff$tmobile) == "intercept")])/
  sum(abs(coeff$tmobile[-which(names(coeff$tmobile) == "intercept")]))


coeff$vodafone[-which(names(coeff$vodafone) == "intercept")] <- abs(coeff$vodafone[-which(names(coeff$vodafone) == "intercept")])/
  sum(abs(coeff$vodafone[-which(names(coeff$vodafone) == "intercept")]))



df_dl_both <- data.frame(provider = c(rep(c("O2", "T-Mobile", "Vodafone"), each = 9),rep(c("O2", "T-Mobile", "Vodafone"), each = 9)),
                         features = c(rep(names(coeff$vodafone)[-1], 3), #dldata$feature,
                                      dldata$feature),
                         value = abs(c(#dldata$Gain,
                                      c(coeff$o2[-which(names(coeff$o2) == "intercept")], 
                                      coeff$tmobile[-which(names(coeff$tmobile) == "intercept")],
                                      coeff$vodafone[-which(names(coeff$vodafone) == "intercept")],
                                      dldata$Permutation))),
                         model = c(rep("ARMA", 27), rep("XGboost", 27))) # Gain vs Permutation


name_mapping = list(
  "vodafone" = "Vodafone", 
  "tmobile" = "T-Mobile", 
  "o2" = "O2"
)
ggplot(data = df_dl_both, aes(x = features, y = value, fill = provider)) +
  geom_bar(stat = "identity" ) + 
  facet_grid(model ~ provider, scales = "free_x", labeller = as_labeller(name_mapping)) +
  theme_grey(base_size = 18) +
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = -45, hjust = 0, vjust = 0.5),
        legend.position = "none") +
  ggtitle("Feature Importance Modellvergleich - Downlink") + 
  xlab("Features") + 
  ylab("Wichtigkeit")
