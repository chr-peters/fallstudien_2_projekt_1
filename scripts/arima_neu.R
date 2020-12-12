

setwd("~/GitHub/fallstudien_2_projekt_1/datasets")
library(tseries)
library(regclass)
library(ggplot2)
#library(car)

ul_data <- read.csv2("dataset_ul.csv", header=TRUE, sep=",", dec=".")
ul_data <- na.omit(ul_data)

# Teile Daten nach Provider auf

vodafone <- ul_data[ul_data$provider == "vodafone", ]
tmobile <- ul_data[ul_data$provider == "tmobile", ]
o2 <- ul_data[ul_data$provider == "o2", ]
providers <- list("vodafone" = vodafone, "tmobile" = tmobile, "o2" = o2)


features_s <- c("throughput_mbits", "payload_mb", #"f_mhz", "rsrq_db",
              "rsrp_dbm", "ta", "velocity_mps", "cqi", "scenario")

train <- lapply(providers, function(provider) 
  provider[provider["drive_id"] != 8 & provider["drive_id"] != 9 & provider["drive_id"] != 10, features_s])
test <- lapply(providers, function(provider) 
  provider[provider["drive_id"] == 8 | provider["drive_id"] == 9 | provider["drive_id"] == 10, features_s])

features <- c("throughput_mbits", "payload_mb", #"f_mhz", "rsrq_db",
                "rsrp_dbm", "ta", "velocity_mps", "cqi")

# Test auf Stationarität: Augmented Dickey-Fuller Test

for (j in c("vodafone", "o2", "tmobile")){
  for (i in c(features)){
    adf.test(train[[j]][,i])$p.value
    print(adf.test(train[[j]][,i])$p.value)
  }
}
# alle p-Werte < 0.05, dh alle Variablen sind stationär

# Daten skalieren

numeric_features <- features[as.vector(unlist(lapply(train[[1]][, features], is.numeric)))]

for (provider in c("vodafone", "tmobile", "o2")){
  scaled <- scale(train[[provider]][, numeric_features])
  train[[provider]][, numeric_features] <- scaled
  attr(train[[provider]], "scaled:center") <- attr(scaled, "scaled:center")
  attr(train[[provider]], "scaled:scale") <- attr(scaled, "scaled:center")
  test[[provider]][, numeric_features] <- scale(test[[provider]][, numeric_features], 
                                                center = attr(scaled, "scaled:center"), 
                                                scale = attr(scaled, "scaled:scale"))
}


# Lineare Modelle

train[["vodafone"]]$scenario <- factor(train[["vodafone"]]$scenario)
lm_vodafone <- lm(throughput_mbits ~ ., data = train[["vodafone"]])

train[["o2"]]$scenario <- factor(train[["o2"]]$scenario)
lm_o2 <- lm(throughput_mbits ~ ., data = train[["o2"]])

train[["tmobile"]]$scenario <- factor(train[["tmobile"]]$scenario)
lm_tmobile <- lm(throughput_mbits ~ ., data = train[["tmobile"]])

# Multikollinearität
# entferne rsrq und f_mhz aus dem Modell (hohe Korrelation mit scenario)

VIF(lm_vodafone) 

VIF(lm_o2)

VIF(lm_tmobile)


# Residuen Plots

res_vodafone <- data.frame(res=lm_vodafone$residuals, provider="Vodafone")
res_tmobile <- data.frame(res=lm_o2$residuals, provider="T-Mobile")
res_o2 <- data.frame(res=lm_tmobile$residuals, provider="O2")
res_data <- rbind(res_vodafone, res_tmobile, res_o2)

ggplot(res_data, aes(x = res)) + geom_histogram(color="black", fill="pink") + 
  facet_wrap(~ provider) + ggtitle("Histogramme Residuen") + xlab("Residuen") + ylab("Anzahl")

 
