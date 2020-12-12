

setwd("~/GitHub/fallstudien_2_projekt_1/datasets")
library(tseries)
library(regclass)
library(car)

ul_data <- read.csv2("dataset_ul.csv", header=TRUE, sep=",", dec=".")
ul_data <- na.omit(ul_data)

# Teile Daten nach Provider auf

vodafone <- ul_data[ul_data$provider == "vodafone", ]
tmobile <- ul_data[ul_data$provider == "tmobile", ]
o2 <- ul_data[ul_data$provider == "o2", ]
providers <- list("vodafone" = vodafone, "tmobile" = tmobile, "o2" = o2)


features_s <- c("throughput_mbits", "payload_mb", #"f_mhz", 
              "rsrp_dbm", "rsrq_db", "ta", "velocity_mps", "cqi", "scenario")

train <- lapply(providers, function(provider) 
  provider[provider["drive_id"] != 8 & provider["drive_id"] != 9 & provider["drive_id"] != 10, features_s])
test <- lapply(providers, function(provider) 
  provider[provider["drive_id"] == 8 | provider["drive_id"] == 9 | provider["drive_id"] == 10, features_s])

features <- c("throughput_mbits", "payload_mb", #"f_mhz", 
                "rsrp_dbm", "rsrq_db", "ta", "velocity_mps", "cqi")

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

# Multikollinearität


for (i in c("vodafone")){
  train[[i]]$scenario <- factor(train[[i]]$scenario)
  l1 <- lm(throughput_mbits ~ ., data = train[[i]])
}
VIF(l1)

for (i in c("o2")){
  train[[i]]$scenario <- factor(train[[i]]$scenario)
  l2 <- lm(throughput_mbits ~ ., data = train[[i]])
}
VIF(l2)

for (i in c("tmobile")){
  train[[i]]$scenario <- factor(train[[i]]$scenario)
  l3 <- lm(throughput_mbits ~ ., data = train[[i]])
}
VIF(l3)

# Residuen Plots

hist(l3$residuals)



 
