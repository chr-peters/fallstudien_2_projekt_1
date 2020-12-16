setwd("~/GitHub/fallstudien_2_projekt_1/prediction_results")

#--------------------------------UPLINK-----------------------------------#
xgboost <- read.csv2("predictions_xgboost_ul.csv", header=TRUE, sep=",", dec=".") 
arima <- read.csv2("predictions_arima_ul.csv", header=TRUE, sep=",", dec=".")

merged_ul <- merge(xgboost, arima)
write.csv(merged_ul, 
          "C:/Users/Laura/Documents/GitHub/fallstudien_2_projekt_1/prediction_results/predictions_ul.csv", 
          row.names = FALSE)

#-------------------------------DOWNLINK----------------------------------#
xgboost <- read.csv2("predictions_xgboost_dl.csv", header=TRUE, sep=",", dec=".") 
arima <- read.csv2("prediction_arima_dl.csv", header=TRUE, sep=",", dec=".")

merged_dl <- merge(xgboost, arima)
write.csv(merged_dl, 
          "C:/Users/Laura/Documents/GitHub/fallstudien_2_projekt_1/prediction_results/predictions_dl.csv", 
          row.names = FALSE)

remove(xgboost)
remove(arima)



