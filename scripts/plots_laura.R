path <- paste("C:/Users/", 
              Sys.getenv("USERNAME"), 
              "/Documents/GitHub/fallstudien_2_projekt_1/scripts/plots_helper.R", 
              sep = "")
source(path)

setwd("~/GitHub/fallstudien_2_projekt_1/prediction_results")
data_ul <- read.csv("predictions_ul.csv", header=TRUE, sep=",", dec=".")
data_dl <- read.csv("predictions_dl.csv", header=TRUE, sep=",", dec=".")

#-------------------------------ARIMA PREDICTIONS----------------------------#
for (direction in c("uplink", "downlink")){
  for (provider in c("vodafone", "tmobile", "o2")){
    if (direction == "uplink") data = data_ul
    else data = data_dl
    plot_predictions(
      data = data, 
      direction = direction, 
      provider = provider, 
      model = "arima", 
      ci = TRUE)
  }
}

#-----------------------------ARIMA SCATTER----------------------------------#
for (direction in c("uplink", "downlink")){
  if (direction == "uplink") data = data_ul
  else data = data_dl
  plot_scatter(
    data = data, 
    direction = direction, 
    model = "arima")
}

#------------------------------XGBOOST PREDICTIONS---------------------------#
for (direction in c("uplink", "downlink")){
  for (provider in c("vodafone", "tmobile", "o2")){
    if (direction == "uplink") data = data_ul
    else data = data_dl
    plot_predictions(
      data = data_ul, 
      direction = direction, 
      provider = provider, 
      model = "xgboost", 
      ci = FALSE)
  }
}

#---------------------------XGBOOST SCATTER---------------------------------#
for (direction in c("uplink", "downlink")){
  if (direction == "uplink") data = data_ul
  else data = data_dl
  plot_scatter(
    data = data, 
    direction = direction, 
    model = "xgboost")
}




