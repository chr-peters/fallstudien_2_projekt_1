setwd("~/GitHub/fallstudien_2_projekt_1/prediction_results")
data_ul <- read.csv("predictions_ul.csv", header=TRUE, sep=",", dec=".")
data_dl <- read.csv("predictions_dl.csv", header=TRUE, sep=",", dec=".")

for (provider in c("vodafone", "tmobile", "o2")){
  actual <- data.frame(
    value = data_dl[data_dl["provider"] == provider, "throughput_mbits"], 
    type = "actual", 
    timestamp = anytime(data_dl[data_dl["provider"] == provider, "timestamp_ms"]),
    drive_id = data_dl[data_dl["provider"] == provider, "drive_id"], 
    scenario = data_dl[data_dl["provider"] == provider, "scenario"])
  
  vorhersage <- data.frame(
    value = data_dl[data_dl["provider"] == provider, "prediction_xgboost"], 
    type = "predict", 
    timestamp = anytime(data_dl[data_dl["provider"] == provider, "timestamp_ms"]),
    drive_id = data_dl[data_dl["provider"] == provider, "drive_id"], 
    scenario = data_dl[data_dl["provider"] == provider, "scenario"])
  
  plot_data <- rbind(actual, vorhersage)
  
  name_mapping = list(
    "vodafone" = "Vodafone", 
    "tmobile" = "T-Mobile", 
    "o2" = "O2"
  )
  
  
#---------------------------- Zeitreihen-Plot----------------------------------#
  
  print(
    ggplot(
      plot_data, 
      aes(x = timestamp, y = value, color = type)
    ) + 
      geom_line(size=1) + 
      #geom_ribbon(aes(ymin = lower, ymax = upper), colour = NA, alpha = 0.2) +
      facet_wrap(drive_id~scenario, 
                 scales = "free", 
                 ncol = 4, 
                 labeller = label_wrap_gen(multi_line = FALSE)) + 
      ggtitle(paste(name_mapping[[provider]], "- Downlink", sep = " ")) + 
      xlab("Zeit") + 
      ylab("Datenübertragungsrate in MBit/s") +
      theme_grey(base_size = 14) +
      theme(legend.position="bottom", 
            legend.title = element_blank()) +
      scale_color_hue(labels = c("Beobachtung", "Vorhersage")))
  
  
#-----------------------------Scatter-Plot-------------------------------------#
  
  plot_data <- data.frame(actual = actual$value, 
                          predict = vorhersage$value)
  plot_data <- cbind(plot_data, 
                     data_dl[data_dl["provider"] == provider, 
                             c("drive_id", "scenario")])
  
  print(
    ggplot(
      plot_data, 
      aes(x = actual, y = predict)
    ) + 
      geom_point() + 
      geom_abline(color = "red", intercept = 0, slope = 1) +
      facet_grid(drive_id ~ scenario, 
                 #scales = "free", 
                 #ncol = 4, 
                 labeller = label_wrap_gen(multi_line=FALSE)) + 
      ggtitle(paste("Scatterplot der Beobachtungen und der Vorhersagen:", name_mapping[[provider]], "- Downlink")) + 
      xlab("Beobachtungen") + 
      ylab("Vorhersage") +
      theme_grey(base_size = 14))
  
}


