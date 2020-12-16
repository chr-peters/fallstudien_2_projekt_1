library(ggplot2)

plot_predictions <- function(data, 
                         direction = c("downlink", "uplink"), 
                         provider = c("vodafone", "tmobile", "o2"), 
                         model = c("arima", "xgboost"), 
                         width = 30, height = 20, 
                         view_plot = FALSE, ci = FALSE, 
                         filename = "predictions.png"){
  
  actual <- data.frame(
    value = data[data["provider"] == provider, "throughput_mbits"], 
    type = "Beobachtung", 
    timestamp = anytime(data[data["provider"] == provider, "timestamp_ms"]),
    drive_id = data[data["provider"] == provider, "drive_id"], 
    scenario = data[data["provider"] == provider, "scenario"], 
    lower = data[data["provider"] == provider, "arima_lower_80"], 
    upper = data[data["provider"] == provider, "arima_upper_80"])
  
  vorhersage <- data.frame(
    value = data[data["provider"] == provider, paste("prediction", model, sep = "_")], 
    type = "Vorhersage", 
    timestamp = anytime(data[data["provider"] == provider, "timestamp_ms"]),
    drive_id = data[data["provider"] == provider, "drive_id"], 
    scenario = data[data["provider"] == provider, "scenario"], 
    lower = data[data["provider"] == provider, "arima_lower_80"], 
    upper = data[data["provider"] == provider, "arima_upper_80"])
  
  plot_data <- rbind(actual, vorhersage)
  
  name_mapping = list(
    "vodafone" = "Vodafone", 
    "tmobile" = "T-Mobile", 
    "o2" = "O2", 
    "downlink" = "Downlink", 
    "uplink" = "Uplink"
  )
  
  plot <- ggplot(
      plot_data, 
      aes(x = timestamp, y = value, color = type)
    ) + 
      geom_line(size=1) + 
      facet_wrap(drive_id~scenario, 
                 scales = "free", 
                 ncol = 4, 
                 labeller = label_wrap_gen(multi_line = FALSE)) + 
      ggtitle(paste(name_mapping[[provider]], 
                    name_mapping[[direction]], sep = " - ")) + 
      xlab("Zeit") + 
      ylab("Datenübertragungsrate in MBit/s") +
      theme_grey(base_size = 14) +
      theme(legend.position="bottom", 
            legend.title = element_blank())
  
  if(ci == TRUE){
    plot = plot + 
      geom_ribbon(aes(ymin = lower, ymax = upper), colour = NA, alpha = 0.2) +
      scale_color_hue(labels = c("Beobachtung", "Vorhersage mit 80% KI"))
  }
  else plot = plot + 
    scale_color_hue(labels = c("Beobachtung", "Vorhersage"))
  
  if (view_plot == TRUE) print(plot)
  
  ggsave(filename=paste(provider, filename, sep="_"), 
         plot=last_plot(),
         path = paste(
           "C:/Users/", 
           Sys.getenv("USERNAME"), 
           "/Documents/GitHub/fallstudien_2_projekt_1/presentation/plots/", 
           model, "/", 
           direction,
           sep = ""),
         width = width, height = height, 
         units = "cm",
         dpi = 200
  )
  
}


plot_scatter <- function(data, 
                         direction = c("downlink", "uplink"), 
                         model = c("arima", "xgboost"), 
                         width = 30, height = 20, 
                         drive_id_colored = FALSE,
                         view_plot = FALSE, 
                         filename = "scatter.png"){
  plot_data <- data[, c("throughput_mbits", 
                        paste("prediction", model, sep = "_"), 
                        "provider", 
                        "scenario", 
                        "drive_id")]
  colnames(plot_data)[
    colnames(plot_data) == paste("prediction", model, sep = "_")
    ] <- "prediction"
  plot_data$Testfahrt <- factor(plot_data$drive_id)
  
  name_mapping = list(
    "vodafone" = "Vodafone", 
    "tmobile" = "T-Mobile", 
    "o2" = "O2", 
    "downlink" = "Downlink", 
    "uplink" = "Uplink"
  )
  for (provider in c("vodafone", "tmobile", "o2")){
    plot_data[plot_data["provider"] == provider, "provider"] <- name_mapping[[provider]]
  }
  
  plot <- ggplot(
      plot_data, 
      aes(x = throughput_mbits, y = prediction)
    ) + 
      geom_abline(color = "red", intercept = 0, slope = 1) +
      facet_grid(provider ~ scenario, 
                 scales = "free_y", 
                 labeller = label_wrap_gen(multi_line=FALSE)) + 
      ggtitle(paste("Scatterplot der Beobachtungen und der Vorhersagen:", 
                    name_mapping[[direction]], sep = " ")) + 
      xlab("Beobachtungen") + 
      ylab("Vorhersage") +
      theme_grey(base_size = 14)

  if (drive_id_colored == TRUE) {plot = plot + 
    geom_point(aes(color = Testfahrt, shape = Testfahrt))
  } else {plot = plot + geom_point()}
  if (view_plot == TRUE) print(plot)
  
  ggsave(filename=filename, 
       plot=last_plot(),
       path = paste(
         "C:/Users/", 
         Sys.getenv("USERNAME"), 
         "/Documents/GitHub/fallstudien_2_projekt_1/presentation/plots/", 
         model, "/", 
         direction,
         sep = ""),
       width = width, height = height, 
       units = "cm",
       dpi = 200
  )
  
}