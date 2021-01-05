
library(ggplot2)
library(tidytext)
library(Metrics)


#--------------------KENNZAHLEN VERGLEICH XGBOOST/ARIMA TASK I----------------------------------------#
setwd("~/GitHub/fallstudien_2_projekt_1/prediction_results")

data_ul <- read.csv("predictions_ul.csv", header=TRUE, sep=",", dec=".")
data_dl <- read.csv("predictions_dl.csv", header=TRUE, sep=",", dec=".")


kennzahlen_ul_xgboost <- list("vodafone" = list(), 
                         "tmobile" = list(), 
                         "o2" = list())

kennzahlen_ul_arima <- list("vodafone" = list(), 
                            "tmobile" = list(), 
                            "o2" = list())

kennzahlen_dl_xgboost <- list("vodafone" = list(), 
                           "tmobile" = list(), 
                           "o2" = list())

kennzahlen_dl_arima <- list("vodafone" = list(), 
                      "tmobile" = list(), 
                      "o2" = list())


for (provider in c("vodafone", "tmobile", "o2")){
  subset_ul <- data_ul[data_ul$provider == provider, ]
  subset_dl <- data_dl[data_dl$provider == provider, ]
  
  kennzahlen_ul_xgboost[[provider]]$mae <- mae(subset_ul$prediction_xgboost, subset_ul$throughput_mbits)
  kennzahlen_ul_xgboost[[provider]]$rsquared <- 1 - sum((subset_ul$prediction_xgboost-subset_ul$throughput_mbits)^2)/
    sum((mean(subset_ul$throughput_mbits)-subset_ul$throughput_mbits)^2)
    
  kennzahlen_dl_xgboost[[provider]]$mae <- mae(subset_dl$prediction_xgboost, subset_dl$throughput_mbits)
  kennzahlen_dl_xgboost[[provider]]$rsquared <- 1 - sum((subset_dl$prediction_xgboost-subset_dl$throughput_mbits)^2)/
    sum((mean(subset_dl$throughput_mbits)-subset_dl$throughput_mbits)^2)
    
  kennzahlen_ul_arima[[provider]]$mae <- mae(subset_ul$prediction_arima, subset_ul$throughput_mbits)
  kennzahlen_ul_arima[[provider]]$rsquared <- 1 - sum((subset_ul$prediction_arima-subset_ul$throughput_mbits)^2)/
    sum((mean(subset_ul$throughput_mbits)-subset_ul$throughput_mbits)^2)
    
  kennzahlen_dl_arima[[provider]]$mae <- mae(subset_dl$prediction_arima, subset_dl$throughput_mbits)
  kennzahlen_dl_arima[[provider]]$rsquared <- 1 - sum((subset_dl$prediction_arima-subset_dl$throughput_mbits)^2)/
    sum((mean(subset_dl$throughput_mbits)-subset_dl$throughput_mbits)^2)
    
}

## UL

df_ul <- data.frame(model = rep(c("XGBoost", "ARMA"), each = 6),
                 provider = rep(c("Vodafone", "T-Mobile", "O2"), 4),
                 kennzahl = c(rep(c("MAE", "R²"), each = 3),rep(c("MAE", "R²"), each = 3)),
                 value = c(kennzahlen_ul_xgboost$vodafone$mae,
                           kennzahlen_ul_xgboost$tmobile$mae,
                           kennzahlen_ul_xgboost$o2$mae,
                           kennzahlen_ul_xgboost$vodafone$rsquared,
                           kennzahlen_ul_xgboost$tmobile$rsquared,
                           kennzahlen_ul_xgboost$o2$rsquared,
                           kennzahlen_ul_arima$vodafone$mae,
                           kennzahlen_ul_arima$tmobile$mae,
                           kennzahlen_ul_arima$o2$mae,
                           kennzahlen_ul_arima$vodafone$rsquared,
                           kennzahlen_ul_arima$tmobile$rsquared,
                           kennzahlen_ul_arima$o2$rsquared))

ggplot(data = df_ul, aes(x = model, y = value, fill = model) )+
  geom_bar(stat = "identity", position = position_dodge()) + 
  facet_grid(kennzahl ~ provider, scales = "free_y") +
  theme_grey(base_size = 18) +
  theme(legend.title = element_blank(), 
        legend.position = "none") +
  ggtitle("Vergleich der Kennzahlen der verschiedenen Modelle - Uplink") +
  geom_text(aes(label=formatC(round(value, digits = 2), format='f', digits=2)), size = 5, position=position_dodge(width=0.9), vjust=1.5) +
  xlab("Modelle") + 
  ylab("Wert")

## DL

df_dl <- data.frame(model = rep(c("XGBoost", "ARMA"), each = 6),
                 provider = rep(c("Vodafone", "T-Mobile", "O2"), 4),
                 kennzahl = c(rep(c("MAE", "R²"), each = 3),rep(c("MAE", "R²"), each = 3)),
                 value = c(kennzahlen_dl_xgboost$vodafone$mae,
                           kennzahlen_dl_xgboost$tmobile$mae,
                           kennzahlen_dl_xgboost$o2$mae,
                           kennzahlen_dl_xgboost$vodafone$rsquared,
                           kennzahlen_dl_xgboost$tmobile$rsquared,
                           kennzahlen_dl_xgboost$o2$rsquared,
                           kennzahlen_dl_arima$vodafone$mae,
                           kennzahlen_dl_arima$tmobile$mae,
                           kennzahlen_dl_arima$o2$mae,
                           kennzahlen_dl_arima$vodafone$rsquared,
                           kennzahlen_dl_arima$tmobile$rsquared,
                           kennzahlen_dl_arima$o2$rsquared))

ggplot(data = df_dl, aes(x = model, y = value, fill = model) )+
  geom_bar(stat = "identity", position = position_dodge()) + 
  facet_grid(kennzahl ~ provider, scales = "free_y") +
  theme_grey(base_size = 18) +
  theme(legend.title = element_blank(), 
        legend.position = "none") +
  ggtitle("Vergleich der Kennzahlen der verschiedenen Modelle - Downlink") +
  geom_text(aes(label=formatC(round(value, digits = 2), format='f', digits=2)), size = 5, position=position_dodge(width=0.9), vjust=1.5) +
  xlab("Modelle") + 
  ylab("Wert")


#--------------------KENNZAHLEN XGBOOST LINK LIFETIME TASK II----------------------------------------#

setwd("~/GitHub/fallstudien_2_projekt_1/prediction_results")
data_kenn <- read.csv("predictions_xgboost_linklifetime.csv", header = TRUE)

kennzahlen_ll_xgboost <- list("vodafone" = list(), 
                              "tmobile" = list(), 
                              "o2" = list())

for (provider in c("o2", "tmobile", "vodafone")){
  prediction <- data_kenn[data_kenn$provider == provider,"prediction_xgboost"]
  link_lt <- data_kenn[data_kenn$provider == provider,"link_lifetime"]
  kennzahlen_ll_xgboost[[provider]]$mae <- mae(prediction, link_lt)
  kennzahlen_ll_xgboost[[provider]]$rsquared <- 1 - sum((prediction-link_lt)^2)/
    sum((mean(link_lt)-link_lt)^2)
}




df <- data.frame(provider = rep(c("vodafone", "tmobile", "o2"), each = 2),
                 kennzahl = rep(c("MAE", "R²"), 3),
                 value = c(kennzahlen_ll_xgboost$vodafone$mae,
                           kennzahlen_ll_xgboost$vodafone$rsquared,
                           kennzahlen_ll_xgboost$tmobile$mae,
                           kennzahlen_ll_xgboost$tmobile$rsquared,
                           kennzahlen_ll_xgboost$o2$mae,
                           kennzahlen_ll_xgboost$o2$rsquared))

ggplot(data = df, aes(x = kennzahl, y = value, fill = provider)) +
  geom_bar(stat = "identity", position = position_dodge()) + 
  facet_wrap(~ kennzahl, scales = "free") +
  theme_grey(base_size = 18) +
  theme(legend.title = element_blank(), 
        legend.position = "bottom") +
  scale_fill_hue(labels = c("O2", "T-Mobile", "Vodafone")) + 
  ggtitle("Vergleich der Kennzahlen der verschiedenen Provider - Link-Liftime") + 
  xlab("Kennzahlen") + 
  ylab("Wert")

#--------------------FEATURE IMPORTANCE XGBOOST LINK LIFETIME TASK II----------------------------------------#

setwd("~/GitHub/fallstudien_2_projekt_1/prediction_results")

data <- read.csv("feature_importance_xgboost_linklifetime.csv", header = TRUE)


df_ll <- data.frame(provider = rep(c(" ", "  ", "   "), each = 9),
                    #features = data$feature[-which(c(data$feature == "enodeb"))],
                    features = data$feature,
                    #value = data$Gain[-which(c(data$feature == "enodeb"))])
                    value = abs(data$Permutation))

name_mapping = c(
  " " = "O2", 
  "  " = "T-Mobile", 
  "   " = "Vodafone"
)

ggplot(data = df_ll, aes(x = reorder_within(features, -value, provider, sep = " "), y = value, fill = provider)) +
  geom_bar(stat = "identity" ) + 
  facet_wrap(~ provider, scales = "free", labeller = as_labeller(name_mapping)) +
  theme_grey(base_size = 18) +
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = -45, hjust = 0, vjust = 0.5, size = 14),
        legend.position = "none", axis.title.x = element_text(size = 30), axis.title.y = element_text(size = 30),
        plot.title = element_text(size = 30)) +
  ggtitle("Feature Importance der verschiedenen Provider - Link-Lifetime") + 
  xlab("Features") + 
  ylab("Wichtigkeit")









