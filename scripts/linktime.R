library(matlab)
library(readr)

setwd("~/GitHub/fallstudien_2_projekt_1/datasets/")
#data_destination_dir <- "~/GitHub/fallstudien_2_projekt_1/datasets/"

dataset_context <- read.csv("dataset_context.csv", header = TRUE, sep = ",")
#data_cells <- read.csv("dataset_cells.csv", header = TRUE, sep = ",")

dataset_context <- dataset_context[complete.cases(dataset_context),]

dataset_context$link_lifetime <- -1 * ones(dim(dataset_context)[1], 1)


for (p in c("o2", "tmobile", "vodafone")){
  dataset_context_p <- as.data.frame(dataset_context[which(dataset_context$provider == p),])

  for (s in unique(dataset_context_p$scenario)){
    dataset_context_ps <- as.data.frame(dataset_context_p[which(dataset_context_p$scenario == s),])
    
    for (d in unique(dataset_context_ps$drive_id)){
      dataset_context_psd <- as.data.frame(dataset_context_ps[which(dataset_context_ps$drive_id == d),])

      for (j in 1:dim(dataset_context_psd)[1]){
        diff_bildungs_ts = dataset_context_psd$time_s[dim(dataset_context_psd)[1]]
        for (i in min(j+1, dim(dataset_context_psd)[1]):dim(dataset_context_psd)[1]){
          if (dataset_context_psd$enodeb[i] != dataset_context_psd$enodeb[j]){
            diff_bildungs_ts <- dataset_context_psd$time_s[i]
            break
          }
        }
        
        dataset_context_psd$link_lifetime[j] <- diff_bildungs_ts - dataset_context_psd$time_s[j]
      }

      dataset_context[(dataset_context$provider == p & dataset_context$scenario == s & dataset_context$drive_id == d),] <- dataset_context_psd
    }
  }
}

write_csv(dataset_context, "../datasets/dataset_context.csv")
