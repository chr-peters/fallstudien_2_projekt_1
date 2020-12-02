

setwd("~/GitHub/fallstudien_2_projekt_1/datasets")
library(anytime)

data_context <- read.csv("dataset_context.csv", header = TRUE, sep = ",")
data_cells <- read.csv("dataset_cells.csv", header = TRUE, sep = ",")

data_context <- data_context[complete.cases(data_context),]

data_context_o2 <- data_context[which(data_context$provider == "o2"),]
data_cells_o2 <- data_cells[which(data_cells$provider == "o2"),]

data_context_o2$dwell_time <- ones(dim(data_context_o2)[1], 1) # add column

for (j in 1:dim(data_context_o2)[1]){
  enodeb <- data_context_o2$enodeb[j]
  for (i in j:dim(data_context_o2)[1]){
    if (data_context_o2$enodeb[i] != enodeb){
      data_context_o2$dwell_time[j] <- data_context_o2$time_s[i-1] - data_context_o2$time_s[j]
      break
    }
  }
}

data_context_tmobile <- data_context[which(data_context$provider == "tmobile"),]
data_context_tmobile$dwell_time <- ones(dim(data_context_tmobile)[1], 1) # add column

for (j in 1:dim(data_context_tmobile)[1]){
  enodeb <- data_context_tmobile$enodeb[j]
  for (i in j:dim(data_context_tmobile)[1]){
    if (data_context_tmobile$enodeb[i] != enodeb){
      data_context_tmobile$dwell_time[j] <- data_context_tmobile$time_s[i-1] - data_context_tmobile$time_s[j]
      break
    }
  }
}

data_context_vodafone <- data_context[which(data_context$provider == "vodafone"),]
data_context_vodafone$dwell_time <- ones(dim(data_context_vodafone)[1], 1) # add column

for (j in 1:dim(data_context_vodafone)[1]){
  enodeb <- data_context_vodafone$enodeb[j]
  for (i in j:dim(data_context_vodafone)[1]){
    if (data_context_vodafone$enodeb[i] != enodeb){
      data_context_vodafone$dwell_time[j] <- data_context_vodafone$time_s[i-1] - data_context_vodafone$time_s[j]
      break
    }
  }
}


