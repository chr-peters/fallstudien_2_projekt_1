# RSRP zu anderen Zellen
# RSRP (Referenzsignal der Empfangsfeldstärke am Endgerät) - steht für die Signalstärke

library(readr)
library(matlab)

setwd("~/GitHub/fallstudien_2_projekt_1/datasets")

# RSRP zu anderen Zellen
# RSRP (Referenzsignal der Empfangsfeldstärke am Endgerät) - steht für die Signalstärke

# daten einlesen
dataset_context <- read.csv("dataset_context.csv", header = TRUE, sep = ",")
dataset_cells <- read.csv("dataset_cells.csv", header = TRUE, sep = ",")

# Na Zeilen entfernen, bzw. in cells die Zeilen mit rsrp = 0, 
# da dort viele features Na's besitzen
dataset_context <- dataset_context[complete.cases(dataset_context),]
dataset_cells <- dataset_cells[-which(dataset_cells$rsrp_dbm == 0),]

# füge zusätzliche Spalte mit erstmal nur Nullen in context, wo dann am Ende die errechneten
# RSRP der Nachbarzellen eingefügt werden sollen 
dataset_context$rsrp_neighbor <- zeros(dim(dataset_context)[1], 1)
dataset_context$rsrq_neighbor <- zeros(dim(dataset_context)[1], 1)


for (p in c("o2", "tmobile", "vodafone")){
	dataset_context_p <- dataset_context[which(dataset_context$provider == p),]
	dataset_cells_p <- dataset_cells[which(dataset_cells$provider == p),]

	for (s in unique(dataset_context_p$scenario)){
    		dataset_context_ps <- as.data.frame(dataset_context_p[which(dataset_context_p$scenario == s),])
		dataset_cells_ps <- as.data.frame(dataset_cells_p[which(dataset_cells_p$scenario == s),])
    		
		for (d in unique(dataset_context_ps$drive_id)){
      			dataset_context_psd <- as.data.frame(dataset_context_ps[which(dataset_context_ps$drive_id == d),])
			dataset_cells_psd <- as.data.frame(dataset_cells_ps[which(dataset_cells_ps$drive_id == d),])
      			for (i in 1:dim(dataset_context_psd)[1]){
				if (any(dataset_cells_psd$timestamp == dataset_context_psd$timestamp[i])){
					neighbor <- dataset_cells_psd[which(dataset_cells_psd$timestamp == dataset_context_psd$timestamp[i]),]
	  				max_rsrp <- max(neighbor$rsrp_dbm)
	  				dataset_context_psd$rsrp_neighbor[i] <- max_rsrp

					neighbor <- dataset_cells_psd[which(dataset_cells_psd$timestamp == dataset_context_psd$timestamp[i]),]
	  				max_rsrq <- max(neighbor$rsrq_db)
	  				dataset_context_psd$rsrq_neighbor[i] <- max_rsrq
				}
				else {
					if (any(dataset_cells_psd$timestamp < dataset_context_psd$timestamp[i])){
						dataset_context_psd$rsrp_neighbor[i] <- dataset_context_psd$rsrp_neighbor[i-1]
						dataset_context_psd$rsrq_neighbor[i] <- dataset_context_psd$rsrq_neighbor[i-1]
					}
					else {
						dataset_context_psd$rsrp_neighbor[i] <- -Inf		
						dataset_context_psd$rsrq_neighbor[i] <- -Inf
					}
				}		
			}
			dataset_context[(dataset_context$provider == p & dataset_context$scenario == s & dataset_context$drive_id == d),] <- dataset_context_psd
		}
	}
}

write_csv(dataset_context, "../datasets/dataset_context.csv")
