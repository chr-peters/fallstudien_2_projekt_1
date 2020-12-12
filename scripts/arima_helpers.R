library(ggplot2)
library(grid)
library(rlist)
library(forecast)

NOTWORKINGplot_acf <- function(throughputs){
  
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(3,2, heights = unit(c(1, 5, 5), "null"))))
  layout <- list(c(2,1), c(2,2), c(3,1))
  
  for (i in length(throughputs)){
    plot <- ggAcf(throughputs[[i]]$throughput_mbits) + 
      ggtitle(unique(throughputs[[i]]$provider)) + 
      ylab("Korrelation")
    print(plot, vp=viewport(layout.pos.row = layout[[i]][1], 
                            layout.pos.col = layout[[i]][2]))
  }
  
  grid.text("Autokorrelationsfunktionen", vp = viewport(layout.pos.row = 1, 
                                                        layout.pos.col = 1:2))
  
  return(NULL)
}

plot_acf <- function(throughputs, type = c("acf", "pacf"), 
                     title="Autokorrelationsfunktionen"){
  
  grid.newpage()
  pushViewport(viewport(
    layout=grid.layout(3,2, heights = unit(c(1, 5, 5), "null"))))
  
  if (type == "acf") {
    chosen_func <- ggAcf
    grid.text(title, vp = viewport(layout.pos.row = 1, 
                                                          layout.pos.col = 1:2))
    }
  else {
    chosen_func <- ggPacf
    grid.text(title, 
              vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
    }
  
  vodafone_plot <- chosen_func(throughputs$vodafone) + ggtitle("Vodafone") +
    ggtitle("Vodafone") + ylab("Korrelation")
  tmobile_plot <- chosen_func(throughputs$tmobile) + ggtitle("T-Mobile") + 
    ggtitle("T-Mobile") + ylab("Korrelation")
  o2_plot <- chosen_func(throughputs$o2) + ggtitle("O2") + ylab("Korrelation")
  
  print(vodafone_plot, vp=viewport(layout.pos.row = 2, layout.pos.col = 1))
  print(tmobile_plot, vp=viewport(layout.pos.row = 2, layout.pos.col = 2))
  print(o2_plot, vp=viewport(layout.pos.row = 3, layout.pos.col = 1))
  
}

plot_ccf <- function(provider, features, lag.max=10){
  
  if (any(features=="throughput_mbits")) 
    features <- features[-which(features=="throughput_mbits")]
  ncol <- ceiling(sqrt(length(features)))
  nrow <- ceiling(length(features)/ncol) + 1
  
  grid.newpage()
  pushViewport(
    viewport(layout=grid.layout(
      nrow+1,ncol, heights = unit(c(1, rep(5, length(features))), "null"))))
  grid.text(
    paste(
      "Kreuzkorrelationsfunktion:", 
      unique(provider["provider"]), sep = " "), 
    vp = viewport(layout.pos.row = 1, layout.pos.col = 1:ncol))
  
  lag.max <- 10
  i <- 2
  j <- 1
  
  if(any(features=="distance_m")){
    plot <- ggCcf(x=provider["throughput_mbits"], y=provider["distance_m"], 
                  lag.max = lag.max) + 
      ylab("Korrelation") + ggtitle("Distanz in Metern")
    print(plot, vp=viewport(layout.pos.row = i, layout.pos.col = j))
    if(j==ncol) {
      i <- i+1
      j <- 1
    }
    else j <- j + 1
  }
  
  if(any(features=="latitude")){
    plot <- ggCcf(x=provider["throughput_mbits"], y=provider["latitude"], 
                  lag.max = lag.max) + 
      ylab("Korrelation") + ggtitle("Höhenmaß")
    print(plot, vp=viewport(layout.pos.row = i, layout.pos.col = j))
    if(j==ncol) {
      i <- i+1
      j <- 1
    }
    else j <- j + 1
  }
  
  
  if(any(features=="longitude")){
    plot <- ggCcf(x=provider["throughput_mbits"], y=provider["longitude"], 
                  lag.max = lag.max) + 
      ylab("Korrelation") + ggtitle("Breitenmaß")
    print(plot, vp=viewport(layout.pos.row = i, layout.pos.col = j))
    if(j==ncol) {
      i <- i+1
      j <- 1
    }
    else j <- j + 1
  }
  
  if(any(features=="altitude")){
    plot <- ggCcf(x=provider["throughput_mbits"], y=provider["altitude"], 
                  lag.max = lag.max) + 
      ylab("Korrelation") + ggtitle("Höhenlage")
    print(plot, vp=viewport(layout.pos.row = i, layout.pos.col = j))
    if(j==ncol) {
      i <- i+1
      j <- 1
    }
    else j <- j + 1
  }
  
  if(any(features=="velocity_mps")){
    plot <- ggCcf(x=provider["throughput_mbits"], y=provider["velocity_mps"], 
                  lag.max = lag.max) + 
      ylab("Korrelation") + ggtitle("Geschwindigkeit in MpS")
    print(plot, vp=viewport(layout.pos.row = i, layout.pos.col = j))
    if(j==ncol) {
      i <- i+1
      j <- 1
    }
    else j <- j + 1
  }
  
  if(any(features=="acceleration_mpss")){
    plot <- ggCcf(x=provider["throughput_mbits"], y=provider["acceleration_mpss"], 
                  lag.max = lag.max) + 
      ylab("Korrelation") + ggtitle("Beschleunigung in MpS")
    print(plot, vp=viewport(layout.pos.row = i, layout.pos.col = j))
    if(j==ncol) {
      i <- i+1
      j <- 1
    }
    else j <- j + 1
  }
  
  if(any(features=="direction")){
    plot <- ggCcf(x=provider["throughput_mbits"], y=provider["direction"], 
                  lag.max = lag.max) + 
      ylab("Korrelation") + ggtitle("Richtung")
    print(plot, vp=viewport(layout.pos.row = i, layout.pos.col = j))
    if(j==ncol) {
      i <- i+1
      j <- 1
    }
    else j <- j + 1
  }
  
  if(any(features=="rsrp_dbm")){
    plot <- ggCcf(x=provider["throughput_mbits"], y=provider["rsrp_dbm"], 
                  lag.max = lag.max) + 
      ylab("Korrelation") + ggtitle("RSRP in dbm")
    print(plot, vp=viewport(layout.pos.row = i, layout.pos.col = j))
    if(j==ncol) {
      i <- i+1
      j <- 1
    }
    else j <- j + 1
  }
  
  if(any(features=="rsrq_db")){
    plot <- ggCcf(x=provider["throughput_mbits"], y=provider["rsrq_db"], 
                  lag.max = lag.max) + 
      ylab("Korrelation") + ggtitle("RSRQ in db")
    print(plot, vp=viewport(layout.pos.row = i, layout.pos.col = j))
    if(j==ncol) {
      i <- i+1
      j <- 1
    }
    else j <- j + 1
  }
  
  if(any(features=="rssnr_db")){
    plot <- ggCcf(x=provider["throughput_mbits"], y=provider["rssnr_db"], 
                  lag.max = lag.max) + 
      ylab("Korrelation") + ggtitle("RSSNR in db")
    print(plot, vp=viewport(layout.pos.row = i, layout.pos.col = j))
    if(j==ncol) {
      i <- i+1
      j <- 1
    }
    else j <- j + 1
  }
  
  if(any(features=="cqi")){
    plot <- ggCcf(x=provider["throughput_mbits"], y=provider["cqi"], 
                  lag.max = lag.max) + 
      ylab("Korrelation") + ggtitle("Kanalqualitätsindex")
    print(plot, vp=viewport(layout.pos.row = i, layout.pos.col = j))
    if(j==ncol) {
      i <- i+1
      j <- 1
    }
    else j <- j + 1
  }
  
  if(any(features=="ss")){
    plot <- ggCcf(x=provider["throughput_mbits"], y=provider["ss"], 
                  lag.max = lag.max) + 
      ylab("Korrelation") + ggtitle("SS")
    print(plot, vp=viewport(layout.pos.row = i, layout.pos.col = j))
    if(j==ncol) {
      i <- i+1
      j <- 1
    }
    else j <- j + 1
  }
  
  if(any(features=="ta")){
    plot <- ggCcf(x=provider["throughput_mbits"], y=provider["ta"], 
                  lag.max = lag.max) + 
      ylab("Korrelation") + ggtitle("TA")
    print(plot, vp=viewport(layout.pos.row = i, layout.pos.col = j))
    if(j==ncol) {
      i <- i+1
      j <- 1
    }
    else j <- j + 1
  }
  
  if(any(features=="ci")){
    plot <- ggCcf(x=provider["throughput_mbits"], y=provider["ci"], 
                  lag.max = lag.max) + 
      ylab("Korrelation") + ggtitle("Zell-Id")
    print(plot, vp=viewport(layout.pos.row = i, layout.pos.col = j))
    if(j==ncol) {
      i <- i+1
      j <- 1
    }
    else j <- j + 1
  }
  
  if(any(features=="pci")){
    plot <- ggCcf(x=provider["throughput_mbits"], y=provider["pci"], 
                  lag.max = lag.max) + 
      ylab("Korrelation") + ggtitle("physische Zell-Id")
    print(plot, vp=viewport(layout.pos.row = i, layout.pos.col = j))
    if(j==ncol) {
      i <- i+1
      j <- 1
    }
    else j <- j + 1
  }
  
  if(any(features=="payload_mb")){
    plot <- ggCcf(x=provider["throughput_mbits"], y=provider["payload_mb"], 
                  lag.max = lag.max) + 
      ylab("Korrelation") + ggtitle("Payload in mb")
    print(plot, vp=viewport(layout.pos.row = i, layout.pos.col = j))
    if(j==ncol) {
      i <- i+1
      j <- 1
    }
    else j <- j + 1
  }
  
  if(any(features=="rtt_ms")){
    plot <- ggCcf(x=provider["throughput_mbits"], y=provider["rtt_ms"], 
                  lag.max = lag.max) + 
      ylab("Korrelation") + ggtitle("RTT in ms")
    print(plot, vp=viewport(layout.pos.row = i, layout.pos.col = j))
    if(j==ncol) {
      i <- i+1
      j <- 1
    }
    else j <- j + 1
  }
  
  if(any(features=="txPower_dbm")){
    plot <- ggCcf(x=provider["throughput_mbits"], y=provider["txPower_dbm"], 
                  lag.max = lag.max) + 
      ylab("Korrelation") + ggtitle("Sendeleistung in dbm")
    print(plot, vp=viewport(layout.pos.row = i, layout.pos.col = j))
    if(j==ncol) {
      i <- i+1
      j <- 1
    }
    else j <- j + 1
  }
  
  if(any(features=="f_mhz")){
    plot <- ggCcf(x=provider["throughput_mbits"], y=provider["f_mhz"], 
                  lag.max = lag.max) + 
      ylab("Korrelation") + ggtitle("Frequenz in MhZ")
    print(plot, vp=viewport(layout.pos.row = i, layout.pos.col = j))
    if(j==ncol) {
      i <- i+1
      j <- 1
    }
    else j <- j + 1
  }
  
  return(NULL)
}

prepare_data <- function(data, ntrain, ntest, ycol = "throughput_mbits"){
  
  # scale data
  scaled <- scale(data[1:ntrain, numeric_features])
  center_history = attr(scaled, "scaled:center")
  scale_history = attr(scaled, "scaled:scale")
  
  # divide data
  traindata <- data[1:ntrain, ]
  traindata[numeric_features] <- scaled
  
  testdata <- data[(ntrain+1):(ntrain+ntest), ]
  testdata[numeric_features] <- scale(testdata[numeric_features], 
                                      center = center_history, 
                                      scale = scale_history)
  
  Xtrain <- as.matrix(traindata[, colnames(traindata) != ycol])
  ytrain <- ts(traindata[, ycol])
  Xtest <- as.matrix(testdata[, colnames(testdata) != ycol])
  ytest <- ts(testdata[, ycol])
  
  # Check if any columns are 0
  for (col in colnames(Xtrain)){
    if (all(Xtrain[, col] == 0)) {
      Xtrain <- Xtrain[, colnames(Xtrain) != col]
      Xtest <- Xtest[, colnames(Xtest) != col]
    }
  }
  
  result <- list("Xtrain" = Xtrain, "ytrain" = ytrain, 
                 "Xtest" = Xtest, "ytest" = ytest)
  return(result)
  
}
