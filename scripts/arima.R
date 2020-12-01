library(forecast)
library(fastDummies)

setwd("~/fallstudien_2_projekt_1/data_raw/campus")
ul_filenames <- dir("~/fallstudien_2_projekt_1/data_raw/campus") %>% 
  grep("ul", ., value=TRUE) 
providers <- c("o2", "vodafone", "tmobile")
ul_data <- data.frame()
for (provider in providers){
  ul_data <- ul_filenames %>% grep(provider, ., value=TRUE) %>%
    lapply(read.csv2, header=TRUE, sep=",", dec=".") %>% do.call(rbind, .) %>% 
    mutate(provider=provider) %>% rbind(ul_data) %>% na.omit()
}

# Select features
ul_data$number <- 1:nrow(ul_data)
ul_data$day <- ul_data$timestamp_ms %>% anytime() %>% format("%d") %>% as.integer()
ul_data$hour <- ul_data$timestamp_ms %>% anytime() %>% format("%H") %>% as.integer()

features <- c("throughput_mbits", "payload_mb", "f_mhz", 
              "rsrp_dbm", "rsrq_db", "pci", "ta", "id", "velocity_mps", 
              "provider", "day", "hour")
numeric_features <- features[as.vector(unlist(lapply(ul_data[, features], is.numeric)))]
ycol <- "throughput_mbits"

X <- ul_data[, features] %>% dummy_cols(
  select_columns = c("provider"), 
  remove_selected_columns = TRUE, 
  remove_first_dummy = TRUE
  )


# Scale Data
ntrain <- floor(nrow(X)*0.9)
ntest <- nrow(X) - ntrain

scaled <- scale(X[1:ntrain, numeric_features])
center_history = attr(scaled, "scaled:center")
scale_history = attr(scaled, "scaled:scale")

traindata <- X[1:ntrain, ]
traindata[numeric_features] <- scaled

testdata <- X[(ntrain+1):(ntrain+ntest), ]
testdata[numeric_features] <- scale(testdata[numeric_features], 
                                    center = center_history, 
                                    scale = scale_history)

# Create input
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

# Model
model <- auto.arima(ytrain, xreg = Xtrain, lambda = 0)

# Prediction
prediction <- forecast(model, xreg = Xtest, lambda = 0)

# Reverse scaling
pred_values <- as.numeric(prediction$mean * scale_history["throughput_mbits"] + center_history["throughput_mbits"])
actual <- X[(ntrain+1):(ntrain+ntest), ycol]

plot_data <- data.frame(actual = actual, predict = pred_values)
ggplot(
  data = plot_data, aes(x=actual, y=pred_values)
  ) + 
  geom_point() + 
  xlim(min(plot_data), max(plot_data)) + 
  ylim(min(plot_data), max(plot_data))

plot_data <- rbind(data.frame(value=pred_values, type="predict", index=1:length(pred_values)), 
                  data.frame(value=actual, type="actual", index=1:length(actual)))

ggplot(plot_data, aes(x=index, y=value, color=type)) + geom_line()

