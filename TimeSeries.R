##### TIME SERIES #####

# This script is a generalization of distinct methods of time series forecasting.
# The main idea is to apply the methods to a univariate time series and select
# the best method looking for the one that minimizes the Mean Error, then a prediction
# is made.

# init

# Set the working directory where the file is located
DIRECTORY <- "C:/Users/sebastian.morales/Desktop"

FILE_NAME <- "JFM.csv"
DATE_INIT <- c(2012, 2)
TIME_FREQ <- 52
TIME_FORECAST <- 52

# data input
MY_DATA <- read.csv(paste(DIRECTORY,FILE_NAME, sep = "/"), sep = ",",
                    stringsAsFactors = FALSE)

# attach
attach(MY_DATA)

X <- GHM

# packages
PACKAGES <- c("stats", "forecast")

if (length(setdiff(PACKAGES, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(PACKAGES, rownames(installed.packages())))  
}

library(stats)
library(forecast)

# time series
TRAIN_IDX <- 1:trunc(length(X)-TIME_FREQ)
YEAR <- trunc(length(TRAIN_IDX) / TIME_FREQ)
PERIOD <- ((length(TRAIN_IDX) / TIME_FREQ) - YEAR) * TIME_FREQ

TRAIN_TS <- ts(X[TRAIN_IDX], start = DATE_INIT, frequency = TIME_FREQ)
TEST_TS <- ts(X[-TRAIN_IDX], start = c(DATE_INIT[1]+YEAR, PERIOD+1), frequency = TIME_FREQ)
X <- ts(X, start = DATE_INIT, frequency = TIME_FREQ)

ARIMA_TS <- auto.arima(TRAIN_TS)
HOLTWINTERS_TS <- HoltWinters(TRAIN_TS)
NNETAR <- nnetar(TRAIN_TS)

FORECAST_TS <- forecast(TRAIN_TS, h = length(TEST_TS))
FORECAST_ARIMA <- forecast(ARIMA_TS, h = length(TEST_TS))
FORECAST_HOLTWINTERS <- forecast(HOLTWINTERS_TS, h = length(TEST_TS))
FORECAST_NNETAR <- forecast(NNETAR, h = length(TEST_TS))

# results
(ACCURACY_TS <- accuracy(FORECAST_TS, TEST_TS))
(ACCURACY_ARIMA <- accuracy(FORECAST_ARIMA, TEST_TS))
(ACCURACY_HOLTWINTERS <- accuracy(FORECAST_HOLTWINTERS, TEST_TS))
(ACCURACY_NNETAR <- accuracy(FORECAST_NNETAR, TEST_TS))

MAPE <- abs(c(ACCURACY_TS[2,5], ACCURACY_ARIMA[2,5],
    ACCURACY_HOLTWINTERS[2,5], ACCURACY_NNETAR[2,5]))

if(min(MAPE) == abs(ACCURACY_TS[2,5])) {
  BEST_FORECAST <- FORECAST_TS
  FINAL_FORECAST <- forecast(X, h = TIME_FORECAST)
  
} else {
  if(min(MAPE) == abs(ACCURACY_ARIMA[2,5])) {
    BEST_FORECAST <- FORECAST_ARIMA
    FINAL_FORECAST <- forecast(auto.arima(X), h = TIME_FORECAST)
    
  } else {
    if(min(MAPE) == abs(ACCURACY_HOLTWINTERS[2,5])) {
      BEST_FORECAST <- FORECAST_HOLTWINTERS
      FINAL_FORECAST <- forecast(HoltWinters(X), h = TIME_FORECAST)
      
    } else {
      if(min(MAPE) == abs(ACCURACY_NNETAR[2,5])) {
        BEST_FORECAST <- FORECAST_NNETAR
        FINAL_FORECAST <- forecast(nnetar(X), h = TIME_FORECAST)
      }
    }
  }
}

paste("Best Method is", BEST_FORECAST$method)

plot(BEST_FORECAST)
lines(TEST_TS, col = "black", lty = 2)

plot(FINAL_FORECAST$mean, main = "Forecast", ylab = "Value")
FINAL_FORECAST$model


## Export data
decomposed <- decompose(X)
decomposed.data <- data.frame(x = decomposed$x, seasonal = decomposed$seasonal,
                              trend = decomposed$trend)
write.csv(decomposed.data, "./decoDATA.csv")
