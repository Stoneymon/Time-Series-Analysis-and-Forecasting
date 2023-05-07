  # Load the necessary libraries
  library(forecast)
  
  wallboxes_ts <- ts(wallboxes_jan_aug$total_power, start = c(2022, 1), frequency = 365)

  ts_train <- window(wallboxes_ts, end = c(2022,186))
  ts_test <- window(wallboxes_ts, start = c(2022, 187))
  
  sarima_model <- auto.arima(ts_train)
  
  checkresiduals(sarima_model)
  
  sarima_pred <- forecast(sarima_model, h = length(ts_test))
  
  accuracy(sarima_pred, ts_test)
  
  sarima_model <- auto.arima(ts_train)
  
  
  sarima_pred <- forecast(sarima_model, h = 47)
  
  plot(sarima_pred, xlab = "Time", ylab = "Total Power", main = "SARIMA Forecast for Wallboxes Data")
  lines(ts_test, col = "blue")
  
  
  
  ############################################################
  # Cross Validation

  
  #sarima_model <- auto.arima(ts_train)
  
  #sarima_pred <- forecast(sarima_model, h = length(ts_test))
  #autoplot(sarima_pred)

  #sarima_pred
  
  #predicted_vals <- sarima_pred$mean
  #actual_vals <- ts_test
  
  
  #actual_vals
  
  actual_vals <- c()
  predicted_vals <- c()
  
  
  for (i in 1:(length(wallboxes_ts)-186)) {
    ts_train <- window(wallboxes_ts, end = c(2022, 186+i))
    
    sarima_model <- auto.arima(ts_train)
    
    sarima_pred <- forecast(sarima_model, h = 1)
    
    predicted_vals <- c(predicted_vals, sarima_pred$mean)
    
    actual_vals <- c(actual_vals, wallboxes_ts[186+i+1])
  }
  
  checkresiduals(sarima_model)
  
  accuracy(predicted_vals, actual_vals)
  
  plot(predicted_vals, type = "l", xlab = "Days", ylab = "Total Power", main = "Time Series Cross-Validation SARIMA Predictions",ylim = c(0, 28000),col = "red")
  lines(actual_vals, col = "blue")
  legend("topright", legend = c("Predicted", "Actual"), col = c("red", "blue"), lty = 1)
  
 
  
actual_vals
