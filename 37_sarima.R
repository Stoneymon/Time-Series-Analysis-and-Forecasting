# Load the necessary libraries
library(forecast)

wallboxes_ts <- ts(wallboxes_jan_aug$total_power, start = c(2022, 1), frequency = 365)

# Split the dataset into train and test sets (e.g., 80% train, 20% test)
#train_index <- floor(length(wallboxes_ts) * 0.8)
#test_index <- length(wallboxes_ts) - train_index

# Split the data into training and testing sets
ts_train <- window(wallboxes_ts, start = c(2022, 1), end = c(2022,186))
ts_test <- window(wallboxes_ts, start = c(2022, 187))

# Fit the SARIMA model
sarima_model <- auto.arima(ts_train)

# Validate the SARIMA model
checkresiduals(sarima_model)

# Make predictions
sarima_pred <- forecast(sarima_model, h = length(ts_test))

# Evaluate the SARIMA model
accuracy(sarima_pred, ts_test)

# Fine-tune the SARIMA model
sarima_model <- auto.arima(ts_train, stepwise = FALSE, approximation = FALSE)

# Make final predictions
sarima_pred <- forecast(sarima_model, h = 47)

# Plot the results
plot(sarima_pred, xlab = "Time", ylab = "Total Power", main = "SARIMA Forecast for Wallboxes Data")
lines(ts_test, col = "blue")
