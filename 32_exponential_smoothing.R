# Here I will be doing exponential smoothing. I will do a simple one as well as utilize a function to get a potentially great model for exponential smoothing.

library(forecast)
library(ggplot2)
#library(readxl)
#wallboxes_jan_aug <- read_excel("./data/preprocessed/total_power_jan-aug.xlsx")

head(wallboxes_jan_aug)

# Time-Series Object 
wallboxes_ts <- ts(wallboxes_jan_aug$total_power, start = c(2022, 1), frequency = 365)

autoplot(wallboxes_ts)
wallboxes_ts

# diff will return a ts object with diff like c(100,110,120) -> c(10,10,10)
# Plot the differenced series
autoplot(diff(wallboxes_ts))

# ACF of the differenced series
ggAcf(diff(wallboxes_ts))
ggAcf(wallboxes_ts)

# indication that this data is more than just random noise and work looking at especially 



Box.test(diff(wallboxes_ts), lag = 10, type = "Ljung")
# p-value = 1.97e-13 Indicating that the time series is not just random noise and it is worth modeling



# Splitting into training and testing sets

train_index <- floor(length(wallboxes_ts)*0.8)
test_index <- length(wallboxes_ts)- train_index

train_set <- window(wallboxes_ts, start = c(2022, 1), end = c(2022, train_index)) # January 1 to July 5
test_set <- window(wallboxes_ts, start = c(2022, train_index+1)) # Starting from July 6

autoplot(train_set) + ggtitle("Training Set")
autoplot(test_set) + ggtitle("Testing Set")






# Simple exponential smoothing 

# Use ses() to forecast the next 47 days of total_power
fc <- ses(train_set, h = 47)
accuracy(fc,test_set)

# Plot the forecast
autoplot(fc) +ggtitle("Simple Exponential Smoothing Forecast")

# RMSE: 5175


# Taking into account Trends - Holt's Trend method

fcholt <- holt(train_set,h=47)
summary(fcholt)

# Plot the forecast
plot(fcholt, main = "Holt's Trend Method - Exponential Smoothing Forecasting", xlab = "Year", ylab = "Total Power")

# Add the test_set data as a red line
lines(test_set, col = "red")

# Check that the residuals look like white noise -> it doesn't
checkresiduals(fcholt)

accuracy(fcholt,test_set)

# RMSE: 4974 only slightly better


# Taking into account Trends and Seasonality with Holt-Winters method

# too little data to take this into account
  #fc1 <- hw(train_set, seasonal = "additive",h=47)
  #fc2<- hw(train_set, seasonal = "multiplicative",h=47)
  #autoplot(fc1) + autolayer(fc2)



# Innovations state space models 
# For exponential smoothing -> there are errors,trends and seasonality there are 18 potential models
# there is a function that takes all of them into account to output the best possible model 


# Fit the ETS model
ets_model <- train_set %>% ets()

autoplot(ets_model)

# Forecast the next 47 days
ets_forecast <- ets_model %>% forecast(h = 47)



# Calculate the accuracy measures
accuracy(ets_forecast, test_set)
# RMSE 5174 but holts trend method was e

checkresiduals(ets_model)

