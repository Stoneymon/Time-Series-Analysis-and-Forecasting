
# LIBRARIES ----
library(prophet)
library(forecast)

# SPLIT THE DATA INTO TRAINING AND TEST SET ----
# create training data
wallboxes_jan_april <- wallboxes_jan_aug[wallboxes_jan_aug$Date < as.Date("2022-05-01"),]

# use only the Date and total_power column
wallboxes_jan_april <- subset(wallboxes_jan_april, select= c("Date", "total_power"))

# rename the columns (Date -> ds, total_power -> y)
names(wallboxes_jan_april) <- c("ds", "y")

# create test data (1 week, May 01 to May 07)
test <- wallboxes_jan_aug[wallboxes_jan_aug$Date >= as.Date("2022-05-01"),]
test <- test[test$Date < as.Date("2022-05-08"),]
test <- subset(test, select = c("Date", "total_power"))


# create the prophet model with standard / automatic parameters
m <- prophet(wallboxes_jan_april)

# create a dataframe with columns from January 01 to May 07
future <- make_future_dataframe(m, periods=7)

# predict the next 7 days
forecast <- predict(m, future)
forecast

plot(m, forecast)

prophet_plot_components(m, forecast)

# PLOTTING RESULTS OF FIRST PREDICTION ----
# select the columns ds (Date), yhat_lower (lower end of prediction range),
# yhat_upper (upper end of prediction range), yhat(predicted value / middle point in prediction range)
data <- subset(forecast, select = c("ds", "yhat_lower", "yhat_upper", "yhat"))
data <- data[data$ds >= as.POSIXct("2022-05-01"),]
data$actuals <- test$total_power
#actuals <- wallboxes_jan_aug[wallboxes_jan_aug$Date >= as.Date("2022-05-01")]
#actuals <- actuals[actuals < as.POSIXct("2022-06-01")]

result_plot <- plot_ly(data, x = ~ds, y = ~yhat, name="Forecast", type="scatter", mode="line")
result_plot <- result_plot %>% add_trace(y = ~actuals, name="Actuals", type="scatter")
result_plot <- result_plot %>% layout(title = "Prophet predictions",
                                      xaxis = list(title="Date"),
                                      yaxis = list(title="Power Output (kWh)"))
result_plot

library(MLmetrics)
first_mae <- MAE(forecast[121:nrow(forecast),"yhat"], test$total_power)
first_mae # 59.77909
first_mape <- MLmetrics::MAPE(forecast[121:nrow(forecast),"yhat"], test$total_power)
first_mape # 0.5595921 -> ~ 55.96%

# use 2 weeks to train prophet, predict one day
data <- subset(wallboxes_jan_aug, select = c("Date", "total_power"))
names(data) <- c("ds", "y")


# baseline for predictions is the last known value
baseline <- data[data$ds >= ("2022-01-14"), ]
baseline <- baseline[1:nrow(baseline)-1,]
baseline 


start_date <- as.Date("2022-01-01")
end_date <- as.Date("2022-01-15")
predictions <- list() # the predictions will get stored in this list
errors <- list() # the errors (predictions) will get stored in this list
pes <- list() # percentage errors
bl_errors <- list() # absolute errors of the baseline
bl_pes <- list() # percentage errors of the baseline
actuals <- list() # store the actual values
i <- 1 # index
while (end_date <= as.Date("2022-08-21")) { # loop until the end day is the last day we have values from
  training <- data[data$ds >= start_date, ]
  training <- data[data$ds < end_date,] # use 2 weeks to train the model
  
  m <- prophet(training, weekly.seasonality=TRUE, yearly.seasonality=FALSE, daily.seasonality=FALSE)
  # disable yearly seasonality since we only use two weeks of data
  # use weekly seasonality since the usage is different depending on the weekday
  # disable daily seasonality since we only have daily data -> can't differentiate between day and night in our data
  
  future <- make_future_dataframe(m, periods=1) # predict 1 day
  forecast <- predict(m, future)
  pred <- forecast[nrow(forecast), "yhat"]
  predictions[[i]] <- pred # store the prediction
  actual <- as.numeric(data[data$ds == end_date, "y"]) 
  actuals[[i]] <- actual # store the actual value
  error <- abs(actual - pred) # calculate the absolute error of the prophet prediction
  errors[[i]] <- error # store the absolute error of the prophet prediction
  pes[[i]] <- error / abs(actual) * 100 # calculate the percentage error of the prophet prediction and store it in the list
  
  baseline_error <- abs(actual - baseline$y[[i]]) # calculate the absolute error of the baseline
  bl_errors[[i]] <- baseline_error # store the absolute error of the baseline
  bl_pes[[i]] <- baseline_error / abs(actual) * 100 # calculate and store the percentage error of the baseline
  
  
  i <- i+1 # increase the index by 1
  start_date <- start_date + 1 # use the next day (after current start date) as the new start date
  end_date <- end_date + 1 # use the day after current end date as the new end date
  
}

mae <- mean(unlist(errors)) # calculate the mean absolute error of the prophet prediction
mae # 66.93662

mape <- mean(unlist(pes)) # calculate the mean absolute percentage error of the prophet prediction
mape # 66.0449


mae_bl <- mean(unlist(bl_errors)) # calculate the mean absolute error of the baseline
mae_bl # 89.5148

mape_bl <- mean(unlist(bl_pes)) # calculate the mean absolute percentage error of the baseline
mape_bl # 84.33121


# store the predictions in a table
results <- data[data$ds > as.Date("2022-01-14"),]
results$pred <- unlist(predictions)

# plot the predictions against the actual values
result_plot <- plot_ly(results, x = ~ds, y = ~y, name="Actuals", type="scatter", mode="line") %>%
  add_trace(y = ~pred, type="scatter", name="Forecast") %>%
  layout(xaxis = list(title="Date"), yaxis =list(title="Daily Power (kWh)"))
result_plot
