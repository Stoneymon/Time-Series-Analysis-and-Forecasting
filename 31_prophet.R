library(prophet)
library(forecast)

wallboxes_jan_april <- wallboxes_jan_aug[wallboxes_jan_aug$Date < as.Date("2022-05-01"),]
wallboxes_jan_april <- subset(wallboxes_jan_april, select= c("Date", "total_power"))
names(wallboxes_jan_april) <- c("ds", "y")
test <- wallboxes_jan_aug[wallboxes_jan_aug$Date >= as.Date("2022-05-01"),]
test <- test[test$Date < as.Date("2022-05-08"),]
test <- subset(test, select = c("Date", "total_power"))


m <- prophet(wallboxes_jan_april)
future <- make_future_dataframe(m, periods=7)

forecast <- predict(m, future)
forecast

plot(m, forecast)

prophet_plot_components(m, forecast)

data <- subset(forecast, select = c("ds", "yhat_lower", "yhat_upper", "yhat"))
data <- data[data$ds >= as.POSIXct("2022-05-01"),]
data$actuals <- test$total_power
#actuals <- wallboxes_jan_aug[wallboxes_jan_aug$Date >= as.Date("2022-05-01")]
#actuals <- actuals[actuals < as.POSIXct("2022-06-01")]

result_plot <- plot_ly(data, x = ~ds, y = ~yhat, name="Forecast", type="scatter", mode="line")
result_plot <- result_plot %>% add_trace(y = ~actuals, name="Actuals", type="scatter")
result_plot


# use 2 weeks to train prophet, predict one day

actuals <- subset(wallboxes_jan_aug, select = c("Date", "total_power"))
names(actuals) <- c("ds", "y")
start_date <- as.Date("2022-01-01")
end_date <- as.Date("2022-01-15")
predictions <- list() # the predictions will get stored in this list
errors <- list() # the errors will get stored in this list
i <- 1
while (end_date <= as.Date("2022-08-21")) {
  training <- actuals[actuals$ds >= start_date, ]
  training <- actuals[actuals$ds < end_date,] # use 2 weeks to train the model
  m <- prophet(training, weekly.seasonality=TRUE, yearly.seasonality=FALSE, daily.seasonality=FALSE)
  future <- make_future_dataframe(m, periods=1) # predict 1 day
  forecast <- predict(m, future)
  pred <- forecast[nrow(forecast), "yhat"]
  predictions[[i]] <- pred # store the prediction
  actual <- as.numeric(actuals[actuals$ds == end_date, "y"]) 
  errors[[i]] <-abs(pred - actual) # store the difference between prediction vs actual value
  i <- i+1
  start_date <- start_date + 1
  end_date <- end_date + 1
}
mean_error <- mean(unlist(errors))
mean_error

results <- actuals[actuals$ds > as.Date("2022-01-14"),]
results$pred <- unlist(predictions)
result_plot <- plot_ly(results, x = ~ds, y = ~y, name="Actuals", type="scatter", mode="line") %>%
  add_trace(y = ~pred, type="scatter", name="Forecast") %>%
  layout(xaxis = list(title="Date"), yaxis =list(title="kWh"))
result_plot
