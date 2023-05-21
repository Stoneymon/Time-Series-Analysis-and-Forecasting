library(prophet)
library(forecast)

wallboxes_jan_april <- wallboxes_jan_aug[wallboxes_jan_aug$Date < as.Date("2022-05-01"),]
wallboxes_jan_april <- subset(wallboxes_jan_april, select= c("Date", "total_power"))
names(wallboxes_jan_april) <- c("ds", "y")
test <- wallboxes_jan_aug[wallboxes_jan_aug$Date >= as.Date("2022-05-01"),]
test <- test[test$Date < as.Date("2022-06-01"),]
test <- subset(test, select = c("Date", "total_power"))


m <- prophet(wallboxes_jan_april)
future <- make_future_dataframe(m, periods=7)

forecast <- predict(m, future)
forecast

plot(m, forecast)

prophet_plot_components(m, forecast)

data <- subset(forecast, select = c("ds", "yhat_lower", "yhat_upper", "yhat"),)
data <- data[data$ds >= as.POSIXct("2022-06-01"),]
actuals <- wallboxes_jan_april[wallboxes_jan_april$y >= as.POSIXct("2022-05-01")]
actuals <- actuals[actuals < as.POSIXct("2022-06-01")]

result_plot <- plot_ly(data, x = ~ds, y = ~yhat_lower, name="Minimum Forecast", type="scatter", mode="line")
result_plot <- result_plot %>% add_trace(y = ~yhat_upper, name="Maximum Forecast", type="scatter", mode="line", fill="tonexty", color="green")
result_plot <- result_plot %>% add_trace(y = ~yhat, name="Actuals", type="scatter")
result_plot


# TODO: Add isweekend column
# TODO; predict only 7 days, shift by one day, etc.
# TODO: average error (MAE)