training <- grid_pred_dt[grid_pred_dt$Time < as.Date("2022-02-01"),]

library(ranger)
setDT(training)
training_x <- training[,!"grid_next_hour"]
training_y <- training[,"grid_next_hour"]
test <- grid_pred_dt[grid_pred_dt$Time < as.Date("2022-02-08"),]
test <- test[test$Time > as.Date("2022-02-01"),]
setDT(test)
test_x <- test[,!"grid_next_hour"]
test_y <- test[,"grid_next_hour"]

fit.ranger <- ranger(grid_next_hour ~ ., data = training, importance = "permutation")

plot_ly(training, x = training, y = ~grid, name = "actual", type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = ~fit.ranger$predictions, name = 'predictions_rf_training', mode = 'lines+markers') %>%
  layout(title = 'Random Forest Training')

my_pred <- predict(object = fit.ranger, data = test_x)

plot_ly(test, x = test, y = ~grid_next_hour, name = "actual", type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = ~my_pred$predictions, name = 'predictions_rf_test', mode = 'lines+markers') %>%
  layout(title = 'Random Forest Test')

errors <- data.table(Time = test$Time, 
                      Error = abs(my_pred$predictions - test_y))

mae <- mean(errors$Error)
mae


grid_pred_dt$day_of_week <- wday(grid_pred_dt$Time, label=TRUE)
for (i in 25:nrow(grid_pred_dt)) {
  time <- grid_pred_dt[i]$Time - days(1)
  value <- as.double(grid_pred_dt[grid_pred_dt$Time == time, "grid"])
  grid_pred_dt[i, day_before:=value]
}
grid_pred_dt <- na.omit(grid_pred_dt)
start <- 1L
end <- 48L
predictions <- list() # the predictions will get stored in this list
errors <- list() # the errors will get stored in this list
pes <- list() # list of percentage errors
actuals <- list()
i <- 1
set.seed(1234)
while (end < (nrow(grid_pred_dt) - 1)) {
  training <- grid_pred_dt[start:end-1L,]
  test <- grid_pred_dt[end,]
  test_x <- test[,!"grid_next_hour"]
  test_y <- test[,"grid_next_hour"]
  actuals[[i]] <- test_y
  fit.ranger <- ranger(grid_next_hour ~ ., data = training, importance = "permutation")
  pred <- predict(object = fit.ranger, data = test_x)$predictions
  predictions[[i]] <- pred
  error <- abs(test_y$grid_next_hour - pred)
  errors[[i]] <- error
  pe <- error/abs(test_y$grid_next_hour) * 100
  pes[[i]] <- pe
  start <- start + 1L
  end <- end + 1L
  i <- i + 1
}
mean_error <- mean(unlist(errors))
mean_error
mape <- mean(unlist(pes))
mape
library(MLmetrics)
MAE(unlist(predictions), unlist(actuals))
MAPE(unlist(predictions), unlist(actuals))

# mean error: 34.01283
# mean error with day_of_week column: 34.19935
# mean error with day_before column: 22.63029
# mean error with day_before column without day_of_week column: 22.53355
# mean error with week_before column: 22.79481
results <- grid_pred_dt[50:nrow(grid_pred_dt)-1,]
results$pred <- unlist(predictions)

plot_ly(results, x = ~Time, y = ~grid_next_hour, name = "actual", type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = ~pred, name = 'predictions_rf_test', mode = 'lines+markers') %>%
  layout(title = 'Random Forest Test', yaxis=list(title="grid (kWh)"))
