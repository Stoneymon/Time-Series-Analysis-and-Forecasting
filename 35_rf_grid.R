training <- grid_pred_dt[grid_pred_dt$Time < as.Date("2022-02-01"),]

library(ranger)
setDT(training)
training_x <- training[,!"grid"]
training_y <- training[,"grid"]
test <- grid_pred_dt[grid_pred_dt$Time < as.Date("2022-02-08"),]
test <- test[test$Time > as.Date("2022-02-01"),]
setDT(test)
test_x <- test[,!"grid"]
test_y <- test[,grid]

fit.ranger <- ranger(grid ~ ., data = training, importance = "permutation")

plot_ly(training, x = training, y = ~grid, name = "actual", type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = ~fit.ranger$predictions, name = 'predictions_rf_training', mode = 'lines+markers') %>%
  layout(title = 'Random Forest Training')

my_pred <- predict(object = fit.ranger, data = test_x)

plot_ly(test, x = test, y = ~grid, name = "actual", type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = ~my_pred$predictions, name = 'predictions_rf_test', mode = 'lines+markers') %>%
  layout(title = 'Random Forest Test')

errors <- data.table(Time = test$Time, 
                      Error = abs(my_pred$predictions - test_y))

mae <- mean(errors$Error)
mae


training$day_of_week <- wday(training$Time, label=TRUE)
test$day_of_week <- wday(test$Time, label=TRUE)
test_x2 <- test[,!"grid"]
test_y2 <- test[,grid]


fit.ranger2 <- ranger(grid ~ ., data = training, importance = "permutation")
plot_ly(training, x = training, y = ~grid, name = "actual", type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = ~fit.ranger2$predictions, name = 'predictions_rf_training', mode = 'lines+markers') %>%
  layout(title = 'Random Forest Training')

my_pred2 <- predict(object = fit.ranger2, data = test_x2)
plot_ly(test, x = test$Time, y = ~grid, name = "actual", type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = ~my_pred2$predictions, name = 'predictions_rf_test', mode = 'lines+markers') %>%
  layout(title = 'Random Forest Test')

errors2 <- data.table(Time = test$Time, 
                      Error = abs(my_pred2$predictions - test_y2))

mae2 <- mean(errors2$Error)
mae2
