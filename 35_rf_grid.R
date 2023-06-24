# LIBARIES ----
library(ranger)

# SPLITTING THE DATA INTO TRAINING AND TEST SET ----
# use 1 month of data for training
training <- grid_pred_dt[1:48,]
setDT(training)
training_x <- training[,!"grid_next_hour"]
training_y <- training[,grid_next_hour]

# 1 week for testing
test <- grid_pred_dt[48:72,]
setDT(test)
test_x <- test[,!"grid_next_hour"]
test_y <- test[,grid_next_hour]

# FUNCTION FOR MAE AND MAPE CALCULATION
calculate <- function(n, pred, actual) {
  errors <- list()
  pe <- list()
  for (i in 1:n) {
    error <- abs(actual[[i]] - pred[[i]])
    errors[[i]] <- error
    pe[[i]] <- error / abs(actual[[i]]) * 100
  }
  mae <- mean(unlist(errors))
  mape <- mean(unlist(pe))
  return(data.frame(MAE = mae, MAPE = mape))
}


# RANDOM FOREST ---
set.seed(1234)
fit.ranger <- ranger(grid_next_hour ~ ., data = training, importance = "permutation")

plot_ly(training, x = training, y = ~grid, name = "actual", type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = ~fit.ranger$predictions, name = 'predictions_rf_training', mode = 'lines+markers') %>%
  layout(title = 'Random Forest Training')

training_errors <- calculate(length(training_y), fit.ranger$predictions, training_y)
training_errors$MAE # 2.941976
training_errors$MAPE # 452.7766

my_pred <- predict(object = fit.ranger, data = test_x)

plot_ly(test, x = test, y = ~grid_next_hour, name = "actual", type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = ~my_pred$predictions, name = 'predictions_rf_test', mode = 'lines+markers') %>%
  layout(title = 'Random Forest Test')

test_errors <- calculate(length(test_y), my_pred$predictions, test_y)
test_errors$MAE # 4.44282
test_errors$MAPE # 540.0902


# FEATURE ENGINEERING ----
# add new features:
# Day of the week, Day before (same Time 24h before), Week before (same time 7 days before)
grid_pred_dt$day_of_week <- wday(grid_pred_dt$Time, label=TRUE) # add the weekday column
for (i in 169:nrow(grid_pred_dt)) { # start at 169 because it's the first value that can have a value 7 days before
  time <- grid_pred_dt[i]$Time - days(1) # get the same time 1 day before
  value <- as.double(grid_pred_dt[grid_pred_dt$Time == time, "grid"])  # get the power drawn from the grid at that time
  grid_pred_dt[i, day_before:=value] # add the value to the datatable (column day_before)
  week <- grid_pred_dt[i]$Time - days(7) # get the same time 7 days before
  week_value <- as.double(grid_pred_dt[grid_pred_dt$Time == week, "grid"]) #(commented out because the feature is not important)
  grid_pred_dt[i, week_before:=week_value] 
}
grid_pred_dt <- na.omit(grid_pred_dt) # get rid of the new NA values

## RANDOM FOREST WITH NEW FEATURES ----
training.fe <- grid_pred_dt[1:48,]
training.fe_y <- training.fe[,grid_next_hour]

set.seed(1234)
fit.ranger.fe <- ranger(grid_next_hour ~ ., data = training.fe, importance = "permutation")

training_errors.fe <- calculate(length(training.fe_y), fit.ranger.fe$predictions, training.fe_y)
training_errors.fe$MAE # 3.647491 with week_before, 3.638129 without
training_errors.fe$MAPE # 176.9341 with week_before, 162.7992 without

test.fe <- grid_pred_dt[49:72,]
test.fe_x <- test.fe[,!"grid_next_hour"]
test.fe_y <- test.fe[,grid_next_hour]

my_pred.fe <- predict(object = fit.ranger.fe, data = test.fe_x)

test_errors.fe <- calculate(length(test.fe_y), my_pred.fe$predictions, test.fe_y)
test_errors.fe$MAE # 11.77533 with week_before, 11.53438 without
test_errors.fe$MAPE # 193.8969 with week_before, 197.7502 without


## FEATURE IMPORTANCE ----
imp <- importance(fit.ranger.fe)
imp <- data.table(Feature = names(imp), importance = imp)
plot_ly(data=imp, x=~Feature, y=~importance, type="bar") %>%
  layout(xaxis = list(categoryorder="total descending"))

# we can remove day_of_week (imp ~= 0.11) and week before (imp ~= 1.18)
grid_pred_dt <- grid_pred_dt[,!"day_of_week"]
# grid_pred_dt <- grid_pred_dt[,!"week_before"]


# FINAL MODEL ----
start <- 1L
end <- 49L
predictions <- list() # the predictions will get stored in this list
errors <- list() # the errors will get stored in this list
pes <- list() # list of percentage errors
bl_errors <- list() # list of baseline (last known value) error
bl_pes <- list() # list of fbaseline percentage error
actuals <- list() # list of the actual values
i <- 1 # index
set.seed(1234)
while (end <= (nrow(grid_pred_dt)-1)) { # loop until we predict the last value we have / we can compare to the actual value
  training <- grid_pred_dt[start:end-1L,] # 48 hours for training
  test <- grid_pred_dt[end,] # 1 hour for predicting
  test_x <- test[,!"grid_next_hour"]
  test_y <- test[,grid_next_hour]
  actuals[[i]] <- test_y # add the actual value to the list at index i
  fit.ranger <- ranger(grid_next_hour ~ ., data = training, importance = "permutation")
  pred <- predict(object = fit.ranger, data = test_x)$predictions
  # time[[i]] <- grid_pred_dt[end + 1]$Time
  time[[i]] <- grid_pred_dt[end+1]$Time
  predictions[[i]] <- pred # add the prediction to the list
  error <- abs(test_y - pred)
  errors[[i]] <- error # add the absolute error to the list
  pes[[i]] <- error/abs(test_y) * 100 # add the percentage error to the list
  
  bl_error <- abs(test_y - test_x[, "grid"])
  bl_errors[[i]] <- bl_error
  bl_pes[[i]] <- bl_error / abs(test_y) * 100
  
  start <- start + 1L
  end <- end + 1L
  i <- i + 1
}
mae <- mean(unlist(errors))
mae # 5.82381
mape <- mean(unlist(pes))
mape # 1275.265

mae_bl <- mean(unlist(bl_errors))
mae_bl # 4.858213
mape_bl <- mean(unlist(bl_pes))
mape_bl # 453.779

# add a datatable with the results and the predictions
results <- grid_pred_dt[50:nrow(grid_pred_dt), c("Time", "grid")]
results$pred <- unlist(predictions)

plot_ly(results, x = ~Time, y = ~grid, name = "actual", type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = ~pred, name = 'predictions_rf_test', mode = 'lines+markers') %>%
  layout(title = 'Random Forest Test', yaxis=list(title="grid (kWh)"))
