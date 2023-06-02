# IMPORT DATA ----
wallboxes_jan_aug_DT <- fread("./data/preprocessed/total_power_jan-aug.csv")

# PREPARATION ----

# remove unnecessary battery_SOC feature for this
wallboxes_jan_aug_DT[, battery_SOC := NULL]

# Convert to data.table
# wallboxes_jan_aug
# wallboxes_jan_aug_DT <- setDT(wallboxes_jan_aug)

# wallboxes_oct_2022_feb_2023
# wallboxes_oct_2022_feb_2023_DT <- setDT(wallboxes_oct_2022_feb_2023)


## Data Enrichment ----
# add more columns that could be used for predicting the total power needed

# 1. total_power used on previous day
wallboxes_jan_aug_DT[, total_power_previous_day := shift(total_power, 1)]
# wallboxes_oct_2022_feb_2023_DT[, total_power_previous_day := shift(total_power, 1)]

# 2. total_power used on the same day last week
wallboxes_jan_aug_DT[, total_power_same_day_previous_week := shift(total_power, 7)]
# wallboxes_oct_2022_feb_2023_DT[, total_power_same_day_previous_week := shift(total_power, 7)]

# 3. average total_power that was used in the last 7 days (rolling average)
# install.packages("zoo")
library(zoo)
wallboxes_jan_aug_DT <- wallboxes_jan_aug_DT %>%
                          mutate(average_total_power_last_seven_days = rollmean(total_power, k = 7, fill = NA, align = "right"))
                                 # total_power_last_six_days = rollmean(total_power, k = 6, fill = NA, align = "right"),
                                 # total_power_last_five_days = rollmean(total_power, k = 5, fill = NA, align = "right"),
                                 # total_power_last_four_days = rollmean(total_power, k = 4, fill = NA, align = "right"),
                                 # total_power_last_three_days = rollmean(total_power, k = 3, fill = NA, align = "right"),
                                 # total_power_last_two_days = rollmean(total_power, k = 2, fill = NA, align = "right"))
                                 
# I found out that this calculation sums the last X values and divides it by X and then writes it to row X, this doesn't
# make sense because when we want to predict the total_power that will be used on a day then we of course don't know the
# total_power of this day already. Therefore all of the solutions have to be shifted by 1. So that for example the rolling average
# of the first 7 days is written in row 8. Meaning we can use this knowledge to predict total_power on day 8.

wallboxes_jan_aug_DT[, average_total_power_last_seven_days := shift(average_total_power_last_seven_days, 1)]
# wallboxes_jan_aug_DT[, total_power_last_six_days := shift(total_power_last_six_days, 1)]
# wallboxes_jan_aug_DT[, total_power_last_five_days := shift(total_power_last_five_days, 1)]
# wallboxes_jan_aug_DT[, total_power_last_four_days := shift(total_power_last_four_days, 1)]
# wallboxes_jan_aug_DT[, total_power_last_three_days := shift(total_power_last_three_days, 1)]
# wallboxes_jan_aug_DT[, total_power_last_two_days := shift(total_power_last_two_days, 1)]


# wallboxes_oct_2022_feb_2023_DT <- wallboxes_oct_2022_feb_2023_DT %>%
#                                     mutate(total_power_last_seven_days = rollmean(total_power, k = 7, fill = NA, align = "right"))
                                           # total_power_last_six_days = rollmean(total_power, k = 6, fill = NA, align = "right"),
                                           # total_power_last_five_days = rollmean(total_power, k = 5, fill = NA, align = "right"),
                                           # total_power_last_four_days = rollmean(total_power, k = 4, fill = NA, align = "right"),
                                           # total_power_last_three_days = rollmean(total_power, k = 3, fill = NA, align = "right"),
                                           # total_power_last_two_days = rollmean(total_power, k = 2, fill = NA, align = "right"))

# wallboxes_oct_2022_feb_2023_DT[, total_power_last_seven_days := shift(total_power_last_seven_days, 1)]
# wallboxes_oct_2022_feb_2023_DT[, total_power_last_six_days := shift(total_power_last_six_days, 1)]
# wallboxes_oct_2022_feb_2023_DT[, total_power_last_five_days := shift(total_power_last_five_days, 1)]
# wallboxes_oct_2022_feb_2023_DT[, total_power_last_four_days := shift(total_power_last_four_days, 1)]
# wallboxes_oct_2022_feb_2023_DT[, total_power_last_three_days := shift(total_power_last_three_days, 1)]
# wallboxes_oct_2022_feb_2023_DT[, total_power_last_two_days := shift(total_power_last_two_days, 1)]

# 4. weekend or not
wallboxes_jan_aug_DT[, is_weekend := weekdays(Date)]
wallboxes_jan_aug_DT$is_weekend <- ifelse(wallboxes_jan_aug_DT$is_weekend %in% c("Samstag", "Sonntag"), 1, 0)
# wallboxes_jan_aug_DT[, weekend := as.factor(weekend)]

str(wallboxes_jan_aug_DT)
summary(wallboxes_jan_aug_DT)
head(wallboxes_jan_aug_DT, n = 15)

## Remove NA values ----
# remove rows containing NA values -> because we shifted some values
colSums(is.na(wallboxes_jan_aug_DT))
wallboxes_jan_aug_DT <- na.omit(wallboxes_jan_aug_DT)
str(wallboxes_jan_aug_DT)
summary(wallboxes_jan_aug_DT)

# colSums(is.na(wallboxes_oct_2022_feb_2023_DT))
# wallboxes_oct_2022_feb_2023_DT <- na.omit(wallboxes_oct_2022_feb_2023_DT)
# str(wallboxes_oct_2022_feb_2023_DT)
# summary(wallboxes_oct_2022_feb_2023_DT)


## Remove date ----
# remove the date column just to be safe so it doesn't interfere with the models
# but store it so we can use it later for plotting the results
# jan_aug <- wallboxes_jan_aug_DT$Date
# oct_feb <- wallboxes_oct_2022_feb_2023_DT$Date

# wallboxes_jan_aug_DT[, Date := NULL]
# wallboxes_oct_2022_feb_2023_DT[, Date := NULL]


# SPLITTING ----
# set.seed(123) # used to make results reproducible
# data <- wallboxes_jan_aug_DT
# idx <- createDataPartition(y = data[, total_power], p = 0.8, list = F, times = 1)
# 
# training <- data[idx]
# training_x <- data[idx, !"total_power"]
# training_y <- data[idx, total_power]
# training_jan_aug <- jan_aug[idx]
# 
# test <- data[!idx]
# test_x <- data[!idx, !"total_power"]
# test_y <- data[!idx, total_power]
# test_jan_aug <- jan_aug[-idx]

## let's try scaling ----
# Date <- actuals$Date
# actuals[, Date := NULL]
# actuals <- as.data.table(scale(actuals)) 
# centering <- attr(actuals, "scaled:center")  # later used for rescaling test with same 
# scaling <- attr(actuals, "scaled:scale")  # later used for rescaling test with same 
# actuals[, Date := Date]


## Feature importance ----
### Linear Model ----
# lin_regr <- lm(total_power ~ ., data = data)
# summary(lin_regr)

# -> Sadly no feature seems to be good for predicting the target variable total_power.


### Random Feature ----
# Let's create a random feature
# We expect that other features are much more important than the random one

# library(ranger)
# training[, random := runif(nrow(training), 1, 100)]
# fit.ranger <- ranger(total_power ~ ., data = training, importance = "permutation")
# imp <- importance(fit.ranger)
# imp <- data.table(Feature = names(imp), importance = imp)
# plot_ly(data=imp, x=~Feature, y=~importance, type="bar") %>%
#   layout(xaxis = list(categoryorder="total descending"))

# -> At least no feature seems to be less important than the random one we just created
# training[, random := NULL]


# Linear Regression ----

# lin_regr <- lm(total_power ~ ., data = training)
# summary(lin_regr)
# 
# training_predictions <- lin_regr$fitted.values
# test_predictions <- predict(lin_regr, test)
# 
# plot_ly(training, x = training_jan_aug, y = ~total_power, name = "actual", type = 'scatter', mode = 'lines+markers') %>%
#   add_trace(y = ~training_predictions, name = 'predictions', mode = 'lines+markers') %>%
#   layout(title = 'Linear Regression Training') # plot_bgcolor = "#e5ecf6"

# -> When looking at the plot we see that the model is not capable of producing a got result on the training data.

# plot_ly(test, x = test_jan_aug, y = ~total_power, name = "actual", type = 'scatter', mode = 'lines+markers') %>%
#   add_trace(y = ~test_predictions, name = 'predictions', mode = 'lines+markers')  %>%
#   layout(title = 'Linear Regression Test')
# -> So not really surprisingly also the performance on the test data is far from good.


## Linear Regression Metrics ----
# MAPE <- function(predicted, actual){
#   mape <- mean(abs((actual - predicted)/actual))*100
#   return (mape)
# }

# Training data
# MAE(training_predictions, training_y) # MAE on training data for linear regression
# RMSE(training_predictions, training_y) # RMSE on training data for linear regression
# MAPE(training_predictions, training_y) # MAPE on training data for linear regression (in percent)

# Test data
# MAE(test_predictions, test_y)
# RMSE(test_predictions, test_y)
# MAPE(test_predictions, test_y)

# -> Also the metrics look pretty bad.


# Polynomial Regression ----
# poly_regr <- list()
# MAEs_training <- vector()
# RMSEs_training <- vector()
# MAPEs_training <- vector()
# MAEs_test <- vector()
# RMSEs_test <- vector()
# MAPEs_test <- vector()
# for (d in 1:6) {
#   poly_regr[[d]] <- lm(total_power ~ poly(total_power_previous_day, total_power_same_day_previous_week, total_power_last_seven_days, degree = d), data = training)
# 
#   training_predictions <- poly_regr[[d]]$fitted.values
#   training_actuals <- training[, total_power]
#   MAEs_training[d] <- MAE(training_predictions, training_actuals)
#   RMSEs_training[d] <- RMSE(training_predictions, training_actuals)
#   MAPEs_training[d] <- MAPE(training_predictions, training_actuals)
# 
#   test_predictions <- predict(poly_regr[[d]], test)
#   test_actuals <- test[, total_power]
#   MAEs_test[d] <- MAE(test_predictions, test_actuals)
#   RMSEs_test[d] <- RMSE(test_predictions, test_actuals)
#   MAPEs_test[d] <- MAPE(test_predictions, test_actuals)
# }
# 
# plot_ly(x = 1:length(MAEs_training),y = MAEs_training, type = "scatter", mode = "lines+markers", name = "MAE Training") %>%
#   add_trace(y = MAEs_test, name = "MAE Test", mode = "lines+markers")

# -> Looking at the graph I would choose a degree of 1 because here the MAE on the test set is the smallest.

## Polynomial Regression Metrics ----
# for (x in 1:6) {
#   print(paste("Degree:", x))
# 
#   print("Training Results")
#   print(paste("MAE:", MAEs_training[x]))
#   print(paste("RMSE:", RMSEs_training[x]))
#   print(paste("MAPE:", MAPEs_training[x]))
# 
#   print("Test Results")
#   print(paste("MAE:", MAEs_test[x]))
#   print(paste("RMSE:", RMSEs_test[x]))
#   print(paste("MAPE:", MAPEs_test[x]))
# 
#   print("-------------------------------")
# 
# }

# -> Looking at the actual numbers we see the model already starts to over fit at a degree of 2 (MAE in Test Results)


# Decision Tree ----
# library(rpart) # Decision trees
# library(rpart.plot)  # plots for decision trees
# 
# fit <- rpart::rpart(total_power ~ ., data = training)
# print(fit)
# printcp(fit)  # cp: complexity parameter
# plotcp(fit)
# summary(fit)
# rpart.plot(fit, type = 2, extra = 101, fallen.leaves = F, main = "Initial Regression Tree", tweak = 1.2)

# fit.entire <- rpart::rpart(total_power ~ ., data = training, control = rpart.control(minsplit = 1, minbucket = 1, cp = 0))
# print(fit.entire)
# printcp(fit.entire)
# plotcp(fit.entire)
# summary(fit.entire)
# rpart.plot(fit.entire, type = 2, extra = 101, fallen.leaves = F, tweak = 1.2, main = "Entire Regression Tree")

# And now we prune it at the optimal level of CP
# best_cp_for_pruning <- fit.entire$cptable[which.min(fit.entire$cptable[, "xerror"]), "CP"]
# fit.entire.pruned <- prune(fit.entire, cp = best_cp_for_pruning)

# This is our final tree
# fit.entire.pruned
# printcp(fit.entire.pruned)
# rpart.plot(fit.entire.pruned, type = 2, extra = 101, fallen.leaves = F, tweak = 1.2, main = "Pruned Regression Tree")


## Performance ----
# my_pred_initial_tree_TRAINING <- predict(fit, newdata = training_x)
# my_pred_entire_tree_TRAINING <- predict(fit.entire, newdata = training_x)
# my_pred_pruned_tree_TRAINING <- predict(fit.entire.pruned, newdata = training_x)
# 
# plot_ly(training, x = training_jan_aug, y = ~total_power, name = "actual", type = 'scatter', mode = 'lines+markers') %>%
#   add_trace(y = ~my_pred_initial_tree_TRAINING, name = 'predictions_initial_tree', mode = 'lines+markers') %>%
#   layout(title = 'Initial Decision Tree Training')
# 
# plot_ly(training, x = training_jan_aug, y = ~total_power, name = "actual", type = 'scatter', mode = 'lines+markers') %>%
#   add_trace(y = ~my_pred_entire_tree_TRAINING, name = 'predictions_entire_tree', mode = 'lines+markers') %>%
#   layout(title = 'Entire Decision Tree Training')
#   
# plot_ly(training, x = training_jan_aug, y = ~total_power, name = "actual", type = 'scatter', mode = 'lines+markers') %>%
#   add_trace(y = ~my_pred_pruned_tree_TRAINING, name = 'predictions_pruned_tree', mode = 'lines+markers') %>%
#   layout(title = 'Pruned Decision Tree Training')
#   
# my_pred_initial_tree_TEST <- predict(fit, newdata = test_x)
# my_pred_entire_tree_TEST <- predict(fit.entire, newdata = test_x)
# my_pred_pruned_tree_TEST <- predict(fit.entire.pruned, newdata = test_x)
# 
# plot_ly(test, x = test_jan_aug, y = ~total_power, name = "actual", type = 'scatter', mode = 'lines+markers') %>%
#   add_trace(y = ~my_pred_initial_tree_TEST, name = 'predictions_initial_tree', mode = 'lines+markers') %>%
#   layout(title = 'Initial Decision Tree Test')
#   
# plot_ly(test, x = test_jan_aug, y = ~total_power, name = "actual", type = 'scatter', mode = 'lines+markers') %>%
#   add_trace(y = ~my_pred_entire_tree_TEST, name = 'predictions_entire_tree', mode = 'lines+markers') %>%
#   layout(title = 'Entire Decision Tree Test')
#   
# plot_ly(test, x = test_jan_aug, y = ~total_power, name = "actual", type = 'scatter', mode = 'lines+markers') %>%
#   add_trace(y = ~my_pred_pruned_tree_TEST, name = 'predictions_pruned_tree', mode = 'lines+markers') %>%
#   layout(title = 'Pruned Decision Tree Test')

# Random Forest ----
## Default ----
# fit.ranger <- ranger(total_power ~ ., data = training) # default parameters

# results on training set
# plot_ly(training, x = training_jan_aug, y = ~total_power, name = "actual", type = 'scatter', mode = 'lines+markers') %>%
#   add_trace(y = ~fit.ranger$predictions, name = 'predictions_rf_training', mode = 'lines+markers') %>%
#   layout(title = 'Random Forest Training')

# results on test set
# my_pred_rf <- predict(fit.ranger, data = test_x)
# plot_ly(test, x = test_jan_aug, y = ~total_power, name = "actual", type = 'scatter', mode = 'lines+markers') %>%
#   add_trace(y = ~my_pred_rf$predictions, name = 'predictions_rf_test', mode = 'lines+markers') %>%
#   layout(title = 'Random Forest Test')


## Tuned Version ----
# r.forest <- train(total_power ~ ., 
#                   data = training, 
#                   method = "ranger")

# results on training set
# pred_random_forest_training <- predict(r.forest, newdata = training_x)
# plot_ly(training, x = training_jan_aug, y = ~total_power, name = "actual", type = 'scatter', mode = 'lines+markers') %>%
#   add_trace(y = pred_random_forest_training, name = 'predictions_rf_training', mode = 'lines+markers') %>%
#   layout(title = 'Tuned Random Forest Training')
# 
# pred_random_forest_test <- predict(r.forest, newdata = test_x)
# plot_ly(test, x = test_jan_aug, y = ~total_power, name = "actual", type = 'scatter', mode = 'lines+markers') %>%
#   add_trace(y = pred_random_forest_test, name = 'predictions_rf_training', mode = 'lines+markers') %>%
#   layout(title = 'Tuned Random Forest Test')


## Final Version ----

set.seed(1234)

# check feature importance
library(ranger)
wallboxes_jan_aug_DT[, random := runif(nrow(wallboxes_jan_aug_DT), 1, 500)]
fit.ranger <- ranger(total_power ~ . - Date, data = wallboxes_jan_aug_DT, importance = "permutation")
imp <- importance(fit.ranger)
imp <- data.table(Feature = names(imp), importance = imp)
plot_ly(data=imp, x=~Feature, y=~importance, type="bar") %>%
  layout(xaxis = list(categoryorder="total descending"))
wallboxes_jan_aug_DT[, random := NULL]

# predict next day using last 14 days
MAPE <- function(predicted, actual){
  mape <- mean(abs((actual - predicted)/actual))*100
  return (mape)
}

actuals <- wallboxes_jan_aug_DT
start_date <- as.Date("2022-01-08")
end_date <- as.Date("2022-01-22")
predicted_date <- list() # the date I tried to predict will get stored in this list
predictions <- list() # the predictions will get stored in this list
real_values <- list() # the actual values will get stored in this list
errors <- list() # the MAE for each model will get stored in this list
mape_errors <- list() # the MAPE for each model will get stored in this list

i <- 1
while (end_date <= as.Date("2022-01-23")) {
  
  print(paste('Currently calculating prediction for', end_date, '| Start Date:', start_date))
  training <- actuals[Date >= start_date & Date < end_date,]
  print(nrow(training))
  print(training)
  test <- actuals[actuals$Date == end_date]
  
  rf_model <- train(total_power ~ . - Date, 
                    data = training, 
                    method = "ranger")
  
  pred <- predict(rf_model, newdata = test) # predict 1 day
  predictions[[i]] <- pred # store the prediction
  predicted_date[[i]] <- end_date
  actual <- test$total_power
  real_values[[i]] <- actual
  errors[[i]] <- abs(actual - pred) # store the difference between actual and predicted value
  mape_errors[[i]] <- MAPE(pred, actual)
  
  i <- i+1
  
  start_date <- start_date + 1
  end_date <- end_date + 1
}

mean_absolute_error <- mean(unlist(errors))  # calculate MAE (Mean Absolute Error)
mean_absolute_error

mean_absolute_percentage_error <- mean(unlist(mape_errors))
mean_absolute_percentage_error

# lines+markers
plot_ly() %>%
  add_trace(x = ~predicted_date, y = ~real_values, name = 'Actuals', type = 'scatter', mode = 'lines+markers') %>%
  add_trace(x = ~predicted_date, y = ~predictions, name = 'Predictions', type = 'scatter', mode = 'lines+markers') %>%
  layout(title = 'RF predictions using the last 14 days', xaxis = list(title="Date"), yaxis =list(title="kWh"))

# only lines
plot_ly() %>%
  add_trace(x = ~predicted_date, y = ~real_values, name = 'Actuals', type = 'scatter', mode = 'lines') %>%
  add_trace(x = ~predicted_date, y = ~predictions, name = 'Predictions', type = 'scatter', mode = 'lines') %>%
  layout(title = 'RF predictions using the last 14 days', xaxis = list(title="Date"), yaxis =list(title="kWh"))

# both visualizations do the same
# results <- actuals[actuals$Date >= as.Date("2022-01-22"),]
# results$pred <- unlist(predictions)
# 
# result_plot <- plot_ly(results, x = ~Date, y = ~total_power, name="Actuals", type="scatter", mode="line") %>%
#   add_trace(y = ~pred, type="scatter", name="Forecast") %>%
#   layout(title = 'RF predictions using the last 14 days', xaxis = list(title="Date"), yaxis =list(title="kWh"))
# result_plot

# Use rest of data ----
# Here I am using the rest of the data we haven't even looked at so far. Let's see how the model performs

# wallboxes_oct_2022_feb_2023_predictions <- predict(lin_regr, wallboxes_oct_2022_feb_2023_DT)
# plot_ly(wallboxes_oct_2022_feb_2023_DT, x = oct_feb, y = ~total_power, name = "actual", type = 'scatter', mode = 'lines+markers') %>%
#   add_trace(y = ~wallboxes_oct_2022_feb_2023_predictions, name = 'predictions', mode = 'lines+markers') 
# 
# wallboxes_oct_2022_feb_2023_initial_tree <- predict(fit, newdata = wallboxes_oct_2022_feb_2023_DT)
# plot_ly(wallboxes_oct_2022_feb_2023_DT, x = oct_feb, y = ~total_power, name = "actual", type = 'scatter', mode = 'lines+markers') %>%
#   add_trace(y = ~wallboxes_oct_2022_feb_2023_initial_tree, name = 'predictions_initial_tree', mode = 'lines+markers') 
# 
# wallboxes_oct_2022_feb_2023_entire_tree <- predict(fit.entire, newdata = wallboxes_oct_2022_feb_2023_DT)
# plot_ly(wallboxes_oct_2022_feb_2023_DT, x = oct_feb, y = ~total_power, name = "actual", type = 'scatter', mode = 'lines+markers') %>%
#   add_trace(y = ~wallboxes_oct_2022_feb_2023_entire_tree, name = 'predictions_entire_tree', mode = 'lines+markers') 
# 
# wallboxes_oct_2022_feb_2023_pruned_tree <- predict(fit.entire.pruned, newdata = wallboxes_oct_2022_feb_2023_DT)
# plot_ly(wallboxes_oct_2022_feb_2023_DT, x = oct_feb, y = ~total_power, name = "actual", type = 'scatter', mode = 'lines+markers') %>%
#   add_trace(y = ~wallboxes_oct_2022_feb_2023_pruned_tree, name = 'predictions_entire_tree', mode = 'lines+markers') 
# 
# wallboxes_oct_2022_feb_2023_rf <- predict(fit.ranger, data = wallboxes_oct_2022_feb_2023_DT)
# plot_ly(wallboxes_oct_2022_feb_2023_DT, x = oct_feb, y = ~total_power, name = "actual", type = 'scatter', mode = 'lines+markers') %>%
#   add_trace(y = ~wallboxes_oct_2022_feb_2023_rf$predictions, name = 'predictions_rf', mode = 'lines+markers') 

