# PREPARATION ----
# Convert to data.table
wallboxes_jan_aug
wallboxes_jan_aug_DT <- setDT(wallboxes_jan_aug)

## Enrichment ----
# add more columns that could be used for predicting the total power needed

# total_power used on previous day
wallboxes_jan_aug_DT[, total_power_previous_day := shift(total_power, 1)]

# total_power used on the same day last week
wallboxes_jan_aug_DT[, total_power_same_day_previous_week := shift(total_power, 7)]

# average of total_power
wallboxes_jan_aug_DT[, total_power_avg := mean(total_power)]

# total_power used last week
wallboxes_jan_aug_DT[, week := isoweek(Date)]
sum_each_week <- wallboxes_jan_aug_DT[, sum(total_power), week] # select sum(total_power) where ALL ROWS group by week
wallboxes_jan_aug_DT <- merge(wallboxes_jan_aug_DT, sum_each_week, by = "week", all.x = TRUE)
setnames(wallboxes_jan_aug_DT,
         old = c("V1"),
         new = c("total_power_last_week"))

## Remove NA values ----
# remove rows containing NA values

colSums(is.na(wallboxes_jan_aug_DT))
wallboxes_jan_aug_DT <- na.omit(wallboxes_jan_aug_DT)
wallboxes_jan_aug_DT
str(wallboxes_jan_aug_DT)

## Check importance of regressors ----
linear_regression <- lm(total_power ~ ., data = wallboxes_jan_aug_DT)
summary(linear_regression) # total_power_same_day_previous_week and total_power_avg seem to be useless

## Remove unnecessary regressors
wallboxes_jan_aug_DT[, week := NULL]
wallboxes_jan_aug_DT[, Date := NULL]

# Split data ----
set.seed(1) # used to make results reproducible
index <- createDataPartition(wallboxes_jan_aug_DT$total_power, p = 0.8, list = F)
training <- wallboxes_jan_aug_DT[index]
test <- wallboxes_jan_aug_DT[-index]

# Linear Regression ----

linear_regression <- lm(total_power ~ ., data = training) # predict "total_power" using all other variables from training set
summary(linear_regression)

training_predictions <- lin_regr$fitted.values
training_actuals <- training[, total_power]

test_predictions <- predict(linear_regression, test)
test_actuals <- test[, total_power]

## Linear Regression Metrics ----
MAPE <- function(predicted, actual){
  mape <- mean(abs((actual - predicted)/actual))*100
  return (mape)
}

# Training data
MAE(training_predictions, training_actuals) # MAE on training data for linear regression
RMSE(training_predictions, training_actuals) # RMSE on training data for linear regression
MAPE(training_predictions, training_actuals) # MAPE on training data for linear regression (in percent)

# Test data
MAE(test_predictions, test_actuals)
RMSE(test_predictions, test_actuals)
MAPE(test_predictions, test_actuals)

# -> (surprisingly) performs a little bit better on test data than on training data, but performs is very bad

# Polynomial Regression ----
poly_regr <- list()
MAEs_training <- vector()
RMSEs_training <- vector()
MAPEs_training <- vector()
MAEs_test <- vector()
RMSEs_test <- vector()
MAPEs_test <- vector()
for (d in 1:6) {
  poly_regr[[d]] <- lm(total_power ~ poly(total_power_previous_day, total_power_same_day_previous_week, total_power_last_week, degree = d), data = training)
  
  training_predictions <- poly_regr[[d]]$fitted.values
  training_actuals <- training[, total_power]
  MAEs_training[d] <- MAE(training_predictions, training_actuals)
  RMSEs_training[d] <- RMSE(training_predictions, training_actuals)
  MAPEs_training[d] <- MAPE(training_predictions, training_actuals)
  
  test_predictions <- predict(poly_regr[[d]], test)
  test_actuals <- test[, total_power]
  MAEs_test[d] <- MAE(test_predictions, test_actuals)
  RMSEs_test[d] <- RMSE(test_predictions, test_actuals)
  MAPEs_test[d] <- MAPE(test_predictions, test_actuals)
}

plot_ly(x = 1:length(MAEs_training),y = MAEs_training, type = "scatter", mode = "line") %>%
  add_lines(x = 1:length(MAEs_test), y = MAEs_test)

# Looking at the graph I would choose a degree of 4.

## Polynomial Regression Metrics ----
for (x in 1:6) {
  print(paste("Degree:", x))
  
  print("Training Results")
  print(paste("MAE:", MAEs_training[x]))
  print(paste("RMSE:", RMSEs_training[x]))
  print(paste("MAPE:", MAPEs_training[x]))
  
  print("Test Results")
  print(paste("MAE:", MAEs_test[x]))
  print(paste("RMSE:", RMSEs_test[x]))
  print(paste("MAPE:", MAPEs_test[x]))
  
  print("-------------------------------")
  
}

# Looking at the actual numbers the model already starts to over fit a tiny bit at a degree of 3.

# Random Forest Regression ----
library(ranger)
fit.ranger <- ranger(total_power ~ ., data = training)
summary(fit.ranger)

























