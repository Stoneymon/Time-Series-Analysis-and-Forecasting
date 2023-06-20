# SETUP ----
## Working Directory ----
if (rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()[["path"]]))
}

## Load Libraries ----
library(data.table)
library(plotly)
library(caret)
library(zoo)
library(ranger)
library(rpart)
library(rpart.plot)  

## Used Functions ----
MAPE <- function(predicted, actual){
  mape <- mean(abs((actual - predicted) / actual)) * 100
  return (mape)
}


# DATA PROCESSING ----
## Import Wallbox Data ----
wallboxes_jan_aug_DT <- fread("./data/preprocessed/total_power_jan-aug.csv")

# remove unnecessary battery_SOC feature for this
wallboxes_jan_aug_DT[, battery_SOC := NULL]

## Feature Engineering ----
# add more columns that could be used for predicting the total power needed

# 1. total_power used on previous day
wallboxes_jan_aug_DT[, total_power_previous_day := shift(total_power, 1)]

# 2. total_power used on the same day last week
wallboxes_jan_aug_DT[, total_power_same_day_previous_week := shift(total_power, 7)]

# 3. average total_power that was used in the last 7 days (rolling average)
wallboxes_jan_aug_DT <- wallboxes_jan_aug_DT %>%
  mutate(average_total_power_last_seven_days = rollmean(total_power, k = 7, fill = NA, align = "right"))

# I found out that this calculation sums the last X values and divides it by X and then writes it to row X, this doesn't
# make sense because when we want to predict the total_power that will be used on a day then we of course don't know the
# total_power of this day already. Therefore all of the solutions have to be shifted by 1. So that for example the rolling average
# of the first 7 days is written in row 8. Meaning we can use this knowledge to predict total_power on day 8.
wallboxes_jan_aug_DT[, average_total_power_last_seven_days := shift(average_total_power_last_seven_days, 1)]

## Remove NA values ----
# remove rows containing NA values -> because we shifted some values
colSums(is.na(wallboxes_jan_aug_DT))
wallboxes_jan_aug_DT <- na.omit(wallboxes_jan_aug_DT)

# also remove the date column just to be safe so it doesn't interfere with the 
# models but store it so we can use it later for plotting the results
jan_aug <- wallboxes_jan_aug_DT$Date  # starts with "2022-01-08"
wallboxes_jan_aug_DT[, Date := NULL]

str(wallboxes_jan_aug_DT)
summary(wallboxes_jan_aug_DT)
head(wallboxes_jan_aug_DT, n = 10)


# DATA SPLITTING ----
# This was actually the mistake in the first try because we tried to split the 
# data like we typically did in the ML class. But for time series data it is
# better to only use a small amount (like 14 days) for training a model and then
# trying to predict the next day for example.

set.seed(123) # used to make results reproducible
data <- wallboxes_jan_aug_DT
idx <- createDataPartition(y = data[, total_power], p = 0.8,
                           list = F, times = 1)

training <- data[idx]
training_dates <- jan_aug[idx]

test <- data[-idx]
test_dates <- jan_aug[-idx]

# -> Rescaling is not necessary because all values are on the same scale


# LINEAR REGRESSION ----
lin_regr <- lm(total_power ~ ., data = training)
summary(lin_regr)

# -> When looking at the feature importance we see that sadly no feature seems 
# to be really good for predicting the target variable "total_power".

training_predictions <- predict(lin_regr, training)
test_predictions <- predict(lin_regr, test)

## Plots ----
plot_ly(x = training_dates, y = training$total_power, name = "actuals", 
        type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = training_predictions, name = 'predictions',
            mode = 'lines+markers') %>%
  layout(title = 'Linear Regression - TRAINING')

# -> When looking at the plot we see that the model is not capable of producing
# a good result on the training data.

plot_ly(x = test_dates, y = test$total_power, name = "actuals", 
        type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = test_predictions, name = 'predictions',
            mode = 'lines+markers') %>%
  layout(title = 'Linear Regression - TEST')

# -> So not really surprisingly also the performance on the test data
# is far from good.

## MAE | MAPE ----
# Training data
MAE(training_predictions, training$total_power)
MAPE(training_predictions, training$total_power) # in percent

# Test data
MAE(test_predictions, test$total_power)
MAPE(test_predictions, test$total_power)

result1 <- data.frame("Model" = "Linear Model",
                 "MAE_Training" = MAE(training_predictions, training$total_power),
                 "MAPE_Training" = MAPE(training_predictions, training$total_power),
                 "MAE_Test" = MAE(test_predictions, test$total_power),
                 "MAPE_Test" = MAPE(test_predictions, test$total_power))

# DECISION TREE ----
## Initial Tree ----
initial_tree <- rpart::rpart(total_power ~ ., data = training)

# Can use those statements below to get more information about the tree
# print(fit)
# printcp(fit)
# plotcp(fit)
# summary(fit)

training_predictions_initial_tree <- predict(initial_tree, training)
test_predictions_initial_tree <- predict(initial_tree, test)

### Plots ----

rpart.plot(initial_tree, type = 2, extra = 101, fallen.leaves = F,
           main = "Initial Tree", tweak = 1.2)

plot_ly(x = training_dates, y = training$total_power, name = "actuals", 
        type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = training_predictions_initial_tree, name = 'predictions',
            mode = 'lines+markers') %>%
  layout(title = 'Initial Tree - TRAINING')

plot_ly(x = test_dates, y = test$total_power, name = "actuals", 
        type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = test_predictions_initial_tree, name = 'predictions',
            mode = 'lines+markers') %>%
  layout(title = 'Initial Tree - TEST')

### MAE | MAPE ----
# Training data
MAE(training_predictions_initial_tree, training$total_power)
MAPE(training_predictions_initial_tree, training$total_power)

# -> On the training set the initial decision tree performs better than the
# linear model

# Test data
MAE(test_predictions_initial_tree, test$total_power)
MAPE(test_predictions_initial_tree, test$total_power)

# -> But it seems like the model overfits because the result is worse than when
# using the linear model

result2 <- data.frame("Model" = "Initial Decision Tree",
                      "MAE_Training" = MAE(training_predictions_initial_tree, training$total_power),
                      "MAPE_Training" = MAPE(training_predictions_initial_tree, training$total_power),
                      "MAE_Test" = MAE(test_predictions_initial_tree, test$total_power),
                      "MAPE_Test" = MAPE(test_predictions_initial_tree, test$total_power))

## Entire Tree ----
entire_tree <- rpart::rpart(total_power ~ ., data = training,
                           control = rpart.control(minsplit = 1, minbucket = 1,
                                                   cp = 0))

training_predictions_entire_tree <- predict(entire_tree, training)
test_predictions_entire_tree <- predict(entire_tree, test)

### Plots ----
rpart.plot(entire_tree, type = 2, extra = 101, fallen.leaves = F,
           main = "Entire Regression Tree", tweak = 1.2)

plot_ly(x = training_dates, y = training$total_power, name = "actuals", 
        type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = training_predictions_entire_tree, name = 'predictions',
            mode = 'lines+markers') %>%
  layout(title = 'Entire Tree - TRAINING')

plot_ly(x = test_dates, y = test$total_power, name = "actuals", 
        type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = test_predictions_entire_tree, name = 'predictions',
            mode = 'lines+markers') %>%
  layout(title = 'Entire Tree - TEST')

### MAE | MAPE ----
# Training data
MAE(training_predictions_entire_tree, training$total_power)
MAPE(training_predictions_entire_tree, training$total_power)

# -> As expected the entire decision tree fits the training data perfectly

# Test data
MAE(test_predictions_entire_tree, test$total_power)
MAPE(test_predictions_entire_tree, test$total_power)

# -> But because the model overfits drastically, the performance on the unseen
# test data is really bad

result3 <- data.frame("Model" = "Entire Decision Tree",
                      "MAE_Training" = MAE(training_predictions_entire_tree, training$total_power),
                      "MAPE_Training" = MAPE(training_predictions_entire_tree, training$total_power),
                      "MAE_Test" = MAE(test_predictions_entire_tree, test$total_power),
                      "MAPE_Test" = MAPE(test_predictions_entire_tree, test$total_power))

## Pruned Tree ----
# Prune entire tree at optimal level of cp
best_cp_for_pruning <- entire_tree$cptable[which.min(entire_tree$cptable[, "xerror"]), "CP"]
pruned_tree <- prune(entire_tree, cp = best_cp_for_pruning)

training_predictions_pruned_tree <- predict(pruned_tree, training)
test_predictions_pruned_tree <- predict(pruned_tree, test)

### Plots ----
rpart.plot(pruned_tree, type = 2, extra = 101, fallen.leaves = F,
           main = "Pruned Regression Tree", tweak = 1.2)

# -> weirdly we see that the optimal cp was found when performing only a
# a single split

plot_ly(x = training_dates, y = training$total_power, name = "actuals", 
        type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = training_predictions_pruned_tree, name = 'predictions',
            mode = 'lines+markers') %>%
  layout(title = 'Pruned Tree - TRAINING')

# -> This split is not able to fit the training data

plot_ly(x = test_dates, y = test$total_power, name = "actuals", 
        type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = test_predictions_pruned_tree, name = 'predictions',
            mode = 'lines+markers') %>%
  layout(title = 'Pruned Tree - TEST')

### MAE | MAPE ----
# Training data
MAE(training_predictions_pruned_tree, training$total_power)
MAPE(training_predictions_pruned_tree, training$total_power)

# Test data
MAE(test_predictions_pruned_tree, test$total_power)
MAPE(test_predictions_pruned_tree, test$total_power)

# -> Still by only splitting like this the performance on the test set is the
# best from all the decision trees. Still worse than the linear model.

result4 <- data.frame("Model" = "Pruned Decision Tree",
                      "MAE_Training" = MAE(training_predictions_pruned_tree, training$total_power),
                      "MAPE_Training" = MAPE(training_predictions_pruned_tree, training$total_power),
                      "MAE_Test" = MAE(test_predictions_pruned_tree, test$total_power),
                      "MAPE_Test" = MAPE(test_predictions_pruned_tree, test$total_power))


# RANDOM FOREST ----
## Feature Importance ----
# Let's create a random feature
# We expect that other features are much more important than the random one
training[, random := runif(nrow(training), 1, 100)]
random_forest <- ranger(total_power ~ ., data = training, 
                        importance = "permutation")

imp <- importance(random_forest)
imp <- data.table(Feature = names(imp), importance = imp)
plot_ly(data=imp, x=~Feature, y=~importance, type="bar") %>%
  layout(xaxis = list(categoryorder="total descending"))

# -> No feature seems to be less important than the random one we just created.
# But it seems like "total_power_same_day_previous_week" has a negative impact
# on the result?
training[, random := NULL]

## Default RF ----
random_forest <- ranger(total_power ~ ., data = training) # default parameters

training_predictions_random_forest <- predict(random_forest, training)
test_predictions_random_forest <- predict(random_forest, test)

### Plots ----
plot_ly(x = training_dates, y = training$total_power, name = "actuals", 
        type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = training_predictions_random_forest$predictions, name = 'predictions',
            mode = 'lines+markers') %>%
  layout(title = 'Random Forest - TRAINING')

plot_ly(x = test_dates, y = test$total_power, name = "actuals", 
        type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = test_predictions_random_forest$predictions, name = 'predictions',
            mode = 'lines+markers') %>%
  layout(title = 'Random Forest - TEST')

### MAE | MAPE ----
# Training data
MAE(training_predictions_random_forest$predictions, training$total_power)
MAPE(training_predictions_random_forest$predictions, training$total_power)

# Test data
MAE(test_predictions_random_forest$predictions, test$total_power)
MAPE(test_predictions_random_forest$predictions, test$total_power)

result5 <- data.frame("Model" = "Default Random Forest",
                      "MAE_Training" = MAE(training_predictions_random_forest$predictions, training$total_power),
                      "MAPE_Training" = MAPE(training_predictions_random_forest$predictions, training$total_power),
                      "MAE_Test" = MAE(test_predictions_random_forest$predictions, test$total_power),
                      "MAPE_Test" = MAPE(test_predictions_random_forest$predictions, test$total_power))


## Tuned Version ----
random_forest_tuned <- train(total_power ~ .,
                             data = training,
                             method = "ranger")

training_predictions_random_forest_tuned <- predict(random_forest_tuned,
                                                    training)
test_predictions_random_forest_tuned <- predict(random_forest_tuned, test)

### Plots ----
plot_ly(x = training_dates, y = training$total_power, name = "actuals", 
        type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = training_predictions_random_forest_tuned, name = 'predictions',
            mode = 'lines+markers') %>%
  layout(title = 'Random Forest Tuned - TRAINING')

plot_ly(x = test_dates, y = test$total_power, name = "actuals", 
        type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = test_predictions_random_forest_tuned, name = 'predictions',
            mode = 'lines+markers') %>%
  layout(title = 'Random Forest Tuned - TEST')

### MAE | MAPE ----
# Training data
MAE(training_predictions_random_forest_tuned, training$total_power)
MAPE(training_predictions_random_forest_tuned, training$total_power)

# Test data
MAE(test_predictions_random_forest_tuned, test$total_power)
MAPE(test_predictions_random_forest_tuned, test$total_power)

result6 <- data.frame("Model" = "Tuned Random Forest",
                      "MAE_Training" = MAE(training_predictions_random_forest_tuned, training$total_power),
                      "MAPE_Training" = MAPE(training_predictions_random_forest_tuned, training$total_power),
                      "MAE_Test" = MAE(test_predictions_random_forest_tuned, test$total_power),
                      "MAPE_Test" = MAPE(test_predictions_random_forest_tuned, test$total_power))

# COMPARISON ----

list(result1, result2, result3, result4, result5, result6)

# When looking at all the models and their results we see the following:

# - the best MAE_Test was achieved by the Pruned Decision Tree, followed by
# the Linear Model and then followed by the Tuned Random Forest

# - the best MAPE_Test was achieved by the Tuned Random Forest, followed by
# the Default Random Forest and then followed by the Linear Model

# -> That's why we decided to focus on Random Forest from now on.
