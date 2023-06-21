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

# 4. weekend or not
wallboxes_jan_aug_DT[, is_weekend := weekdays(Date)]
wallboxes_jan_aug_DT$is_weekend <- ifelse(wallboxes_jan_aug_DT$is_weekend %in% c("Samstag", "Sonntag"), 1, 0)
wallboxes_jan_aug_DT[, is_weekend := as.factor(is_weekend)]
wallboxes_jan_aug_DT[, is_weekend := as.factor(ifelse(is_weekend == "1", "Yes", "No"))]

## Remove NA values ----
# remove rows containing NA values -> because we shifted some values
colSums(is.na(wallboxes_jan_aug_DT))
wallboxes_jan_aug_DT <- na.omit(wallboxes_jan_aug_DT)

str(wallboxes_jan_aug_DT)
summary(wallboxes_jan_aug_DT)
head(wallboxes_jan_aug_DT, n = 10)


# RANDOM FOREST ----
set.seed(123)

## Feature Importance ----
# Let's create a random feature
# We expect that other features are much more important than the random one
wallboxes_jan_aug_DT[, random := runif(nrow(wallboxes_jan_aug_DT), 1, 100)]
random_forest <- ranger(total_power ~ . - Date, data = wallboxes_jan_aug_DT, 
                        importance = "permutation")  # do not use Date

imp <- importance(random_forest)
imp <- data.table(Feature = names(imp), importance = imp)
plot_ly(data=imp, x=~Feature, y=~importance, type="bar") %>%
  layout(xaxis = list(categoryorder="total descending"))

# -> Weirdly the random feature seems to be more important than the feature
# "total_power_same_day_previous_week"; Additionally, also "is_weekend" seems
# to have a negative influence on the result...
wallboxes_jan_aug_DT[, random := NULL]

## Train Tuned Random Forests ----
data <- wallboxes_jan_aug_DT
start_date <- as.Date("2022-01-08")
end_date <- as.Date("2022-01-22")
predicted_date <- list() # date we tried to predict will get stored in this list
predictions <- list() # predicted values will get stored in this list
actuals <- list() # actual values will get stored in this list
MAEs <- list() # MAE for each model will get stored in this list
MAPEs <- list() # MAPE for each model will get stored in this list
feature_importance <- list() # feature importance of each trained model 
                            # will get stored in this list

i <- 1
while (end_date <= as.Date("2022-08-21")) {  # highest date: 2022-08-21
  
  print(paste('Start Date:', start_date, '| Predicting:', end_date))
  
  training <- data[Date >= start_date & Date < end_date,]
  test <- data[data$Date == end_date]
  
  rf_model <- train(total_power ~ . - Date, 
                    data = training, 
                    method = "ranger",
                    importance = "permutation")
  feature_importance[[i]] <- varImp(rf_model)  # different feature importance
                                              # than the one that was used above
                                              # because we use caret here to 
                                              # train the model
  
  pred <- predict(rf_model, newdata = test) # predict next day
  
  predictions[[i]] <- pred
  predicted_date[[i]] <- end_date
  actual <- test$total_power
  actuals[[i]] <- actual
  MAEs[[i]] <- MAE(pred, actual)
  MAPEs[[i]] <- MAPE(pred, actual)
  
  i <- i + 1
  
  start_date <- start_date + 1
  end_date <- end_date + 1
}

## Aggregated Feature Importance ----
aggregated_feature_importance <- data.table(Feature = rownames(feature_importance[[1]]$importance),
                                            importance = c(0, 0, 0, 0))
y <- 1
for (x in feature_importance) {
  imp <- data.table(Feature = rownames(x$importance),
                    importance = x$importance$Overall)
  aggregated_feature_importance <- rbindlist(list(aggregated_feature_importance,
                                                  imp))
  
  y <- y + 1
}

colSums(is.na(aggregated_feature_importance))
aggregated_feature_importance <- na.omit(aggregated_feature_importance)
divisor <- nrow(aggregated_feature_importance) / 4

aggregated_feature_importance <- aggregated_feature_importance[, sum(importance) / divisor, Feature]
plot_ly(data=aggregated_feature_importance, x=~Feature, y=~V1, type="bar") %>%
  layout(xaxis = list(categoryorder="total descending"),
         yaxis = list(title="Feature Importance"))

# -> This plot shows the aggregated feature importance of all the trained models

## Plots ----
# lines+markers
plot_ly() %>%
  add_trace(x = ~predicted_date, y = ~actuals, name = 'Actuals',
            type = 'scatter', mode = 'lines+markers') %>%
  add_trace(x = ~predicted_date, y = ~predictions, name = 'Predictions',
            type = 'scatter', mode = 'lines+markers') %>%
  layout(title = 'RF predictions using the last 14 days',
         xaxis = list(title="Date"), yaxis =list(title="kWh"))

# only lines
plot_ly() %>%
  add_trace(x = ~predicted_date, y = ~actuals, name = 'Actuals',
            type = 'scatter', mode = 'lines') %>%
  add_trace(x = ~predicted_date, y = ~predictions, name = 'Predictions',
            type = 'scatter', mode = 'lines') %>%
  layout(title = 'RF predictions using the last 14 days',
         xaxis = list(title="Date"), yaxis =list(title="kWh"))

## Aggregated MAE | MAPE ----
mean_absolute_error <- mean(unlist(MAEs))  # calculate aggregated MAE
mean_absolute_error

mean_absolute_percentage_error <- mean(unlist(MAPEs))
mean_absolute_percentage_error

# -> The aggregated MAE/MAPE (on the test data) is better than any of the 
# performance measures we tried before like Linear Models, Decision Trees and
# Random Forests