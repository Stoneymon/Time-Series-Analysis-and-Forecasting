# PREPARATION ----
# Convert to data.table
wallboxes_jan_aug
wallboxes_jan_aug_DT <- setDT(wallboxes_jan_aug)

## Data Enrichment ----
# add more columns that could be used for predicting the total power needed

# total_power used on previous day
wallboxes_jan_aug_DT[, total_power_previous_day := shift(total_power, 1)]

# total_power used on the same day last week
wallboxes_jan_aug_DT[, total_power_same_day_previous_week := shift(total_power, 7)]

# total power used last 7 days
wallboxes_jan_aug_DT <- wallboxes_jan_aug_DT %>%
                          mutate(total_power_last_seven_days = rollmean(total_power, k=7, fill=NA, align='right'))
                      #  ,total_power_last_six_days = rollmean(total_power, k=4, fill=NA, align='right'))

## Remove NA values ----
# remove rows containing NA values
View(wallboxes_jan_aug_DT)
colSums(is.na(wallboxes_jan_aug_DT))
wallboxes_jan_aug_DT <- na.omit(wallboxes_jan_aug_DT)
View(wallboxes_jan_aug_DT)
str(wallboxes_jan_aug_DT)

## DT without Date and Week ----
wallboxes_jan_aug_DT[, Date := NULL]
data <- wallboxes_jan_aug_DT

## Check importance of regressors ----
lin_regr <- lm(total_power ~ ., data = data)
summary(lin_regr) 

# plotten forecast und actual

# SPLITTING ----
set.seed(1) # used to make results reproducible

idx <- createDataPartition(y = data[, total_power], p = 0.8, list = F, times = 1)

training <- data[idx]
training_x <- data[idx, !"total_power"]
training_y <- data[idx, total_power]

test <- data[!idx]
test_x <- data[!idx, !"total_power"]
test_y <- data[!idx, total_power]

## Feature Importance ----
# Let's create a random feature
# We expect that other features are much more important than the random one
library(ranger)
training[,random:=runif(nrow(training), 1, 100)]
fit.ranger <- ranger(total_power ~ ., data = training, importance = "permutation")

View(fit.ranger)
imp <- importance(fit.ranger)
imp <- data.table(Feature = names(imp), importance = imp)

plot_ly(data=imp, x=~Feature, y=~importance, type="bar") %>%
  layout(xaxis = list(categoryorder="total descending"))
# Confirmed. We can remove the random feature
training[, random :=NULL]
# If we would have seen that some features are much less important than
# others we could have dropped them as well.

# Linear Regression ----

lin_regr <- lm(total_power ~ ., data = training) # predict "total_power" using all other variables from training set
summary(lin_regr)

training_predictions <- lin_regr$fitted.values

test_predictions <- predict(lin_regr, test)

fig <- plot_ly(training, x = ~Date, y = ~total_power, name = "actual", type = 'scatter', mode = 'lines+markers')
fig <- fig %>% add_trace(y = ~training_predictions, name = 'predictions', mode = 'lines+markers') 
fig

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

# Decision Tree ----
library(rpart) # Decision trees
library(rpart.plot)  # plots for decision trees
fit <- rpart::rpart(total_power ~ ., data = training)
print(fit)
printcp(fit)  # cp: complexity parameter
plotcp(fit)
summary(fit)
rpart.plot(fit, type = 2, extra = 101, fallen.leaves = F, main = "Classification Tree for Banknotes", tweak=1.2)

fit.entire <- rpart::rpart(total_power ~ ., data = training,
                           control = rpart.control(minsplit = 1, cp = 0))
print(fit.entire)
printcp(fit.entire)
plotcp(fit.entire)
summary(fit.entire)
rpart.plot(fit.entire, type = 2, extra = 101, fallen.leaves = F, tweak = 1.2, main = "Entire tree for Banknotes")

# And now we prune it at the optimal level of CP
best_cp_for_pruning <- fit.entire$cptable[which.min(fit.entire$cptable[, "xerror"]), "CP"]
fit.entire.pruned <- prune(fit.entire, cp = best_cp_for_pruning)

# This is our final tree
fit.entire.pruned
printcp(fit.entire.pruned)
rpart.plot(fit.entire.pruned, type = 2, extra = 101, fallen.leaves = F, tweak = 1.2, main = "Pruned tree for Banknotes")
























