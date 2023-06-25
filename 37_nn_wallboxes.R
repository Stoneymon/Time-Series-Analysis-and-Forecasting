# I am following a book for using torch in R by Sigrid Keydana in order to implement a LSTM Model
# Source: https://skeydan.github.io/Deep-Learning-and-Scientific-Computing-with-R-torch/time_series.html
# Some parts of template code are also from this source 


# Loading necessary Libraries

library(torch)
library(dplyr)
library(luz)
library(tidyr)
library(tibble)
library(ggplot2)
library(lubridate)
library(tsibble)
library(feasts)
library(tsibbledata)
library(zoo)
library(Metrics)
library(readxl)
library(plotly)



# Available zoom - minute, hour, day
# Here if you set the zoom to day. The input sequence for the neural network will be 14 days and 
# the output sequence will be the next day. As for hour it will be 14 days in hours to predict the next hour.

# The reason for these zooms is that I realised by increasing the amount of data the lstm model trains on, the forecasts
# improve considerably.

 # The minute zoom is a bit of an exception. I was limited by how long it took to train the model. Hence I had to reduce the 
# data it is trained  as well as the input sequence lenth (12 hours). This is not to be compared with the other models
# but rather just to observe how well it forecasts. 



zoom = "day"

if (zoom == "minute") {
  wallboxes <-  readxl::read_excel(path="./data/preprocessed/total_power_jan-aug_minute.xlsx")
  wallboxes <- rename(wallboxes,total_power = total.P)
  n_timesteps <- 60 * 12
  
  
} else if (zoom == "hour") {
  wallboxes <-  readxl::read_excel(path="./data/preprocessed/total_power_jan-aug_hour.xlsx")
  
  n_timesteps <- 24 * 7 * 2
  
  
} else if (zoom == "day") {
  wallboxes <-  readxl::read_excel(path="./data/preprocessed/total_power_jan-aug.xlsx")
  wallboxes <- select(wallboxes,Date,total_power)
  wallboxes <- data.table::as.data.table(wallboxes)
  wallboxes$Date <- as.Date(wallboxes$Date)
  
  n_timesteps <- 14
  

  
} else{
  print("Invalid zoom")
}


wallboxes <- as_tsibble(wallboxes, index = Date)

wallboxes <- wallboxes %>%
  as_tsibble(index = Date) %>%
  # Set NA to any missing time steps 
  fill_gaps()

# This filter is needed for the minute zoom. Fill_gap creates NA values for every missing second.
wallboxes <- wallboxes %>%
  filter(second(Date) == 0)

# There are not many missing time steps. Hence I am filling the missing ones with approximations
wallboxes <- wallboxes %>%
  mutate(total_power = na.approx(total_power, na.rm = FALSE))


# Training
power_train <- wallboxes %>%
  mutate(Year = year(Date), Month = month(Date), Day = day(Date)) %>%
  filter(Year == 2022 & Month >= 1 & Month <= 5) %>%
  pull(total_power) %>%
  as.matrix()


# Validation - This will be used to implement early stoppings to reduce the time it takes to train the model
power_validation <- wallboxes %>%
  mutate(Year = year(Date), Month = month(Date), Day = day(Date)) %>%
  filter((Year == 2022 & Month == 6) | (Year == 2022 & Month == 7 & Day <= 14)) %>%
  pull(total_power) %>%
  as.matrix()

# Test
power_test <- wallboxes %>%
  mutate(Year = year(Date), Month = month(Date), Day = day(Date)) %>%
  filter(Year == 2022 & ((Month == 7 & Day > 14) | Month > 7)) %>%
  pull(total_power) %>%
  as.matrix()

length(power_train)
length(power_validation)
length(power_test)

train_mean <- mean(power_train)
train_sd <- sd(power_train)

# Creating the dataset, which is needed for the model. Here I am following closely the book
demand_dataset <- dataset(
  name = "demand_dataset",
  initialize = function(x, n_timesteps, sample_frac = 1) {
    self$n_timesteps <- n_timesteps
    self$x <- torch_tensor((x - train_mean) / train_sd)
    
    n <- length(self$x) - self$n_timesteps
    
    self$starts <- sort(sample.int(
      n = n,
      size = n * sample_frac
    ))
  },
  .getitem = function(i) {
    start <- self$starts[i]
    end <- start + self$n_timesteps - 1
    
    list(
      x = self$x[start:end],
      y = self$x[end + 1]
    )
  },
  .length = function() {
    length(self$starts)
  }
)

train_ds <- demand_dataset(power_train, n_timesteps)
valid_ds <- demand_dataset(power_validation, n_timesteps)
test_ds <- demand_dataset(power_test, n_timesteps)
dim(train_ds[1]$x)
dim(train_ds[1]$y)

batch_size <- 128

train_dl <- train_ds %>%
  dataloader(batch_size = batch_size, shuffle = TRUE)
valid_dl <- valid_ds %>%
  dataloader(batch_size = batch_size)
test_dl <- test_ds %>%
  dataloader(batch_size = length(test_ds))


b <- train_dl %>%
  dataloader_make_iter() %>%
  dataloader_next()

dim(b$x)
dim(b$y)


# By setting dropout to 0.2, I am trying to make sure that the model generalizes well on unseen data and does not overfit
model <- nn_module(
  initialize = function(input_size,
                        hidden_size,
                        dropout = 0.2,
                        num_layers = 1,
                        rec_dropout = 0.1) {
    self$num_layers <- num_layers
    
    self$rnn <- nn_lstm(
      input_size = input_size,
      hidden_size = hidden_size,
      num_layers = num_layers,
      dropout = rec_dropout,
      batch_first = TRUE
    )

    
    self$dropout <- nn_dropout(dropout)
    self$output <- nn_linear(hidden_size, 1)
  },
  forward = function(x) {
    (x %>%
       self$rnn())[[1]][, dim(x)[2], ] %>%
      self$dropout() %>%
      self$output()
  }
)


input_size <- 1
hidden_size <- 32
num_layers <- 2
rec_dropout <- 0.2


# Creating the model
model <- model %>%
  setup(optimizer = optim_adam, loss = nn_mse_loss()) %>%
  set_hparams(
    input_size = input_size,
    hidden_size = hidden_size,
    num_layers = num_layers,
    rec_dropout = rec_dropout
  )

# This way I can use an optimal learning rate. This has a huge effect on how long it takes to train the model

rates_and_losses <- model %>%
  lr_finder(train_dl, start_lr = 1e-3, end_lr = 1)
rates_and_losses %>% plot()

# Training the neural network. Here I implement the early stopping with the validation set.
# If the Lôss on the validation set does not improve for 4 epochs which is the patience parameter, then
# we stop training the model


fitted <- model %>%
  fit(train_dl, epochs = 50, valid_data = valid_dl,
      callbacks = list(
        luz_callback_early_stopping(patience = 4),
        luz_callback_lr_scheduler(
          lr_one_cycle,
          max_lr = 0.01,
          epochs = 50,
          steps_per_epoch = length(train_dl),
          call_on = "on_batch_end")
      ),
      verbose = TRUE)

# Saving and Loading trained models

# luz_save(fitted, "./data/day.rds")

# fitted <- luz_load("./data/trained_nn_models/day.rds")
# fitted <- luz_load("./data/trained_nn_models/hour.rds")
# fitted <- luz_load("./data/trained_nn_models/nn_wallboxes_with_12_hour_minute.rds")


plot(fitted)
evaluate(fitted, test_dl)

# Following the source again closely to test the model

test <- wallboxes %>%
  filter(year(Date) == 2022, month(Date) >= 7, !(month(Date) == 7 & day(Date) < 15))


test_matrix <- test %>%
  as_tibble() %>%
  select(total_power) %>%
  as.matrix()

ds <- demand_dataset(test_matrix, n_timesteps)
dl <- ds %>% dataloader(batch_size = length(ds))

# This is where we forecast the values. This is done on a rolling basis.
preds  <- predict(fitted, dl)

preds <- preds$to(device = "cpu") %>% as.matrix()
preds <- c(rep(NA, n_timesteps), preds)


pred_ts <- test %>%
  add_column(forecast = preds * train_sd + train_mean) %>%
  pivot_longer(-Date) %>%
  update_tsibble(key = name)

pred_ts
pred_ts$Date <- as.POSIXct(pred_ts$Date)


# Creating two variables, actual and forecasts
actual_data <- pred_ts %>% filter(name == "total_power")
forecast_data <- pred_ts %>% filter(name == "forecast")


# Depending on zoom performing preprocessing. If the Zoom is minute or Hours I will aggregate the data into daily forecasts


if (zoom == "day"){
  actual_data_august <- actual_data %>% filter(month(Date) == 8)
  forecast_data_august <- forecast_data %>% filter(month(Date) == 8)
} else if (zoom == "hour"){
  # Day 21 has missing hours
  actual_data_august <- actual_data %>% filter(month(Date) == 8  & day(Date) != 21)
  forecast_data_august <- forecast_data %>% filter(month(Date) == 8  & day(Date) != 21)
  # Aggregating to Day
  
  actual_data_august <- actual_data_august %>%
    group_by(Date = floor_date(Date, '1 day')) %>%
    summarise(value = sum(value))
  
  forecast_data_august <- forecast_data_august %>%
    group_by(Date = floor_date(Date, '1 day')) %>%
    summarise(value = sum(value))
} else if (zoom == "minute"){
  actual_data_august <- actual_data %>% filter(month(Date) == 8  & day(Date) != 21)
  forecast_data_august <- forecast_data %>% filter(month(Date) == 8  & day(Date) != 21)
  
  ## Aggregate to hour
  actual_data_august <- actual_data_august %>%
    group_by(Date = floor_date(Date, '1 hour')) %>%
    summarise(value = mean(value))
  
  forecast_data_august <- forecast_data_august %>%
    group_by(Date = floor_date(Date, '1 hour')) %>%
    summarise(value = mean(value))
  
  ## Aggregating to day
  
  actual_data_august <- actual_data_august %>%
    group_by(Date = floor_date(Date, '1 day')) %>%
    summarise(value = sum(value))
  
  forecast_data_august <- forecast_data_august %>%
    group_by(Date = floor_date(Date, '1 day')) %>%
    summarise(value = sum(value))
}




plot_ly() %>%
  add_trace(data = actual_data_august, x = ~Date, y = ~value, name = "actual", type = 'scatter', mode = 'lines') %>%
  add_trace(data = forecast_data_august, x = ~Date, y = ~value, name = 'predictions', mode = 'lines') %>%
  layout(title = 'RNN+LSTM Forecast',
         yaxis = list(title = "kWh"),
         xaxis = list(title = "Hours")
         
         
  )



# if (length(actual_data_august$value) == length(forecast_data_august$value)) {
#   
#   mae_value <- mae(actual_data_august$value, forecast_data_august$value)
#   print(paste0("Mean Absolute Error (MAE) for August: ", mae_value))
#   
#   mape_value <- mape(actual_data_august$value, forecast_data_august$value)
#   print(paste0("Mean Absolute Percentage Error (MAPE) for August: ", mape_value))
#   
# }



# I will also be plotting forecasts on the entire dataset


# Similar to above
test <- wallboxes


test_matrix <- test %>%
  as_tibble() %>%
  select(total_power) %>%
  as.matrix()

ds <- demand_dataset(test_matrix, n_timesteps)
dl <- ds %>% dataloader(batch_size = length(ds))

# As for minute zoom, my system does not have enough memory to make the predictions
# For the other zooms it works as expected.
preds  <- predict(fitted, dl)

preds <- preds$to(device = "cpu") %>% as.matrix()
preds <- c(rep(NA, n_timesteps), preds)


pred_ts <- test %>%
  add_column(forecast = preds * train_sd + train_mean) %>%
  pivot_longer(-Date) %>%
  update_tsibble(key = name)

pred_ts
pred_ts$Date <- as.POSIXct(pred_ts$Date)


actual_data_total <- pred_ts %>% filter(name == "total_power")
forecast_data_total <- pred_ts %>% filter(name == "forecast")

actual_data_total <- actual_data_total %>% filter(month(Date) >= 2 )
forecast_data_total <- forecast_data_total %>% filter(month(Date) >= 2 )


if (zoom == "hour"){
  actual_data_total <- actual_data_total %>%
    group_by(Date = floor_date(Date, '1 day')) %>%
    summarise(value = sum(value))
  
  forecast_data_total <- forecast_data_total %>%
    group_by(Date = floor_date(Date, '1 day')) %>%
    summarise(value = sum(value))
} else if (zoom == "minute"){

  ## Aggregate to hour
  actual_data_total <- actual_data_total %>%
    group_by(Date = floor_date(Date, '1 hour')) %>%
    summarise(value = mean(value))
  
  forecast_data_total <- forecast_data_total %>%
    group_by(Date = floor_date(Date, '1 hour')) %>%
    summarise(value = mean(value))
  
  ## Aggregating to day
  
  actual_data_total <- actual_data_total %>%
    group_by(Date = floor_date(Date, '1 day')) %>%
    summarise(value = sum(value))
  
  forecast_data_total <- forecast_data_total %>%
    group_by(Date = floor_date(Date, '1 day')) %>%
    summarise(value = sum(value))
}


plot_ly() %>%
  add_trace(data = actual_data_total, x = ~Date, y = ~value, name = "actual", type = 'scatter', mode = 'lines') %>%
  add_trace(data = forecast_data_total, x = ~Date, y = ~value, name = 'predictions', mode = 'lines') %>%
  layout(title = 'RNN+LSTM Forecast',
         yaxis = list(title = "kWh"),
         xaxis = list(title = "Hours")
         
         
  )


# Calculating MAPE and MAE 

if (length(actual_data_total$value) == length(forecast_data_total$value)) {
  
  mae_value <- mae(actual_data_total$value, forecast_data_total$value)
  print(paste0("Mean Absolute Error (MAE): ", mae_value))
  
  mape_value <- mape(actual_data_total$value, forecast_data_total$value)
  print(paste0("Mean Absolute Percentage Error (MAPE): ", mape_value))
  
}

