# Since this is a new topic I followed this source closely. Some parts of the code are also from this source
# Source : Deep Learning and Scientific Computing with R torch by Sigrid Keydana (April, 2023)


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


# Hourly
###########################################
wallboxes_h <- wallboxes_hourly
wallboxes_h %>% glimpse()

# Converting to tsibble which is needed to decompose the plot
wallboxes_h_ts <- wallboxes_h %>%
  as_tsibble(index = Time) %>%
  fill_gaps()


# There are missing values that I am filling with approximations as they are not many
wallboxes_h_ts <- wallboxes_h_ts %>%
  mutate(wallboxes = na.approx(wallboxes, na.rm = FALSE))

# Decompose wallboxes_hourly_ts using STL and plot
decompose <- wallboxes_h_ts %>%
  model(feasts::STL(wallboxes)) %>%
  components()

decompose %>% autoplot()

# Zooming in on January
decompose_jan <- wallboxes_h_ts %>%
  filter(year(Time) == 2022, month(Time) == 1) %>%
  model(feasts::STL(wallboxes)) %>%
  components()

decompose_jan %>% autoplot()


# Training
power_train <- wallboxes_h_ts %>%
  mutate(Year = year(Time), Month = month(Time)) %>%
  filter(Year == 2022 & Month >= 1 & Month <= 5) %>%
  pull(wallboxes) %>%
  as.matrix()


# Validation - This will help prevent overfitting
power_validation <- wallboxes_h_ts %>%
  mutate(Year = year(Time), Month = month(Time)) %>%
  filter(Year == 2022 & Month >= 6 & Month <= 7) %>%
  pull(wallboxes) %>%
  as.matrix()

# Test
power_test <- wallboxes_h_ts %>%
  mutate(Year = year(Time), Month = month(Time)) %>%
  filter(Year == 2022 & Month >= 8) %>%
  pull(wallboxes) %>%
  as.matrix()

length(power_train)
length(power_validation)
length(power_test)




train_mean <- mean(power_train)
train_sd <- sd(power_train)



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

# We want the daily, weekly data as we saw in the decompose function. I will use 2 weeks as training input.
n_timesteps <- 7 * 24 * 2

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

model <- nn_module(
  initialize = function(input_size,
                        hidden_size,
                        dropout = 0.2,
                        num_layers = 1,
                        rec_dropout = 0) {
    self$num_layers <- num_layers
    
    self$rnn <- nn_lstm(
      input_size = input_size,
      hidden_size = hidden_size,
      num_layers = num_layers,
      dropout = rec_dropout,
      batch_first = TRUE
    )
    # self$rnn <- nn_gru(
    #   input_size = input_size,
    #   hidden_size = hidden_size,
    #   num_layers = num_layers,
    #   dropout = dropout,
    #   batch_first = TRUE
    # )
    
    self$dropout <- nn_dropout(dropout)
    self$output <- nn_linear(hidden_size, 1)
  },
  forward = function(x) {
    (x %>%
       # these two are equivalent
       # (1)
       # take output tensor,restrict to last time step
       self$rnn())[[1]][, dim(x)[2], ] %>%
      # (2)
      # from list of state tensors,take the first,
      # and pick the final layer
      # self$rnn())[[2]][[1]][self$num_layers, , ] %>%
      self$dropout() %>%
      self$output()
  }
)


input_size <- 1
hidden_size <- 32
num_layers <- 2
rec_dropout <- 0.1

model <- model %>%
  setup(optimizer = optim_adam, loss = nn_mse_loss()) %>%
  set_hparams(
    input_size = input_size,
    hidden_size = hidden_size,
    num_layers = num_layers,
    rec_dropout = rec_dropout
  )

rates_and_losses <- model %>% 
  lr_finder(train_dl, start_lr = 1e-3, end_lr = 1)
rates_and_losses %>% plot()

# With this plot I am now looking for the minimum

fitted <- model %>%
  fit(train_dl, epochs = 50, valid_data = valid_dl,
      callbacks = list(
        luz_callback_early_stopping(patience = 5),
        luz_callback_lr_scheduler(
          lr_one_cycle,
          max_lr = 0.05,
          epochs = 50,
          steps_per_epoch = length(train_dl),
          call_on = "on_batch_end")
      ),
      verbose = TRUE)

plot(fitted)
evaluate(fitted, test_dl)


# Getting actual values from tensors 
# 

# Actual values vs Predicted Values on Test

test_viz <- wallboxes_h_ts %>%
  filter(year(Time) == 2022, month(Time) >= 7)

# Converting it to matrix form
test_viz_matrix <- test_viz %>%
  as_tibble() %>%
  select(wallboxes) %>%
  as.matrix()

# Creating a dataset and dataloader
viz_ds <- demand_dataset(test_viz_matrix, n_timesteps)
viz_dl <- viz_ds %>% dataloader(batch_size = length(viz_ds))

# Making predictions
preds  <- predict(fitted, viz_dl)

# Converting the torch tensor to R array
preds <- preds$to(device = "cpu") %>% as.matrix()
preds <- c(rep(NA, n_timesteps), preds)
preds

pred_ts <- test_viz %>%
  add_column(forecast = preds * train_sd + train_mean) %>%
  pivot_longer(-Time) %>%
  update_tsibble(key = name)

pred_ts




library(plotly)

pred_ts$Time <- as.POSIXct(pred_ts$Time)


# Filter data for actual and forecast
actual_data <- pred_ts %>% filter(name == "wallboxes")
forecast_data <- pred_ts %>% filter(name == "forecast")

# Create plotly plot
plot_ly() %>%
  add_trace(data = actual_data, x = ~Time, y = ~value, name = "actual", type = 'scatter', mode = 'lines') %>%
  add_trace(data = forecast_data, x = ~Time, y = ~value, name = 'predictions', mode = 'lines') %>%
  layout(title = 'RNN+LSTM Forecast')


# Calculation MAPE and MAE 

# Filter data for August
actual_data_august <- actual_data %>% filter(month(Time) == 8)
forecast_data_august <- forecast_data %>% filter(month(Time) == 8)

plot_ly() %>%
  add_trace(data = actual_data_august, x = ~Time, y = ~value, name = "actual", type = 'scatter', mode = 'lines') %>%
  add_trace(data = forecast_data_august, x = ~Time, y = ~value, name = 'predictions', mode = 'lines') %>%
  layout(title = 'RNN+LSTM Forecast')

# Check if they have the same length
if (length(actual_data_august$value) == length(forecast_data_august$value)) {
  
  mae_value <- mae(actual_data_august$value, forecast_data_august$value)
  print(paste0("Mean Absolute Error (MAE) for August: ", mae_value))
  
  mape_value <- mape(actual_data_august$value, forecast_data_august$value)
  print(paste0("Mean Absolute Percentage Error (MAPE) for August: ", mape_value))
  
}


# End Hourly
##########################################


# Initially Used Daily Total power to predict next Day 
# With RNN this did not yield a good result
# Then modified the code to use per minute data of a whole week to predict the next day
# This takes too much time to run -> R suggested ETS 2 hours per iteration. I decided eventually that 
# The hourly data () is a good balance between accuracy and computation time


#####################################

# By running only certain lines on 10_preprocessing i get the minutes data
# wallboxes_jan <- wallboxes_jan_aug
# wallboxes_jan <- wallboxes_jan[,c(1,10)]
# wallboxes_jan %>% glimpse()
# 
# 
# library(tidyr)
# 
# wallboxes_jan <- as.data.table(wallboxes_jan)
# # 
# wallboxes_jan %>% glimpse()
# # 
# # # Convert wallboxes to a tsibble
# wallboxes_jan_ts <- wallboxes_jan %>%
#   arrange(Date) %>%
#   as_tsibble(index = Date) %>%
#   fill_gaps()
# 
# wallboxes_jan_ts <- wallboxes_jan_ts %>%
#   dplyr::filter(second(Date) == 0)
# 
# sum(is.na(wallboxes_jan_ts))
# 
# wallboxes_jan_ts <- wallboxes_jan_ts %>%
#   mutate(total.P = na.approx(total.P, na.rm = FALSE))
# 
# 
# sum(is.na(wallboxes_jan_ts))
# 
# #wallboxes_jan_ts <- na.omit(wallboxes_jan_ts)
# 
#   
# # 
# decompose_jan <- wallboxes_jan_ts %>%
#   model(feasts::STL(total.P)) %>%
#   components()
# 
# decompose_jan %>% autoplot()
# 
# 
# 
# power_train <- wallboxes_jan_ts %>%
#   mutate(Year = year(Date), Week = week(Date), Month = month(Date)) %>%
#   filter(Year == 2022 & Month == 1) %>%
#   pull(total.P) %>%
#   as.matrix()
# 
# # Validation set (Third week of January 2022)
# power_validation <- wallboxes_jan_ts %>%
#   mutate(Year = year(Date), Week = week(Date), Month = month(Date)) %>%
#   filter(Year == 2022 & Month == 2) %>%
#   pull(total.P) %>%
#   as.matrix()
# 
# # Test set (Last week of January 2022)
# power_test <- wallboxes_jan_ts %>%
#   mutate(Year = year(Date), Week = week(Date), Month = month(Date)) %>%
#   filter(Year == 2022 & Month == 3) %>%
#   pull(total.P) %>%
#   as.matrix()
# # 
# length(power_train)
# length(power_validation)
# length(power_test)
# # 
# train_mean <- mean(power_train)
# train_sd <- sd(power_train)
# # 
# demand_dataset <- dataset(
#   name = "demand_dataset",
#   initialize = function(x,
#                         n_timesteps,
#                         n_forecast,
#                         sample_frac = 1) {
#     self$n_timesteps <- n_timesteps
#     self$n_forecast <- n_forecast
#     self$x <- torch_tensor((x - train_mean) / train_sd)
#     
#     n <- length(self$x) -
#       self$n_timesteps - self$n_forecast + 1
#     
#     self$starts <- sort(sample.int(
#       n = n,
#       size = n * sample_frac
#     ))
#   },
#   .getitem = function(i) {
#     start <- self$starts[i]
#     end <- start + self$n_timesteps - 1
#     
#     list(
#       x = self$x[start:end],
#       y = self$x[(end + 1):(end + self$n_forecast)]$
#         squeeze(2)
#     )
#   },
#   .length = function() {
#     length(self$starts)
#   }
# )
# n_timesteps <- 60 * 24 * 7
# n_forecast <- 60 * 24 * 7
# 
# train_ds <- demand_dataset(
#   power_train,
#   n_timesteps,
#   n_forecast,
#   sample_frac = 1
# )
# valid_ds <- demand_dataset(
#   power_validation,
#   n_timesteps,
#   n_forecast,
#   sample_frac = 1
# )
# test_ds <- demand_dataset(
#   power_test,
#   n_timesteps,
#   n_forecast
# )
# 
# 
# # train_ds <- demand_dataset(power_train, n_timesteps)
# # length(power_validation)
# # n_timesteps
# # valid_ds <- demand_dataset(power_validation, n_timesteps)
# # test_ds <- demand_dataset(power_test, n_timesteps)
# 
# n_train <- length(power_train) - n_timesteps
# n_valid <- length(power_validation) - n_timesteps
# n_test <- length(power_test) - n_timesteps
# 
# print(paste("n_train:", n_train))
# print(paste("n_valid:", n_valid))
# print(paste("n_test:", n_test))
# 
# 
# dim(train_ds[1]$x)
# dim(train_ds[1]$y)
# 
# batch_size <- 128
# 
# train_dl <- train_ds %>%
#   dataloader(batch_size = batch_size, shuffle = TRUE)
# valid_dl <- valid_ds %>%
#   dataloader(batch_size = batch_size)
# test_dl <- test_ds %>%
#   dataloader(batch_size = length(test_ds))
# 
# b <- train_dl %>%
#   dataloader_make_iter() %>%
#   dataloader_next()
# 
# dim(b$x)
# dim(b$y)
# 
# model <- nn_module(
#   initialize = function(input_size,
#                         hidden_size,
#                         linear_size,
#                         output_size,
#                         dropout = 0.2,
#                         num_layers = 1,
#                         rec_dropout = 0) {
#     self$num_layers <- num_layers
#     
#     self$rnn <- nn_lstm(
#       input_size = input_size,
#       hidden_size = hidden_size,
#       num_layers = num_layers,
#       dropout = rec_dropout,
#       batch_first = TRUE
#     )
#     
#     self$dropout <- nn_dropout(dropout)
#     self$mlp <- nn_sequential(
#       nn_linear(hidden_size, linear_size),
#       nn_relu(),
#       nn_dropout(dropout),
#       nn_linear(linear_size, output_size)
#     )
#   },
#   forward = function(x) {
#     x <- self$rnn(x)[[2]][[1]][self$num_layers, , ] %>%
#       self$mlp()
#   }
# )
# 
# 
# input_size <- 1
# hidden_size <- 32
# linear_size <- 512
# dropout <- 0.5
# num_layers <- 2
# rec_dropout <- 0.2
# 
# model <- model %>%
#   setup(optimizer = optim_adam, loss = nn_mse_loss()) %>%
#   set_hparams(
#     input_size = input_size,
#     hidden_size = hidden_size,
#     linear_size = linear_size,
#     output_size = n_forecast,
#     num_layers = num_layers,
#     rec_dropout = rec_dropout
#   )
# 
# rates_and_losses <- model %>% lr_finder(
#   train_dl,
#   start_lr = 1e-4,
#   end_lr = 0.5
# )
# rates_and_losses %>% plot()
# 
# fitted <- model %>%
#   fit(train_dl, epochs = 100, valid_data = valid_dl,
#       callbacks = list(
#         luz_callback_early_stopping(patience = 3),
#         luz_callback_lr_scheduler(
#           lr_one_cycle,
#           max_lr = 0.01,
#           epochs = 100,
#           steps_per_epoch = length(train_dl),
#           call_on = "on_batch_end")
#       ),
#       verbose = TRUE)
# 
# plot(fitted)
# evaluate(fitted, test_dl)
# 
# 
# 















