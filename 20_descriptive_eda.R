# EXPLORING THE DATASET (EDA) ----
dim(data_battery_1)
str(data_battery_1)
summary(data_battery_1)

dim(data_grid_1)
str(data_grid_1)
summary(data_grid_1)

dim(data_photovoltaic_1)
str(data_photovoltaic_1)
summary(data_photovoltaic_1)

dim(data_wallboxes_1)
str(data_wallboxes_1)
summary(data_wallboxes_1)

dim(wallboxes_jan_aug)
str(wallboxes_jan_aug)
summary(wallboxes_jan_aug)


# PLOTTING ----
## total power - Battery SOC ----
plot_ly(wallboxes_jan_aug, x = ~Date, y = ~total_power, type = "scatter", mode="lines", name = "Total Power", fill="tozeroy", 
        line = list(color = 'rgb(105, 179, 162)')) %>%
  add_lines(x = ~Date, y = ~battery_SOC, mode = "lines", yaxis = "y2", name="Battery SOC", fill="none", line=list(color="rgb(255, 0, 0)")) %>%
  layout(yaxis2 = list(overlaying = "y", side = "right"))



library(tibble)
library(tidyr)
library(lubridate)
library(tsibble)
library(feasts)
library(zoo)




 # Wallbox total power STL Decomposition

wallboxes_h_ts <- wallboxes_hourly %>%
  as_tsibble(index = Time) %>%
  fill_gaps()

wallboxes_h_ts <- wallboxes_h_ts %>%
  mutate(wallboxes = na.approx(wallboxes, na.rm = FALSE))

decompose <- wallboxes_h_ts %>%
  model(feasts::STL(wallboxes)) %>%
  components()

decompose %>% autoplot()


# We Look for the smaller bar value on the left side. A smaller scale implies more significance during predictions
# We see how the Daily and Weekly Trends are significant.
# This implies training data of a few weeks might yield better results



# Grid Predictions EDA


# COLD Week
grid_pred_dt %>%
  filter(Time >= ymd("2022-01-01") & Time < ymd("2022-01-08")) %>%
  plot_ly(x = ~Time) %>%
  add_lines(y = ~wallboxes, name = "Wallboxes") %>%
  add_lines(y = ~grid, name = "Grid") %>%
  add_lines(y = ~pv, name = "PV") %>%
  add_lines(y = ~SOC, name = "SOC")

# Hot Week 
grid_pred_dt %>%
  filter(Time >= ymd("2022-07-01") & Time < ymd("2022-07-08")) %>%
  plot_ly(x = ~Time) %>%
  add_lines(y = ~wallboxes, name = "Wallboxes") %>%
  add_lines(y = ~grid, name = "Grid") %>%
  add_lines(y = ~pv, name = "PV") %>%
  add_lines(y = ~SOC, name = "SOC")


# Zooming into  a day 
grid_pred_dt %>%
  filter(date(Time) == ymd("2022-01-01")) %>%
  plot_ly(x = ~Time) %>%
  add_lines(y = ~wallboxes, name = "Wallboxes") %>%
  add_lines(y = ~grid, name = "Grid") %>%
  add_lines(y = ~pv, name = "PV") %>%
  add_lines(y = ~SOC, name = "SOC")


# Next Hour Grid STL Decomposition

next_grid_dt <- grid_pred_dt[,c("Time","grid_next_hour")]

grid_h_ts <- next_grid_dt %>%
  as_tsibble(index = Time) %>%
  fill_gaps()

sum(is.na(grid_h_ts))

grid_h_ts <- grid_h_ts %>%
  mutate(grid_next_hour = na.approx(grid_next_hour, na.rm = FALSE))
grid_h_ts <- grid_h_ts[c(1:length(grid_h_ts$grid_next_hour)-1),]

  
decompose <- grid_h_ts %>%
  model(feasts::STL(grid_next_hour)) %>%
  components()
decompose %>% autoplot()

# Again we see that Weekly Data and Daily data is relevant. So again in your training model it might be a good idea 
# to have atleast a few weeks worth of data
