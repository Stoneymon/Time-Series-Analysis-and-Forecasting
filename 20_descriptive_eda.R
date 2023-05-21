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

## total power per wallbox
boxplot(total_power_per_wb[, 2:9])

fig <- plot_ly(total_power_per_wb, x = ~Date, y = ~KEBA_1, name = 'Keba 1', type = 'scatter', mode = 'lines')
fig <- fig %>% add_trace(y = ~KEBA_2, name = 'Keba 2', type = 'scatter', mode = 'lines')
fig <- fig %>% add_trace(y = ~KEBA_3, name = 'Keba 3', type = 'scatter', mode = 'lines')
fig <- fig %>% add_trace(y = ~Ladebox1, name = 'Ladebox 1', type = 'scatter', mode = 'lines')
fig <- fig %>% add_trace(y = ~Ladebox2, name = 'Ladebox 2', type = 'scatter', mode = 'lines')
fig <- fig %>% add_trace(y = ~Ladebox3, name = 'Ladebox 3', type = 'scatter', mode = 'lines')
fig <- fig %>% add_trace(y = ~Delta, name = 'Delta', type = 'scatter', mode = 'lines')
fig <- fig %>% add_trace(y = ~Raption_50, name = 'Raption 50', type = 'scatter', mode = 'lines')

fig
