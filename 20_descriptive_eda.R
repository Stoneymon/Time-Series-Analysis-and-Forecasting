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


# PLOTTING TOTAL POWER ----
p <- wallboxes_jan_aug %>% ggplot(aes(x=Date, y=total_power)) +
  geom_area(fill="#69b3a2", alpha=0.5) + geom_line(color="#69b3a2") + 
  geom_line(aes(y=SOC$total_SOC), color="red")+ theme_ipsum()
p <- ggplotly(p)
p

