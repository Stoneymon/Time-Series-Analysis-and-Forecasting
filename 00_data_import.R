if (rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# LOAD LIBRARIES ----
library(data.table)
library(plotly)
library(caret)
<<<<<<< Updated upstream
=======
library(ggplot2)
library(hrbrthemes)
library(writexl)
>>>>>>> Stashed changes

# IMPORT DATA ----
data_battery <- fread("./data/raw/data_battery_2022-01-01_2023-02-21.csv")
data_grid <- fread("./data/raw/data_grid_2022-01-01_2023-02-21.csv")
data_photovoltaic <- fread("./data/raw/data_photovoltaic_2022-01-01_2023-02-21.csv")
<<<<<<< Updated upstream
data_wallboxes <- fread("./data/raw/data_wallboxes_2022-01-01_2023-02-21.csv")

# EXPLORING THE DATASET (EDA) ----
dim(data_battery)
str(data_battery)
summary(data_battery)

dim(data_grid)
str(data_grid)
summary(data_grid)

dim(data_photovoltaic)
str(data_photovoltaic)
summary(data_photovoltaic)

dim(data_wallboxes)
str(data_wallboxes)
summary(data_wallboxes)

# # SPLIT DATE & TIME INTO TWO COLUMNS ----
# data_battery$Date <- as.Date(data_battery$V1)
# data_battery$Time <- format(as.POSIXct(data_battery$V1), format="%H:%M:%S")
# # data_battery <- subset(data_battery, select=-c(V1))
# setcolorder(data_battery, c(5,6,1:4))
# data_battery
# 
# data_grid$Date <- as.Date(data_grid$V1)
# data_grid$Time <- format(as.POSIXct(data_grid$V1), format="%H:%M:%S")
# # data_grid <- subset(data_grid, select=-c(V1))
# setcolorder(data_grid, c(2,3,1))
# data_grid
# 
# data_photovoltaic$Date <- as.Date(data_photovoltaic$V1)
# data_photovoltaic$Time <- format(as.POSIXct(data_photovoltaic$V1), format="%H:%M:%S")
# # data_photovoltaic <- subset(data_photovoltaic, select=-c(V1))
# setcolorder(data_photovoltaic, c(2,3,1))
# data_photovoltaic
# 
# data_wallboxes$Date <- as.Date(data_wallboxes$V1)
# data_wallboxes$Time <- format(as.POSIXct(data_wallboxes$V1), format="%H:%M:%S")
# # data_wallboxes <- subset(data_wallboxes, select=-c(V1))
# setcolorder(data_wallboxes, c(41,42,1:40))
# data_wallboxes

# CHECK IF DATES ARE CONSECUTIVE ----
unique_dates_db <- data.table(as.Date(data_battery$V1[!duplicated(as.Date(data_battery$V1))]))
unique_dates_db$consecutive <- c(NA,diff(as.Date(unique_dates_db$V1))==1)
unique_dates_db[unique_dates_db$consecutive==FALSE]

unique_dates_dg <- data.table(as.Date(data_grid$V1[!duplicated(as.Date(data_grid$V1))]))
unique_dates_dg$consecutive <- c(NA,diff(as.Date(unique_dates_dg$V1))==1)
unique_dates_dg[unique_dates_dg$consecutive==FALSE]

unique_dates_dp <- data.table(as.Date(data_photovoltaic$V1[!duplicated(as.Date(data_photovoltaic$V1))]))
unique_dates_dp$consecutive <- c(NA,diff(as.Date(unique_dates_dp$V1))==1)
unique_dates_dp[unique_dates_dp$consecutive==FALSE]

unique_dates_wb <- data.table(as.Date(data_wallboxes$V1[!duplicated(as.Date(data_wallboxes$V1))]))
unique_dates_wb$consecutive <- c(NA,diff(as.Date(unique_dates_wb$V1))==1)
unique_dates_dp[unique_dates_dp$consecutive==FALSE]
# 2022-09-27 and 2022-10-02 are not consecutive

# # CHECK IF TIME IS CONSECUTIVE ----
# unique_time_db <- data.table(data_battery$V1)
# unique_time_db$consecutive <- c(NA,diff(as.POSIXct(unique_time_db$V1, format="%H:%M:%S"))==60)
# unique_time_db[unique_time_db$consecutive==FALSE]

# SPLIT DATASET INTO BEFORE AND AFTER GAP ----
data_battery_1 <- data_battery[data_battery$V1 < as.Date("2022-09-27"),]
data_battery_2 <- data_battery[data_battery$V1 >= as.Date("2022-10-02"),]

data_grid_1 <- data_grid[data_grid$V1 < as.Date("2022-09-27"),]
data_grid_2 <- data_grid[data_grid$V1 >= as.Date("2022-10-02"),]

data_photovoltaic_1 <- data_photovoltaic[data_photovoltaic$V1 < as.Date("2022-09-27"),]
data_photovoltaic_2 <- data_photovoltaic[data_photovoltaic$V1 >= as.Date("2022-10-02")]

data_wallboxes_1 <- data_wallboxes[data_wallboxes$V1 < as.Date("2022-09-27"),]
data_wallboxes_2 <- data_wallboxes[data_wallboxes$V1 >= as.Date("2022-10-02"),]

plot(data_battery_1$V1, data_battery_1$LEM.Overview.Wirkleistung_P)
plot(data_battery_1$V1, data_battery_1$LEM.Overview.Energy_Charged)
plot(data_battery_1$V1, data_battery_1$LEM.Overview.Energy_Discharged)
plot(data_battery_1$V1, data_battery_1$LEM.Overview.Battery_SOC)
=======
data_wallboxes <- fread("./data/raw/data_wallboxes_2022-01-01_2023-02-21.csv")
>>>>>>> Stashed changes
