if (rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# LOAD LIBRARIES ----
library(data.table)
library(plotly)
library(caret)

# IMPORT DATA ----
data_battery <- fread("./data/raw/data_battery_2022-01-01_2023-02-21.csv")
data_grid <- fread("./data/raw/data_grid_2022-01-01_2023-02-21.csv")
data_photovoltaic <- fread("./data/raw/data_photovoltaic_2022-01-01_2023-02-21.csv")
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