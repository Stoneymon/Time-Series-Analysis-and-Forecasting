if (rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# LOAD LIBRARIES ----
library(data.table)
library(plotly)
library(caret)
library(ggplot2)
library(hrbrthemes)
library(writexl)
library(dplyr)


# IMPORT DATA ----
data_battery <- fread("./data/raw/data_battery_2022-01-01_2023-02-21.csv")
data_grid <- fread("./data/raw/data_grid_2022-01-01_2023-02-21.csv")
data_photovoltaic <- fread("./data/raw/data_photovoltaic_2022-01-01_2023-02-21.csv")
data_wallboxes <- fread("./data/raw/data_wallboxes_2022-01-01_2023-02-21.csv")
