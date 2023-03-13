library(data.table)
if (rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
# IMPORT DATA ----
data_battery <- fread("./data/raw/data_battery_2022-01-01_2023-02-21.csv")
data_grid <- fread("./data/raw/data_grid_2022-01-01_2023-02-21.csv")
data_photovoltaic <- fread("./data/raw/data_photovoltaic_2022-01-01_2023-02-21.csv")
data_wallboxes <- fread("./data/raw/data_wallboxes_2022-01-01_2023-02-21.csv")


# 1.0 Data Cleaning

# 1.1 Handle missing values. 0 Implies no missing values
sum(is.na(data_battery))
sum(is.na(data_grid))
sum(is.na(data_photovoltaic))
sum(is.na(data_wallboxes))

# We observe that there are no missing values


# 1.2 Handle duplicates
# 1.3 Handle outliers
# 1.4 Check for consistency in rows and columns
# 1.5 Grouping data 

# 2.0 Data Integration


# 3.0 Data Transformation


# 4.0 Data Reduction