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

# 1.1 Handle missing values. False Implies no missing values
any(is.na(data_battery))
any(is.na(data_grid))
any(is.na(data_photovoltaic))
any(is.na(data_wallboxes))

# We observe that there are no missing values

# 1.2 Handle duplicates. False implies each row is a unique time

any(duplicated(data_battery$V1))
any(duplicated(data_grid$V1))
any(duplicated(data_photovoltaic$V1))
any(duplicated(data_wallboxes$V1))


# We observe that there are no duplicate times recorded

# 1.3 Handle Outliers

# Checking for outliers with Boxplot

boxplot(data_battery$V1)
boxplot(data_battery$LEM.Overview.Wirkleistung_P)

# We observe clear outliers. Safe to say our data-set contains outliers

# We will use another way to detect exact locations of these. Then we will replace the anomaly
# with the column average so that the time series data is still consistent (not removing rows)
IQR(data_battery$LEM.Overview.Wirkleistung_P)

# all observations above (quartile) 0.75 + 1.5* IQR or below (quartile) 0.25 - 1.5*IQR are
# Potential outliers

# We can observe data in this outlier range with

length(boxplot.stats(data_battery$LEM.Overview.Wirkleistung_P)$out)
# 160'000 outliers in this column ? <- Need help of professor how to proceed


max(data_wallboxes$LEM.KEBA_P30_1.Wirkleistung_P)
min(data_wallboxes$LEM.KEBA_P30_1.Wirkleistung_P)
mean(data_wallboxes$LEM.KEBA_P30_1.Wirkleistung_P)


# 1.4 Check for consistency in rows and columns
# 1.5 Grouping data 

# 2.0 Data Integration


# 3.0 Data Transformation


# 4.0 Data Reduction