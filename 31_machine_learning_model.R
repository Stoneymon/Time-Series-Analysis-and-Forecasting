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
wallboxes_jan_aug <- fread("./data/preprocessed/total_power_jan-aug.xlsx")

































