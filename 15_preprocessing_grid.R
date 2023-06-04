library(lubridate)
library(dplyr)

grid_pred_dt <- data.table(
  Time=data_battery_1$V1,
  KEBA1=data_wallboxes_1$LEM.KEBA_P30_1.Wirkleistung_P,
  KEBA2=data_wallboxes_1$LEM.KEBA_P30_2.Wirkleistung_P,
  KEBA3=data_wallboxes_1$LEM.KEBA_P30_3.Wirkleistung_P,
  Ladebox1=data_wallboxes_1$LEM.Ladebox1.P,
  Ladebox2=data_wallboxes_1$LEM.Ladebox2.P,
  Ladebox3=data_wallboxes_1$LEM.Ladebox3.P,
  Delta=data_wallboxes_1$LEM.Delta_Wallbox.Wirkleistung_P,
  Raption50=data_wallboxes_1$LEM.Raption_50.Wirkleistung_P)

grid_pred_dt$wallboxes <- rowSums(grid_pred_dt[, c(2:9)])

grid_pred_dt <- grid_pred_dt %>%
  group_by(Time=floor_date(Time, '1 hour')) %>%
  summarize(wallboxes=mean(wallboxes))

data_grid_1 <- data_grid_1 %>% group_by(Time=floor_date(V1, '1 hour')) %>%
  summarize(grid=mean(LEM.Einspeisung_HV_NE6.Wirkleistung_P))

grid_pred_dt$grid <- data_grid_1$grid

data_photovoltaic_1<- data_photovoltaic_1 %>%
  group_by(Time=floor_date(V1, '1 hour')) %>%
  summarize(pv=mean(LEM.PV_Anlage.Wirkleistung_P))

grid_pred_dt$pv <- data_photovoltaic_1$pv

db <- data_battery_1[, .( SOC = last(LEM.Overview.Battery_SOC)), floor_date(V1, '1 hour')]

grid_pred_dt$SOC <- db$SOC


wallboxes_hourly <- data.table()
wallboxes_hourly$Time <- grid_pred_dt$Time
wallboxes_hourly$wallboxes <- grid_pred_dt$wallboxes

setDT(grid_pred_dt)
grid_pred_dt[,grid_next_hour:=shift(grid,-1)]