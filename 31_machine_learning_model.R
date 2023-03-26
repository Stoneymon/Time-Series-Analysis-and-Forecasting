# Convert to data.table
wallboxes_jan_aug
wallboxes_jan_aug_DT <- setDT(wallboxes_jan_aug)

# add more columns that could be used for predicting the total power needed

# total_power used on previous day
wallboxes_jan_aug_DT[, total_power_previous_day := shift(total_power, 1)]

# total_power used on the same day last week
wallboxes_jan_aug_DT[, total_power_same_day_previous_week := shift(total_power, 7)]

# average of total_power
wallboxes_jan_aug_DT[, total_power_avg := mean(total_power)]

# total_power used last week
wallboxes_jan_aug_DT[, week := isoweek(Date)]
sum_each_week <- wallboxes_jan_aug_DT[, sum(total_power), week] # select sum(total_power) where ALL ROWS group by week
wallboxes_jan_aug_DT <- merge(wallboxes_jan_aug_DT, sum_each_week, by = "week", all.x = TRUE)
setnames(wallboxes_jan_aug_DT,
         old = c("V1"),
         new = c("total_power_last_week"))

# remove rows containing NA values
colSums(is.na(wallboxes_jan_aug_DT))
wallboxes_jan_aug_DT <- na.omit(wallboxes_jan_aug_DT)

wallboxes_jan_aug_DT
str(wallboxes_jan_aug_DT)

























