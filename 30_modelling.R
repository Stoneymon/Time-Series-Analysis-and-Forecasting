library(fpp2)
library(forecast)
dim(wallboxes_jan_aug)


mean(wallboxes_jan_aug$total_power)
setDT(wallboxes_jan_aug)


# Shifting 
wallboxes_jan_aug[,my_new_feature:=shift(total_power,1)]
wallboxes_jan_aug
