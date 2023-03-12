data_battery_jan <- data_battery[data_battery$V1 < as.Date("2022-02-01"),]

p <- data_battery_jan %>% ggplot(aes(x=V1, y=LEM.Overview.Wirkleistung_P)) +
  geom_area(fill="#69b3a2", alpha=0.5) + geom_line(color="#69b3a2") +
  ylab("Wirkleistung") + theme_ipsum()
p <- ggplotly(p)
p

p2 <- data_battery_jan %>% ggplot(aes(x=V1, y=LEM.Overview.Energy_Charged)) +
  geom_area(fill="#69b3a2", alpha=0.5) + geom_line(color="#69b3a2") +
  ylab("EC") + theme_ipsum()
p2 <- ggplotly(p2)
p2

p3 <- data_battery_jan %>% ggplot(aes(x=V1, y=LEM.Overview.Energy_Discharged)) +
  geom_area(fill="#69b3a2", alpha=0.5) + geom_line(color="#69b3a2") +
  ylab("ED") + theme_ipsum()
p3 <- ggplotly(p3)
p3 

p4 <- data_battery_jan %>% ggplot(aes(x=V1, y=LEM.Overview.Battery_SOC)) +
  geom_area(fill="#69b3a2", alpha=0.5) + geom_line(color="#69b3a2") +
  ylab("SOC") + theme_ipsum()
p4 <- ggplotly(p4)
p4