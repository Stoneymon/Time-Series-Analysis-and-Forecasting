library(dplyr)

if (rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# LOAD LIBRARIES ----
library(data.table)
library(plotly)
library(caret)
library(ggplot2)
library(hrbrthemes)

# IMPORT DATA ----

data_wallboxes <- fread("./data/raw/data_wallboxes_2022-01-01_2023-02-21.csv")
data_wallboxes_1 <- data_wallboxes[data_wallboxes$V1 < as.Date("2022-09-27"),]


data_wallboxes_1 <- data_wallboxes_1

subset_wallboxes = subset(data_wallboxes_1, select = c(V1,
                                             LEM.KEBA_P30_1.Wirkleistung_P,
                                             LEM.KEBA_P30_2.Wirkleistung_P,
                                             LEM.KEBA_P30_3.Wirkleistung_P,
                                             LEM.Ladebox1.P,
                                             LEM.Ladebox2.P,
                                             LEM.Ladebox3.P,
                                             LEM.Delta_Wallbox.Wirkleistung_P,
                                             LEM.Raption_50.Wirkleistung_P))
subset_wallboxes
colnames(data_wallboxes_1)


sum(subset_wallboxes[1,-1])
subset_wallboxes$Total.P <- apply(subset_wallboxes[, -1], 1, sum)

subset_wallboxes_total <- subset(subset_wallboxes, select = c(V1,Total.P))
subset_wallboxes_total



# assuming your table is named "power_data" and has two columns: "time" and "power"
# create a new column with the date
str(subset_wallboxes_total)
wallboxes_with_date_transformed <- subset_wallboxes_total %>% 
  mutate(V1 = as.Date(V1))



# group by date and summarize to get total power per day
total_power_per_day <- wallboxes_with_date_transformed %>% 
  group_by(V1) %>% 
  summarize(total_power = sum(Total.P))


# interactive graph total power per day
p <- total_power_per_day %>% ggplot(aes(x = V1)) +
  geom_line(aes(y=total_power), color = "blue")
p <- ggplotly(p)
p
