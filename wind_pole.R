library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
#library(influxdbclient)
#library(zoo)
#library(VGAM)  # Laplace分布の関数を利用
#library(stats) # KS検定
#library(signal)

data_wind_original <- read_csv("https://raw.githubusercontent.com/e-i314/wind/main/20250125_Strong_MultiDrone_FrontYard/wind_20.csv", col_names = FALSE)

#data_wind_original <- data_wind_original[1000:nrow(data_wind_original),]
data_wind_original <- data_wind_original
data_wind <- data_wind_original[data_wind_original[,4] == 1 , ]

#A: プリアンブル
#B: センサーノードID
#C: 測定有効・無効 [1 : 正常 / 0 : 異常]
#D: 風向[°(時計回り)]
#E: 風速 [m/s]
#F: 機首風速(筐体マーキング方向) [m/s]
#G: 音速 [m/s]
#H: 音仮温度 [℃]
#I: 改行コード

data_wind[,2:4] <- NULL
data_wind<- data_wind[-1,]
colnames(data_wind) <- c("timestamp", "winddir", "windspd", "windspd_head", "soundspd","temp")
data_wind$soundspd　<- NULL


data_wind$timestamp <- sub(":b'#", "", data_wind$timestamp)
data_wind$timestamp <- 
  data_wind$timestamp %>%
  parse_date_time(., orders = "Ymd HMS", tz = "Asia/Tokyo") 


data_wind$temparature <- sub("\\\\r\\\\n'", "", data_wind$temp)
data_wind$temparature <- as.numeric(data_wind$temp)


data_wind[is.na(data_wind)] <- 0
data_wind$temp <- NULL

ggplot(data = data_wind, aes(x = timestamp, y = windspd)) +
  geom_line(size = 0.2, na.rm = FALSE) +  # NAはそのまま残す（線を切る）
  labs(
    title = "Time Series of Wind Speed",
    x = "Time (hh:mm)",
    y = "Wind Speed (m/s)"
  ) 
