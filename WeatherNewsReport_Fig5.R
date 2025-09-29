library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
#library(zoo)
#library(VGAM)  # Laplace分布の関数を利用
library(signal)

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

print(head(data_wind))

ggplot(data = data_wind, aes(x = timestamp, y = windspd)) +
  geom_line(size = 0.2, na.rm = FALSE) +  # NAはそのまま残す（線を切る）
  labs(
    title = "Time Series of Wind Speed",
    x = "Time (hh:mm)",
    y = "Wind Speed (m/s)"
  ) 


ggplot(data = data_wind, aes(x = winddir, y = windspd)) +
  geom_point(alpha = 0.1, size = 0.1) +
  coord_polar(start = 0, direction = 1) +
  scale_x_continuous(
    limits = c(0, 360),
    breaks = seq(0, 350, by = 10),   # 10°刻み
    minor_breaks = NULL,   
    labels = function(x) ifelse(x == 360, "0", x) 
    ) +# 360°を0°に統一

    xlab("Wind Direction(deg)")+
    ylab("Wind Speed(m/s)")

# ここでアンチエイリアスフィルターをかける 20250909
#今回のケースでは 10Hzサンプリングのデータを1Hzに間引きしたい.
#1Hzにすると、表現できるのはナイキストの 0.5Hz以下の成分だけ。
#だから cutoff ≈ 0.4Hz を選んで、
#0.4Hzまでの変化は残す

apply_antialias_filter <- function(data, fs = 10, cutoff = 0.4, order = 4) {
  nyquist <- fs / 2
  normal_cutoff <- cutoff / nyquist
  
  # バターワースローパスフィルタ設計
  bf <- butter(order, normal_cutoff, type = "low", plane = "z")
  
  # ゼロ位相フィルタ処理（forward + reverse）
  filtered <- filtfilt(bf, data)
  
  return(filtered)
}

data_wind$windspd_filtered <- apply_antialias_filter(data_wind$windspd, fs = 10, cutoff = 0.4)
data_wind$windspd_head_filtered <- apply_antialias_filter(data_wind$windspd_head, fs = 10, cutoff = 0.4)

######################### 風向もローパスフィルターに通す　
# fs      : 入力サンプリング周波数[Hz]（例: 10）
# cutoff  : カットオフ[Hz]（例: 0.4）
# order   : フィルタ次数（例: 4）
# weight  : 風速などの重み（任意; 弱風で方向が暴れるのを抑えたいとき）
# min_speed_for_dir: 低風速時の風向を NA にする閾値（任意）
lowpass_wind_dir <- function(dir_deg, fs, cutoff, order = 4,
                             weight = NULL, min_speed_for_dir = NULL) {
  n <- length(dir_deg)
  theta <- dir_deg * pi/180  # 度→ラジアン（気象式のまま扱ってOK）

  # 単位ベクトル（円上）
  # ※重み付けする場合はここで掛ける（例: weight = 風速）
  w <- if (is.null(weight)) rep(1, n) else weight
  x <- cos(theta) * w
  y <- sin(theta) * w

  # バターワースLPF（ゼロ位相）
  bf <- butter(order, cutoff/(fs/2), type = "low", plane = "z")
  x_f <- filtfilt(bf, x)
  y_f <- filtfilt(bf, y)

  # 正規化（方向だけ欲しいので大きさは1に戻す）
  mag <- sqrt(x_f^2 + y_f^2)
  # 数値安定化
  mag[mag == 0] <- 1e-12
  x_n <- x_f / mag
  y_n <- y_f / mag

  # 角度に戻す（0–360°）
  dir_rad <- atan2(y_n, x_n)
  dir_out <- (dir_rad * 180/pi) %% 360

  # 低風速の扱い（任意）
  if (!is.null(min_speed_for_dir) && !is.null(weight)) {
    dir_out[weight < min_speed_for_dir] <- NA
  }

  return(dir_out)
}

fs <- 10       # 10Hzで記録
cutoff <- 0.4  # 1Hz化の前処理なら0.3–0.4Hzが安全
data_wind$winddir_filtered <- lowpass_wind_dir(data_wind$winddir, fs = fs, cutoff = cutoff,
                                 order = 4,
                                 weight = NULL,        # 風速で重み付け
                                 min_speed_for_dir = 0.5)   # 低風速はNA
