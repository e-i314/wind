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


##########　図5
ggplot(data = data_wind, aes(x = timestamp, y = windspd)) +
  geom_line(size = 0.2, na.rm = FALSE) +  # NAはそのまま残す（線を切る）
  labs(
    title = "Time Series of Wind Speed",
    x = "Time (hh:mm)",
    y = "Wind Speed (m/s)"
  ) 

##########　図5
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

##########　図6
ggplot(data_wind[1000:1500,]) +
  geom_line(aes(x = timestamp, y = windspd, color = "Raw")) +
  geom_line(aes(x = timestamp, y = windspd_filtered, color = "Filtered")) +
  labs(
    title = "Effect of Low-pass Filter on Wind Speed",
    x = "Time (sec)",
    y = "Wind Speed (m/s)",
    color = "Legend"
  ) +
  scale_color_manual(values = c("Raw" = "black", "Filtered" = "blue"))+
  theme(legend.title = element_blank())


data_wind_1sec <- data_wind %>%
  mutate(
    rounded_timestamp = as.POSIXct(round(as.numeric(timestamp)), origin = "1970-01-01")
  ) %>%
  group_by(rounded_timestamp) %>%
  slice_min(abs(as.numeric(timestamp) - as.numeric(rounded_timestamp))) %>%
  ungroup()

# 角度リスト
# 角度リスト
angles <- c(0,10,20, 30,40,50, 60,70,80, 90, 100,110,120,130,140, 150,160,170)
angles2 <- c(0,10,20, 30,40,50, 60,70,80, 90, 100,110,120,130,140, 150,160,170,999)

# 各角度ごとの風速成分を計算
# 各角度ごとの風速成分を計算
for (angle in angles2) {
  data_wind_1sec[[paste0("wind_", angle)]] <-data_wind_1sec$windspd * cos((data_wind_1sec$winddir - angle) * pi / 180)
}

for (angle in angles2) {
  data_wind_1sec[[paste0("windfiltered_", angle)]] <-data_wind_1sec$windspd_filtered * cos((data_wind_1sec$winddir_filtered - angle) * pi / 180)
}


# Filterされていないデータへの処理
data_wind_1sec[[paste0("wind_", 999)]] <-data_wind_1sec$windspd 
# 各角度ごとの風速の時間変化率を計算
for (angle in angles2) {
  col_name <- paste0("wind_", angle) # 元の列名
  diff_col_name <- paste0("d_wind_", angle) # 差分の列名

  # 差分計算 (1つ前の行との差)
  data_wind_1sec[[diff_col_name]] <- c(NA, diff(data_wind_1sec[[col_name]])) / c(NA, diff(as.numeric(data_wind_1sec$timestamp)))

}


###########################filtered dataで同じ処理を行う###################################
data_wind_1sec[[paste0("windfiltered_", 999)]] <-data_wind_1sec$windspd_filtered
# 各角度ごとの風速の時間変化率を計算
for (angle in angles2) {
  col_name <- paste0("windfiltered_", angle) # 元の列名
  diff_col_name <- paste0("d_windfiltered_", angle) # 差分の列名
  print(c(col_name,diff_col_name))

  # 差分計算 (1つ前の行との差)
  data_wind_1sec[[diff_col_name]] <- c(NA, diff(data_wind_1sec[[col_name]])) / c(NA, diff(as.numeric(data_wind_1sec$timestamp)))
}


# 各 d_wind_角度 の移動平均を計算
# Filterされていないデータへの処理
for (angle in angles2) {
  diff_col_name <- paste0("d_wind_", angle)  # 変化量の列名
  avg_col_name <- paste0("b_wind_", angle)   # 移動平均の列名
  
  # 移動平均の計算 
  data_wind_1sec[[avg_col_name]] <- zoo::rollapply(
    abs(data_wind_1sec[[diff_col_name]]), # 絶対値を取る
    width = 120,           # Window幅を設定
    FUN = mean,           # 平均を計算
    align = "center",     # 中央に値を配置
    fill = NA             # 計算できない場合はNAを挿入
  )
}


# 各 d_windfiltered_角度 の移動平均を計算
###########################filtered dataで同じ処理を行う###################################
for (angle in angles2) {
  diff_col_name <- paste0("d_windfiltered_", angle)  # 変化量の列名
  avg_col_name <- paste0("b_windfiltered_", angle)   # 移動平均の列名
  
  # 移動平均の計算 
  data_wind_1sec[[avg_col_name]] <- zoo::rollapply(
    abs(data_wind_1sec[[diff_col_name]]), # 絶対値を取る
    width = 120,           # Window幅を設定
    FUN = mean,           # 平均を計算
    align = "center",     # 中央に値を配置
    fill = NA             # 計算できない場合はNAを挿入
  )
}



# timestamp を POSIXct に変換（もし未変換なら）
data_wind_1sec <- data_wind_1sec %>%
  mutate(timestamp = as.POSIXct(timestamp, format="%Y-%m-%d %H:%M:%OS"))

data_wind_1sec <- data_wind_1sec %>%
  rowwise() %>%
  mutate(b_wind_median = median(c_across(starts_with("b_wind_")), na.rm = TRUE),
         b_windfiltered_median = median(c_across(starts_with("b_windfiltered_")), na.rm = TRUE)) %>%
  ungroup()

# データを long 形式に変換　分布を確認するために
data_long_1sec_distrubution <- data_wind_1sec %>%
  select(timestamp, starts_with("d_wind_")) %>%
  pivot_longer(cols = -timestamp, names_to = "angle", values_to = "value")


df <- data_long_1sec_distrubution %>%
  mutate(
    angle_chr = as.character(angle),
    angle_num = as.numeric(gsub("d_wind_", "", angle_chr))
  )

lvls <- unique(df$angle_chr[order(df$angle_num)])  # 一意なレベルを作成

data_long_1sec_distrubution <- df %>%
  mutate(angle = factor(angle_chr, levels = lvls)) %>%
  select(-angle_chr, -angle_num)


###########################filtered dataで同じ処理を行う###################################

data_long_1sec_filtered_distrubution <- data_wind_1sec %>%
  select(timestamp, starts_with("d_windfiltered_")) %>%
  pivot_longer(cols = -timestamp, names_to = "angle", values_to = "value")


df <- data_long_1sec_filtered_distrubution %>%
  mutate(
    angle_chr = as.character(angle),
    angle_num = as.numeric(gsub("d_windfiltered_", "", angle_chr))
  )

lvls <- unique(df$angle_chr[order(df$angle_num)])  # 一意なレベルを作成

data_long_1sec_filtered_distrubution <- df %>%
  mutate(angle = factor(angle_chr, levels = lvls)) %>%
  select(-angle_chr, -angle_num)

##########　図7
ggplot(data_long_1sec_filtered_distrubution, aes(x = value)) +
  geom_histogram(bins = 30, fill = "cornsilk", color = "grey50", alpha = 0.7) +
  facet_wrap(~ angle, scales = "free_x") +  # 各角度ごとのFacet
  scale_y_log10() +  # 縦軸をログスケール
  xlim(-5,5)+
  labs(title = "Histogram of Filtered Wind Speed Change Rate by Angle",
       x = "Wind Speed Change Rate (m/s^2)",
       y = "Count (log scale)") 

data_long_1sec <- data_wind_1sec %>%
  select(timestamp, starts_with("b_wind_")) %>%
  pivot_longer(cols = -timestamp, names_to = "angle", values_to = "value")

data_long_1sec$angle <- factor(
  data_long_1sec$angle,
  levels = paste0("b_wind_", c(seq(0,170,10)))  # 順序を手動で指定
)


# 凡例の順序（maxを最後に追加）
levels_order <- c(paste0("b_wind_", seq(0,170,10)), "b_wind_999")
data_long_1sec$angle <- factor(data_long_1sec$angle, levels = levels_order)
data_long_1sec <- data_long_1sec[!is.na(data_long_1sec$angle),]
data_long_1sec <- data_long_1sec[!is.na(data_long_1sec$value),]
# 赤→黄→緑→青 のグラデーション色
gradient_colors <- colorRampPalette(c("red", "yellow", "green", "blue"))(length(levels_order) - 1)

# b_wind_max は黒にする

##########　図8
ggplot(data_long_1sec[2000:20000,], aes(x = timestamp, y = value, color = angle)) +
  geom_line() +
  scale_color_manual(values = gradient_colors) +
  labs(title = "120sec Moving Average of Wind Speed Change Rate",
       x = "Time",
       y = "Wind Speed Change Rate (m/s^2)",
       color = "Wind Angle") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# 相関係数と回帰式を計算
# data_long_1secを再定義する
data_long_1sec <- data_wind_1sec %>%
  select(timestamp, starts_with("b_wind_")) %>%
  pivot_longer(cols = -timestamp, names_to = "angle", values_to = "value")

data_long_1sec$angle <- factor(
  data_long_1sec$angle,
  levels = c( paste0("b_wind_", c(seq(0,170,10))) ,"b_wind_999" ) # 順序を手動で指定
)


# 凡例の順序（maxを最後に追加）
levels_order <- c(paste0("b_wind_", seq(0,170,10)), "b_wind_999")
data_long_1sec$angle <- factor(data_long_1sec$angle, levels = levels_order)
data_long_1sec <- data_long_1sec[!is.na(data_long_1sec$angle),]
data_long_1sec <- data_long_1sec[!is.na(data_long_1sec$value),]


# 1) 横持ちに
df_wide <- data_long_1sec %>%
  mutate(angle = as.character(angle)) %>%   # factorの影響を避ける
  pivot_wider(names_from = angle, values_from = value)

# 2) b_wind_999は列のまま残し、他だけ縦持ちに
df_scatter <- df_wide %>%
  pivot_longer(
    cols = starts_with("b_wind_") & -all_of("b_wind_999"),
    names_to = "angle", values_to = "val"
  ) %>%
  tidyr::drop_na(b_wind_999, val)  # 両方ある行だけ残す

# 3) 角度の並びを数値昇順に（999は自然に最後へ）
lvl <- df_scatter %>%
  distinct(angle) %>%
  mutate(num = as.numeric(sub("b_wind_", "", angle))) %>%
  arrange(num) %>% pull(angle)
df_scatter <- df_scatter %>% mutate(angle = factor(angle, levels = lvl))

label_df <- df_scatter %>%
  group_by(angle) %>%
  summarise(
    r = cor(b_wind_999, val, use = "complete.obs"),
    fit = list(lm(val ~ b_wind_999)),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    eq = {
      coefs <- coef(fit)
      sprintf("y = %.3f x %+ .3f", coefs[2], coefs[1])
    },
    label = paste0(eq, "\n r = ", sprintf("%.3f", r))
  ) %>%
  select(angle, label)

# ラベルの表示位置を適当に決める（例: x=最小, y=最大付近）
pos_df <- df_scatter %>%
  group_by(angle) %>%
  summarise(
    xpos = min(`b_wind_999`, na.rm=TRUE),
    ypos = max(val, na.rm=TRUE),
    .groups = "drop"
  )

label_df <- left_join(label_df, pos_df, by="angle")

# プロット
ggplot(df_scatter, aes(x = `b_wind_999`, y = val)) +
  geom_point(size = .1, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 0.3) +
  geom_text(data = label_df, aes(x = xpos, y = ypos, label = label),
            inherit.aes = FALSE, hjust = 0, vjust = 1, size = 3) +
  facet_wrap(~ angle, ncol = 6) +
  labs(title = "b_wind_999 vs each angle",
       x = "b_wind_999", y = "value") +
  theme_minimal() 



###########################filtered dataで同じ処理を行う###################################

data_long_filtered_1sec <- data_wind_1sec %>%
  select(timestamp, starts_with("b_windfiltered_")) %>%
  pivot_longer(cols = -timestamp, names_to = "angle", values_to = "value")

data_long_filtered_1sec$angle <- factor(
  data_long_filtered_1sec$angle,
  levels = c( paste0("b_windfiltered_", c(seq(0,170,10))) ,"b_windfiltered_999" ) # 順序を手動で指定
)


# 凡例の順序（maxを最後に追加）
levels_order <- c(paste0("b_windfiltered_", seq(0,170,10)), "b_windfiltered_999")
data_long_filtered_1sec$angle <- factor(data_long_filtered_1sec$angle, levels = levels_order)
data_long_filtered_1sec <- data_long_filtered_1sec[!is.na(data_long_filtered_1sec$angle),]
data_long_filtered_1sec <- data_long_filtered_1sec[!is.na(data_long_filtered_1sec$value),]


# 1) 横持ちに
df_wide <- data_long_filtered_1sec %>%
  mutate(angle = as.character(angle)) %>%   # factorの影響を避ける
  pivot_wider(names_from = angle, values_from = value)

# 2) b_windfiltered_999は列のまま残し、他だけ縦持ちに
df_scatter <- df_wide %>%
  pivot_longer(
    cols = starts_with("b_windfiltered_") & -all_of("b_windfiltered_999"),
    names_to = "angle", values_to = "val"
  ) %>%
  tidyr::drop_na(b_windfiltered_999, val)  # 両方ある行だけ残す

# 3) 角度の並びを数値昇順に（999は自然に最後へ）
lvl <- df_scatter %>%
  distinct(angle) %>%
  mutate(num = as.numeric(sub("b_windfiltered_", "", angle))) %>%
  arrange(num) %>% pull(angle)
df_scatter <- df_scatter %>% mutate(angle = factor(angle, levels = lvl))

label_df <- df_scatter %>%
  group_by(angle) %>%
  summarise(
    r = cor(b_windfiltered_999, val, use = "complete.obs"),
    fit = list(lm(val ~ b_windfiltered_999)),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    eq = {
      coefs <- coef(fit)
      sprintf("y = %.3f x %+ .3f", coefs[2], coefs[1])
    },
    label = paste0(eq, "\n r = ", sprintf("%.3f", r))
  ) %>%
  select(angle, label)

# ラベルの表示位置を適当に決める（例: x=最小, y=最大付近）
pos_df <- df_scatter %>%
  group_by(angle) %>%
  summarise(
    xpos = min(`b_windfiltered_999`, na.rm=TRUE),
    ypos = max(val, na.rm=TRUE),
    .groups = "drop"
  )

label_df <- left_join(label_df, pos_df, by="angle")

############ 図11
ggplot(df_scatter, aes(x = `b_windfiltered_999`, y = val)) +
  geom_point(size = .1, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 0.3) +
  geom_text(data = label_df, aes(x = xpos, y = ypos, label = label),
            inherit.aes = FALSE, hjust = 0, vjust = 1, size = 3) +
  facet_wrap(~ angle, ncol = 6) +
  labs(title = "b_windfiltered_999 vs each angle",
       x = "b_windfiltered_999", y = "value") +
  theme_minimal() 

# 1) 横持ちに：timestamp ごとに b_wind_* を列化
wide <- data_long_1sec %>%
  mutate(angle = as.character(angle)) %>%
  pivot_wider(
    id_cols = timestamp,
    names_from = angle,
    values_from = value,
    # 同一 timestamp・angle に複数行がある場合は平均でまとめる（任意）
    values_fn = list(value = ~mean(.x, na.rm = TRUE))
  )

# 2) 999 を除いた b_wind_* の中央値（縦軸）を作る
cols_other <- grep("^b_wind_\\d+$", names(wide), value = TRUE)
cols_other <- setdiff(cols_other, "b_wind_999")

df_median <- wide %>%
  rowwise() %>%
  mutate(y_median = median(c_across(all_of(cols_other)), na.rm = TRUE)) %>%
  ungroup() %>%
  transmute(b_wind_999 = `b_wind_999`, y_median) %>%
  tidyr::drop_na(b_wind_999, y_median)

# 3) 回帰式と相関係数 r を計算
fit <- lm(y_median ~ b_wind_999, data = df_median)
coefs <- coef(fit)
r_val <- cor(df_median$b_wind_999, df_median$y_median, use = "complete.obs")

label <- sprintf("y = %.3f x %+ .3f\nr = %.3f", coefs[2], coefs[1], r_val)

# ラベル位置（左上）
xpos <- min(df_median$b_wind_999, na.rm = TRUE)
ypos <- max(df_median$y_median, na.rm = TRUE)

# 4) 散布図＋回帰直線＋テキスト
ggplot(df_median, aes(x = b_wind_999, y = y_median)) +
  geom_point(size = .5, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 1) +
  annotate("text", x = xpos, y = ypos, label = label,
           hjust = 0, vjust = 1, size = 4) +
  labs(title = "Median(b_wind_0..170) vs b_wind_999 (per timestamp)",
       x = "b_wind_999", y = "median of b_wind_0..b_wind_170") +
  xlim(0,1)+
  ylim(0,1.25)



###########################filtered dataで同じ処理を行う###################################

# 1) 横持ちに：timestamp ごとに b_wind_* を列化
wide <- data_long_filtered_1sec %>%
  mutate(angle = as.character(angle)) %>%
  pivot_wider(
    id_cols = timestamp,
    names_from = angle,
    values_from = value,
    # 同一 timestamp・angle に複数行がある場合は平均でまとめる（任意）
    values_fn = list(value = ~mean(.x, na.rm = TRUE))
  )

# 2) 999 を除いた b_wind_* の中央値（縦軸）を作る
cols_other <- grep("^b_windfiltered_\\d+$", names(wide), value = TRUE)
cols_other <- setdiff(cols_other, "b_windfiltered_999")

df_median <- wide %>%
  rowwise() %>%
  mutate(y_median = median(c_across(all_of(cols_other)), na.rm = TRUE)) %>%
  ungroup() %>%
  transmute(b_windfiltered_999 = `b_windfiltered_999`, y_median) %>%
  tidyr::drop_na(b_windfiltered_999, y_median)

# 3) 回帰式と相関係数 r を計算
fit <- lm(y_median ~ b_windfiltered_999, data = df_median)
coefs <- coef(fit)
r_val <- cor(df_median$b_windfiltered_999, df_median$y_median, use = "complete.obs")

label <- sprintf("y = %.3f x %+ .3f\nr = %.3f", coefs[2], coefs[1], r_val)

# ラベル位置（左上）
xpos <- min(df_median$b_windfiltered_999, na.rm = TRUE)
ypos <- max(df_median$y_median, na.rm = TRUE)

##########　図12
# 4) 散布図＋回帰直線＋テキスト
ggplot(df_median, aes(x = b_windfiltered_999, y = y_median)) +
  geom_point(size = .5, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 1) +
  annotate("text", x = xpos, y = ypos, label = label,
           hjust = 0, vjust = 1, size = 4) +
  labs(title = "Median(b_windfiltered_0..170) vs b_windfiltered_999 (per timestamp)",
       x = "b_windfiltered_999", y = "median of b_wind_0..b_wind_170") +
  xlim(0,1)+
  ylim(0,1.25)


# 1) 横持ち化
wide <- data_long_1sec[2000:20000, ] %>%
  mutate(angle = as.character(angle)) %>%
  pivot_wider(names_from = angle, values_from = value)

# 2) b_wind_0..170 の max と min を計算
cols_other <- grep("^b_wind_\\d+$", names(wide), value = TRUE)
cols_other <- setdiff(cols_other, "b_wind_999")

df_comp <- wide %>%
  rowwise() %>%
  mutate(
    max_val = max(c_across(all_of(cols_other)), na.rm = TRUE),
    min_val = min(c_across(all_of(cols_other)), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  transmute(
    timestamp,
    max_val,
    min_val,
    pred_val = 1.124 * `b_wind_999` + 0.034
  ) %>%
  tidyr::drop_na(max_val, min_val, pred_val)

# 3) ロング形式にしてまとめてプロット
df_long <- df_comp %>%
  pivot_longer(cols = c("max_val", "min_val", "pred_val"),
               names_to = "type", values_to = "value")

# 4) 時系列プロット
ggplot(df_long, aes(x = timestamp, y = value, color = type)) +
  geom_line() +
  labs(title = "Max/Min and Regression Prediction",
       x = "Time",
       y = "Wind Speed Change Rate (m/s^2)",
       color = "") +
  scale_color_manual(values = c(
    "max_val" = "red",
    "min_val" = "blue",
    "pred_val" = "black"
  )) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))+
  ylim(0,.7)




wide <- data_long_filtered_1sec[2000:20000, ] %>%
  mutate(angle = as.character(angle)) %>%
  pivot_wider(names_from = angle, values_from = value)

# 2) b_wind_0..170 の max と min を計算
cols_other <- grep("^b_windfiltered_\\d+$", names(wide), value = TRUE)
cols_other <- setdiff(cols_other, "b_windfiltered_999")

df_comp <- wide %>%
  rowwise() %>%
  mutate(
    max_val = max(c_across(all_of(cols_other)), na.rm = TRUE),
    min_val = min(c_across(all_of(cols_other)), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  transmute(
    timestamp,
    max_val,
    min_val,
    pred_val = 1.036 * `b_windfiltered_999` + 0.028
  ) %>%
  tidyr::drop_na(max_val, min_val, pred_val)

# 3) ロング形式にしてまとめてプロット
df_long <- df_comp %>%
  pivot_longer(cols = c("max_val", "min_val", "pred_val"),
               names_to = "type", values_to = "value")
##########　図13
# 4) 時系列プロット
ggplot(df_long, aes(x = timestamp, y = value, color = type)) +
  geom_line() +
  labs(title = "Max/Min and Regression Prediction Filtered Data",
       x = "Time",
       y = "Wind Speed Change Rate (m/s^2)",
       color = "")  +
  scale_color_manual(values = c(
    "max_val" = "red",
    "min_val" = "blue",
    "pred_val" = "black"
  )) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))+
  ylim(0,.7)
