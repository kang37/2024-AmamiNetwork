# 计算每个用户相连两个轨迹点之间的时间差。
library(dplyr)
library(lubridate)

data_raw = agoop_amami %>% st_drop_geometry()

# 1. 数据预处理：确保时间列是 datetime 对象
data_processed <- data_raw %>%
  # 转换 time 列为标准的 POSIXct/datetime 格式
  # 确保格式与你的数据匹配，这里假设是 "YYYY-MM-DD HH:MM:SS"
  mutate(time = ymd_hms(time))

# 2. 计算每个 dailyid 的时间差
time_difference_data <- data_processed %>%
  # 按每个个体的每日ID分组
  group_by(dailyid) %>%
  # 确保轨迹点按时间正确排序（这是关键步骤）
  arrange(time) %>%
  # 使用 mutate 和 difftime 计算时间差
  mutate(
    # lag(time) 获取上一个点的时间
    time_prev = lag(time),

    # 计算当前时间点与上一个时间点的时间差
    # units="mins" 指定以分钟为单位显示
    time_diff_mins = as.numeric(difftime(time, time_prev, units = "mins"))
  ) %>%
  ungroup() # 取消分组

quantile(time_difference_data$time_diff_mins, na.rm = T)


# 查看结果
# 第一个点的时间差会是 NA，因为没有上一个点
print(head(time_difference_data))
