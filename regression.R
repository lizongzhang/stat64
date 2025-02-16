# 设置随机种子，确保结果可重复
set.seed(42)

# 模拟自变量 Working_hours，范围为100到200小时（生成整数）
Working_hours <- sample(100:200, 100, replace = TRUE)

# 增强回归关系：减少误差的影响，增加自变量对因变量的影响
# 假设回归模型为： Monthly_salary = 2 * Working_hours + 1000 + 误差项
# 误差项服从正态分布，生成整数误差项
error <- round(rnorm(100, mean = 0, sd = 30))

# 计算因变量 Monthly_salary，结果四舍五入为整数
Monthly_salary <- round(3 * Working_hours + 3000 + error)

# 创建数据框
data <- data.frame(Working_hours, Monthly_salary)

# 查看前几行数据
head(data)

# 计算回归模型
model <- lm(Monthly_salary ~ Working_hours, data = data)

# 输出回归结果和判定系数 R-squared
summary(model)

# 导出数据到CSV文件
write.csv(data, "employee_salary_data.csv", row.names = FALSE)


# 设置随机种子，确保结果可重复
set.seed(42)

# 模拟自变量 Working_hours，范围为100到200小时（生成整数）
Working_hours <- sample(100:200, 100, replace = TRUE)

# 假设回归模型为： Health_index = 100 - 0.3 * Working_hours + 误差项
# 误差项服从正态分布，生成整数误差项
error <- round(rnorm(100, mean = 0, sd = 5))

# 计算因变量 Health_index，结果四舍五入为整数
Health_index <- round(100 - 0.3 * Working_hours + error)

# 创建数据框
data <- data.frame(Working_hours, Health_index)

# 查看前几行数据
head(data)

# 计算回归模型
model <- lm(Health_index ~ Working_hours, data = data)

# 输出回归结果和判定系数 R-squared
summary(model)

# 导出数据到CSV文件
write.csv(data, "employee_health_data.csv", row.names = FALSE)


# smoothing ---------------------------------------------------------------

# 设置随机种子以确保结果可重复
set.seed(123)

# 生成30期的访问量数据（假设基础访问量为1000，标准差为50的正态分布波动）
n <- 30
base_visitors <- 1000  # 基础日访问量
visitors_data <- round(base_visitors + rnorm(n, mean = 0, sd = 50),0)  # 网站访问量数据（低波动）

# 创建一个时间序列对象
visitors_ts <- ts(visitors_data, frequency = 1)

# 绘制模拟的访问量时间序列图
plot.ts(visitors_ts, main = "网站每日访问量（低波动模拟数据）", ylab = "访问量（人次）", xlab = "日期")

#纵轴调整为0到1500
plot.ts(visitors_ts, 
        main = "网站每日访问量（低波动模拟数据）", 
        ylab = "访问量（人次）", xlab = "日期", ylim = c(0, 1500))

#美化上述图形
plot.ts(visitors_ts, 
        main = "网站每日访问量（低波动模拟数据）", 
        ylab = "访问量（人次）", 
        xlab = "日期", 
        ylim = c(0, 1500), col = "blue", lwd = 2)

# 创建一个数据框来存储日期和访问量数据
date_seq <- seq.Date(from = as.Date("2024-01-01"), by = "day", length.out = n)  # 假设从2024年1月1日开始
visitors_df <- data.frame(
  Date = date_seq,  # 日期
  Visitors = visitors_data  # 访问量数据
)

# 将数据导出为 Excel 文件
write.csv(visitors_df, "website_traffic_data.csv")

