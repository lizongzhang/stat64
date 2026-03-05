library(tidyverse)

set.seed(123)

# Define student counts
num_students <- 120
num_female <- 45
num_male <- 75

# Define groups
study_groups <- c("self-study", "group study", "online learning")

# Assign genders
gender <- c(rep("Female", num_female), rep("Male", num_male))

# Assign study methods
study_method <- c(
  rep("self-study", 15), rep("group study", 15), rep("online learning", 10),  # Females
  rep("self-study", 25), rep("group study", 25), rep("online learning", 30)   # Males
)


class <- c(rep("C", 48), rep("B", 36), rep("A", 24), rep("D", 12))



# Generate Math scores based on group
math_score <- c(
  round(rnorm(40, mean = 80, sd = 2)),
  round(rnorm(50, mean = 88, sd = 2)),
  round(rnorm(30, mean = 60, sd = 2))
)

# Generate study hours based on group
study_hours <- c(
  round(rnorm(40, mean = 15, sd = 1)),
  round(rnorm(40, mean = 13, sd = 1)),
  round(rnorm(40, mean = 11, sd = 1))
)

# Combine data into a dataframe
student_data <- data.frame(
  gender = gender,
  study_mode = study_method,
  math_score = math_score,
  study_hours = study_hours,
  class = class
)

# Display the first few rows of the dataframe
head(student_data)

# View distribution checks
with(student_data, table(gender, study_method))


summary(student_data)

t.test(student_data$math_score[student_data$gender == "Female"],
       student_data$math_score[student_data$gender == "Male"],
       var.equlal = TRUE)


t.test(student_data$study_hours[student_data$gender == "Female"],
       student_data$study_hours[student_data$gender == "Male"],
       var.equlal = TRUE)

var.test(student_data$study_hours[student_data$gender == "Female"],
         student_data$study_hours[student_data$gender == "Male"])




chisq.test(table(student_data$study_mode, student_data$gender))

table(student_data$study_mode, student_data$gender) %>% 
  prop.table(margin = 1)

aov(student_data$math_score ~ student_data$study_mode) %>% 
  summary()

library(psych)

describe(student_data)

table(student_data$class)

# 设定分组区间
breaks <- seq(50, 100, by = 10)
score_groups <- cut(student_data$math_score, breaks = breaks, right = FALSE)

# 计算每组的频数
freq_group_table <- table(score_groups)
print(freq_group_table)

summary(student_data$math_score)

describe(student_data$math_score)

describe(student_data$study_hours)

table(student_data$class)



student_data$math_score[student_data$gender == "Female" &
                        study_method == "self-study"]

student_data %>% 
  filter(gender == "Female" & study_mode == "group study") %>%
  arrange(math_score) %>% 
  pull(math_score) %>% 
  stem()

stem
  
  

summary(student_data$math_score)

cov(student_data$math_score, student_data$study_hours)

cor(student_data$math_score, student_data$study_hours)

13.9/(12*2)

#install.packages("writexl")
library(writexl)

# 导出
write_xlsx(student_data, "student.xlsx")



# final B -----------------------------------------------------------------

# 加载必要包
library(dplyr)

set.seed(233)  # 保证结果可重复

# 人数设置
n <- 150


# 生成分组变量
track <- rep(c("Science", "Humanities", "Arts", "Sports", "Science", "Humanities", "Arts", "Sports"), 
             c(15, 30, 10, 5, 50, 20, 10, 10) )

# 性别（60 female, 90 male）
gender <- c(rep("Female", 60), rep("Male", 90))

# 阅读方式（每种50人）
reading_mode <- rep(c("Paper books", "E-books", "Audio books", "Paper books", "E-books", "Audio books"), 
                    c(10, 30, 20, 20, 50, 20))

# 按阅读方式生成不同均值的词汇得分
vocab_score <- c(
  round(rnorm(50, mean = 60, sd = 8)),   # Paper books
  round(rnorm(50, mean = 55, sd = 8)),   # E-books
  round(rnorm(50, mean = 50, sd = 8))    # Audio books
)

# 阅读时长，假设近似正态分布，均值12小时，SD=2
reading_hours <- round(rnorm(150, mean = 12, sd = 2))

# 构建数据框
df <- data.frame(
  track = track,
  gender = gender,
  reading_mode = reading_mode,
  vocab_score = vocab_score,
  reading_hours = reading_hours
)

# 查看数据结构
str(df)

# vocab_score的频数分布表
table(df$vocab_score)

# 以5分为一个区间，统计每个区间的频数
vocab_score_groups <- cut(df$vocab_score, breaks = seq(40, 80, by = 5), right = FALSE)
freq_table <- table(vocab_score_groups)
# 查看频数分布
print(freq_table)


# 导出

write_xlsx(df, "finalb.xlsx")


describe(df)

table(df$gender, df$reading_mode)

# 进行方差分析
aov_result <- aov(vocab_score ~ reading_mode, data = df)
summary(aov_result)

# 比较男生和女生的词汇分数的均值
t.test(df$vocab_score[gender == "Male"], df$vocab_score[gender == "Female"], 
       var.equal = TRUE)

var.test(df$vocab_score[gender == "Male"], df$vocab_score[gender == "Female"])


# 进行独立性检验
chisq_result <- chisq.test(table(df$reading_mode, df$gender))
chisq_result

df %>% 
  filter(gender == "Male", reading_mode == "Paper books") %>% 
  select(vocab_score) %>%
  arrange(vocab_score) %>%
  pull() %>% 
  toString()

summary(df$vocab_score)

df %>% 
  select(vocab_score, reading_hours) %>%
  describe()

# 计算协方差和相关系数
cov(df$vocab_score, df$reading_hours)



# keys --------------------------------------------------------------------

# 安装并加载 qcc 包
install.packages("qcc")     # 如未安装
library(qcc)

# 示例数据：各类缺陷的频数
defects <- c(A = 24, B = 36, C = 48, D = 12)

# 绘制帕雷托图
pareto.chart(defects,
             main = "Pareto Chart of Class Frequencies",
             ylab = "Frequency")


# 数据准备
freq <- c(C = 48, B = 36, A = 24, D = 12)
sorted_freq <- sort(freq, decreasing = TRUE)
cum_pct <- cumsum(sorted_freq) / sum(sorted_freq) * 100

# 基础条形图
bp <- barplot(sorted_freq,
              col = "lightblue",
              main = "Pareto Chart",
              ylab = "Frequency")

# 添加累计百分比线
lines(x = bp, y = cum_pct, type = "b", col = "red", pch = 19)

# 添加次要纵轴
axis(side = 4, at = seq(0, 100, 20), 
     labels = paste0(seq(0, 100, 20), "%"), 
     col.axis = "red")
mtext("Cumulative Percentage", side = 4, line = 3, col = "red")


# 安装并加载 qcc 包（如尚未安装）
if (!require(qcc)) install.packages("qcc")
library(qcc)

# 设置绘图边距，右边距加大避免文字被遮挡
par(mar = c(5, 4, 4, 6), xpd = TRUE, las = 1)

# 示例数据
defects <- c(C = 48, B = 36, A = 24, D = 12)

# 绘制帕雷托图
pareto.chart(defects,
             col = "lightblue",
             main = "Pareto Chart of Class Frequencies",
             ylab = "Frequency",
             las = 2)


# 数据准备
freq <- c(C = 48, B = 36, A = 24, D = 12)
sorted_freq <- sort(freq, decreasing = TRUE)
cum_pct <- cumsum(sorted_freq) / sum(sorted_freq) * 100

# 设置边距与坐标标签方向
par(mar = c(6, 4, 4, 6) + 0.1)

# 绘制条形图，设置 ylim 高一点给累计线留空间
bp <- barplot(sorted_freq,
              col = "lightblue",
              ylim = c(0, max(sorted_freq) * 1.3),
              ylab = "Frequency",
              main = "Pareto Chart of Class Frequencies",
              names.arg = names(sorted_freq),
              las = 2,         # 垂直X轴标签
              cex.names = 1.2  # 字体大小
)

# 添加累计百分比折线图
par(new = TRUE)  # 叠加第二层图形
plot(bp, cum_pct,
     type = "b", pch = 19, axes = FALSE, xlab = "", ylab = "",
     ylim = c(0, 100))  # 用百分比为坐标轴

# 添加右侧Y轴作为累计百分比
axis(4, at = seq(0, 100, 25), labels = paste0(seq(0, 100, 25), "%"))
mtext("Cumulative Percentage", side = 4, line = 3)


boxplot(student_data$math_score, 
        horizontal = TRUE
        )

boxplot(student_data$math_score,
        horizontal = TRUE,
        xlim = c(50, 100), 
        col = "white",
        main = "Math Score Boxplot",
        xlab = "Score")

# 添加文本标注（五数值）
box_stats <- boxplot.stats(student_data$math_score)$stats
text(box_stats, rep(1, 5), 
     labels = round(box_stats, 1), 
     pos = 1, cex = 0.8, col = "blue")

library(ggplot2)

# 获取五数数据
five_num <- c(54, 73, 81, 88, 93)

# 创建箱线图
ggplot(student_data, aes(x = math_score, y = "")) +
  geom_boxplot(fill = "white", width = 0.3) +
  scale_x_continuous(limits = c(50, 100)) +
  labs(title = "Math Score Boxplot with Five-Number Summary", x = "Score", y = "") +
  theme_minimal(base_size = 14) +
  # 添加注释文本
  annotate("text", x = five_num, y = rep(0.8, 5), 
           label = round(five_num, 1),
           size = 3.5, color = "blue")



