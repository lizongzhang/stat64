library(tidyverse)

set.seed(123)

# Define student counts
num_students <- 120
num_female <- 45
num_male <- 75

# Define groups
study_groups <- c("self-study", "group-study", "online course")

# Assign genders
gender <- c(rep("Female", num_female), rep("Male", num_male))

# Assign study methods
study_method <- c(
  rep("self-study", 15), rep("group-study", 15), rep("online course", 10),  # Females
  rep("self-study", 25), rep("group-study", 25), rep("online course", 30)   # Males
)


class <- c(rep("C", 48), rep("B", 36), rep("A", 24), rep("D", 12))



# Generate Math scores based on group
math_score <- c(
  round(rnorm(40, mean = 80, sd = 2)),
  round(rnorm(40, mean = 88, sd = 2)),
  round(rnorm(40, mean = 60, sd = 2))
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
  study_method = study_method,
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

table(student_data$study_method, student_data$gender)





chisq.test(table(student_data$study_method, student_data$gender))

table(student_data$study_method, student_data$gender) %>% 
  prop.table(margin = 1)

aov(student_data$math_score ~ student_data$study_method) %>% 
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


student_data$math_score[student_data$gender == "Female" &
                        study_method == "self-study"] %>% 
  arrange()

stem <- student_data %>% 
  filter(gender == "Female" & study_method == "self-study") %>%
  arrange(math_score) %>% 
  select(math_score) %>% 
  t()

summary(student_data$math_score)

cov(student_data$math_score, student_data$study_hours)
cor(student_data$math_score, student_data$study_hours)

13.9/(12*2)

install.packages("writexl")
library(writexl)

# 导出
write_xlsx(student_data, "student.xlsx")
  
