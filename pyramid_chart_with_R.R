library(readxl)
library(tidyverse)
library(extrafont) # For Mac
library(ggthemes)

# For Mac
font_import()
theme_set(theme_gray(base_family='NanumGothic'))

# Data Import
data <- read_excel("population_data.xlsx")
head(data)

# Colnames by Eng.
colnames(data) <- c('Age', 'Total', 'Male', 'Female')
col_desc <- data$Age
data$Age <- factor(data$Age, levels = col_desc)

# Data Preprocess
data$Male <- data$Male * -1


brks <- seq(-3000000, 3000000, 1000000)
lbls <- paste0(as.character(c(seq(300, 0, -100), seq(100, 300, 100))), "만")

ggplot(data = data) +
    geom_bar(aes(x = Age, y = Male), stat = "identity", fill = '#619CFF', alpha = 0.7) +
    geom_bar(aes(x = Age, y = Female), stat = "identity", fill = '#F8766D', alpha = 0.7) +
    geom_text(aes(x= Age, y = Male, label = round((Male*-1)/10000,0)), size = 3, vjust = 0.5) +
    geom_text(aes(x= Age, y = Female, label = round(Female/10000,0)), size = 3, vjust = 0.5) +
    scale_y_continuous(breaks = brks,
                       labels = lbls) +
    coord_flip() +
    labs(title = '2020년 대한민국 인구 피라미드', x = '연령구간', y = '인구 수(백만)') +
    theme_clean(base_family='NanumGothic') +
    theme(plot.title = element_text(hjust = 0.5),
          axis.ticks = element_blank())

# User Data 결합

user_data <- read_excel("user_cnt_data.xlsx")

head(user_data)

# Colnames by Eng.
colnames(user_data) <- c('Age', 'Male_User', 'Female_User')
col_desc <- user_data$Age
user_data$Age <- factor(user_data$Age, levels = col_desc)

head(user_data)

# Data Preprocess
user_data$Male_User <- user_data$Male_User * -1

merge_data <- data %>%
    inner_join(user_data, by = 'Age')
head(merge_data)

p <- ggplot(data = merge_data) +
    geom_bar(aes(x = Age, y = Male), stat = "identity", fill = '#619CFF', alpha = 0.5) +
    geom_bar(aes(x = Age, y = Female), stat = "identity", fill = '#F8766D', alpha = 0.5) +
    geom_bar(aes(x = Age, y = Male_User), stat = "identity", fill = '#619CFF') +
    geom_bar(aes(x = Age, y = Female_User), stat = "identity", fill = '#F8766D') +
    geom_text(aes(x= Age, y = Male, label = round((Male*-1)/10000,0)), size = 3, vjust = 0.5, hjust = 1) +
    geom_text(aes(x= Age, y = Female, label = round(Female/10000,0)), size = 3, vjust = 0.5, hjust = 1) +
    #geom_text(aes(x= Age, y = Male_User, label = round((Male_User*-1)/10000,0)), size = 3, vjust = 0.5, hjust = -0.5, color = 'blue') +
    #geom_text(aes(x= Age, y = Female_User, label = round(Female_User/10000,0)), size = 3, vjust = 0.5, hjust = -0.5, color = 'blue') +
    scale_y_continuous(breaks = brks,
                       labels = lbls) +
    coord_flip() +
    labs(title = '2020년 대한민국 인구 피라미드', x = '연령구간', y = '인구 수(백만)') +
    theme_clean(base_family='NanumGothic') +
    theme(plot.title = element_text(hjust = 0.5),
          axis.ticks = element_blank())

# User 수는 2만명 이상 구간만 labeling
p2 <- p +
    geom_text(aes(x = Age, y = Male_User, label = round((Male_User*-1)/10000,0)),
              data = merge_data[merge_data$Male_User < -20000,], size = 3, vjust = 0.5, hjust = 1, color = 'blue') +
    geom_text(aes(x = Age, y = Female_User, label = round(Female_User/10000,0)),
              data = merge_data[merge_data$Female_User > 20000,], size = 3, vjust = 0.5, hjust = -1, color = 'blue')
p2

