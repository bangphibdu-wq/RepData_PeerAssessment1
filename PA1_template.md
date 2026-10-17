---
title: "Reproducible Research: Course Project 1"
author: "bangphibdu-wq"
date: "2026-10-17"
output: 
  html_document:
    keep_md: true
---



## Introduction
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Loading and preprocessing the data
We will load the dataset using `read.csv()`. We will also convert the `date` variable to a proper `Date` format.


``` r
# Đọc dữ liệu từ thư mục làm việc hiện tại
activity <- read.csv("activity.csv")

# Tiền xử lý định dạng ngày tháng
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")

# Kiểm tra sơ lược cấu trúc dữ liệu
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What is mean total number of steps taken per day?
For this part of the assignment, the missing values in the dataset are ignored.

1. **Calculate the total number of steps taken per day:**

``` r
library(dplyr)
total_steps_per_day <- activity %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps))

head(total_steps_per_day)
```

```
## # A tibble: 6 × 2
##   date       total_steps
##   <date>           <int>
## 1 2012-10-02         126
## 2 2012-10-03       11352
## 3 2012-10-04       12116
## 4 2012-10-05       13294
## 5 2012-10-06       15420
## 6 2012-10-07       11015
```

2. **Make a histogram of the total number of steps taken each day:**

``` r
hist(total_steps_per_day$total_steps, 
     breaks = 20,
     main = "Histogram of Total Steps Taken Each Day", 
     xlab = "Total Steps per Day", 
     col = "skyblue")
```

![](PA1_template_files/figure-html/histogram_1-1.png)<!-- -->

3. **Calculate and report the mean and median of the total number of steps taken per day:**

``` r
mean_steps <- mean(total_steps_per_day$total_steps)
median_steps <- median(total_steps_per_day$total_steps)
```
* **Mean** number of steps taken per day: 1.076619\times 10^{4}
* **Median** number of steps taken per day: 10765

## What is the average daily activity pattern?
1. **Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis):**

``` r
average_daily_pattern <- activity %>%
  filter(!is.na(steps)) %>%
  group_by(interval) %>%
  summarise(avg_steps = mean(steps))

plot(average_daily_pattern$interval, average_daily_pattern$avg_steps, 
     type = "l", 
     main = "Average Daily Activity Pattern",
     xlab = "5-minute Interval", 
     ylab = "Average Number of Steps", 
     col = "darkblue",
     lwd = 1.5)
```

![](PA1_template_files/figure-html/daily_pattern-1.png)<!-- -->

2. **Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?**

``` r
max_interval <- average_daily_pattern$interval[which.max(average_daily_pattern$avg_steps)]
max_steps <- max(average_daily_pattern$avg_steps)
```
Khoảng thời gian 5 phút chứa số lượng bước chân trung bình lớn nhất là khoảng **835** (với trung bình 206.17 bước).

## Imputing missing values
1. **Calculate and report the total number of missing values in the dataset:**

``` r
total_nas <- sum(is.na(activity$steps))
```
Tổng số dòng bị thiếu dữ liệu (`NA`) trong tập dữ liệu ban đầu là: **2304**.

2. **Devise a strategy for filling in all of the missing values:**
Chiến lược được áp dụng là thay thế các giá trị `NA` bằng giá trị bước chân trung bình của chính khoảng thời gian 5 phút (`interval`) đó đã được tính toán ở phần trước.

3. **Create a new dataset that is equal to the original dataset but with the missing data filled in:**

``` r
activity_imputed <- activity %>%
  left_join(average_daily_pattern, by = "interval") %>%
  mutate(steps = ifelse(is.na(steps), avg_steps, steps)) %>%
  select(-avg_steps)

# Xác nhận không còn giá trị NA nào
sum(is.na(activity_imputed$steps))
```

```
## [1] 0
```

4. **Make a histogram of the total number of steps taken each day and Calculate and report the mean and median:**

``` r
total_steps_imputed <- activity_imputed %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps))

hist(total_steps_imputed$total_steps, 
     breaks = 20,
     main = "Histogram of Total Steps per Day (Imputed Data)", 
     xlab = "Total Steps per Day", 
     col = "lightgreen")
```

![](PA1_template_files/figure-html/histogram_2-1.png)<!-- -->

``` r
mean_imputed <- mean(total_steps_imputed$total_steps)
median_imputed <- median(total_steps_imputed$total_steps)
```
* **Mean** sau khi xử lý dữ liệu khuyết: 1.076619\times 10^{4}
* **Median** sau khi xử lý dữ liệu khuyết: 1.076619\times 10^{4}

**Nhận xét:** Giá trị Mean vẫn giữ nguyên so với phần đầu tiên (1.076619\times 10^{4}). Tuy nhiên, giá trị Median đã có sự thay đổi nhẹ từ 10765 sang 1.076619\times 10^{4}, lúc này Median đã trùng khớp hoàn toàn với Mean do chúng ta sử dụng phương pháp điền giá trị trung bình từ các khoảng thời gian tương ứng.

## Are there differences in activity patterns between weekdays and weekends?
1. **Create a new factor variable in the dataset with two levels – “weekday” and “weekend”:**

``` r
# Chuyển đổi ngôn ngữ hệ thống sang tiếng Anh để hàm weekdays() nhận diện chính xác "Saturday"/"Sunday"
Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

``` r
activity_imputed <- activity_imputed %>%
  mutate(day_type = ifelse(weekdays(date) %in% c("Saturday", "Sunday"), 
                           "weekend", "weekday"),
         day_type = as.factor(day_type))

table(activity_imputed$day_type)
```

```
## 
## weekday weekend 
##   12960    4608
```

2. **Make a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken:**

``` r
panel_data <- activity_imputed %>%
  group_by(interval, day_type) %>%
  summarise(avg_steps = mean(steps))

library(ggplot2)
ggplot(panel_data, aes(x = interval, y = avg_steps, color = day_type)) +
  geom_line(size = 0.7) +
  facet_wrap(~day_type, nrow = 2) +
  labs(title = "Activity Patterns: Weekdays vs. Weekends",
       x = "5-minute Interval",
       y = "Average Number of Steps") +
  theme_bw() +
  theme(legend.position = "none")
```

![](PA1_template_files/figure-html/panel_plot-1.png)<!-- -->
