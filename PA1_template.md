---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
library(knitr)


```r
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",
              destfile = tempfile())

unzip("activity.zip")

activitycsv <- read.csv("activity.csv")
##pode ter q mudar

library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
activitycsv$date <- ymd(activitycsv$date)

library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```


```r
## What is mean total number of steps taken per day?

steps_per_day <- activitycsv %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarise(steps = sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
steps_per_day_mean <- mean(steps_per_day$steps, na.rm = TRUE)
  
steps_per_day_median <- median(steps_per_day$steps, na.rm = TRUE)

##    Histogram of the total number of steps taken each day

library(ggplot2)

h <- ggplot(steps_per_day, aes(x = steps)) 

h <- h + geom_histogram(fill = "blue", colour = "red", binwidth = 2000) +
  labs(x = "Steps Per Day", y = "Freq.", "Total Number of Steps Each Day") 

h
```

![](PA1_template_files/figure-html/mean-1.png)<!-- -->


```r
## What is the average daily activity pattern?

interval_data <- activitycsv %>%
  filter(!is.na(steps)) %>%
  group_by(interval) %>%
  summarise(steps = mean(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
##    Time series plot of the average number of steps taken

qline <- qplot(x = interval, y = steps, data = interval_data, geom = "line") 

qline
```

![](PA1_template_files/figure-html/average-1.png)<!-- -->

```r
##    The 5-minute interval that, on average, contains the maximum number of steps

max_steps_interval <- interval_data[which.max(interval_data$steps),]

max_steps_interval
```

```
## # A tibble: 1 x 2
##   interval steps
##      <int> <dbl>
## 1      835  206.
```


```r
## Imputing missing values

##    Code to describe and show a strategy for imputing missing data

input_data = activitycsv

summary(input_data)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

```r
input_data$steps[which(is.na(input_data$steps))] = mean(input_data$steps,na.rm = TRUE)

 input_steps_per_day <- input_data %>%
      filter(!is.na(steps)) %>%
      group_by(date) %>%
      summarise(steps = sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
##    Histogram of the total number of steps taken each day after missing values are ##    imputed

inp_h <- ggplot(input_steps_per_day, aes(x = steps)) 

inp_h <- inp_h + geom_histogram(fill = "yellow", colour = "red", binwidth = 2000) +
  labs(x = "Steps Per Day", y = "Freq.", "Total Number of Steps Each Day") 

inp_h
```

![](PA1_template_files/figure-html/missing-1.png)<!-- -->


```r
## Are there differences in activity patterns between weekdays and weekends?

input_interval_data <- input_data %>%
  filter(!is.na(steps)) %>%
  group_by(interval) %>%
  summarise(steps = mean(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
inp_line <- ggplot() + geom_line(data=interval_data, aes(x=interval, y=steps)) + 
     geom_line(data=input_interval_data, aes(x=interval,y=steps), color="red") 

inp_line
```

![](PA1_template_files/figure-html/differences-1.png)<!-- -->

```r
##    Panel plot comparing the average number of steps taken per 5-minute interval  ##    across weekdays and weekends

input_data$day <- as.factor(weekdays(input_data$date, abbreviate = FALSE))

input_data$Type <- ifelse(input_data$day == "sábado" | input_data$day == "domingo", "Weekend", "Weekday")

inp_wxw <- ggplot(data=input_data, aes(x = interval, y = steps, color = Type)) + 
  geom_line() +
  ggtitle("Activity paterns: Weekdays vs weekends")

inp_wxw
```

![](PA1_template_files/figure-html/differences-2.png)<!-- -->
