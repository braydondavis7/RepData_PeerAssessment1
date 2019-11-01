---
title: "Activity Monitoring Data"
author: "Braydon"
date: "10/30/2019"
output: 
  html_document: 
    keep_md: yes
keep_md: true
---

Clear workspace

```r
rm(list=ls())
```
Load necessary libraries

```r
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
library(ggplot2)
```
Load the activity data file and explore

```r
activity_data <- read.csv("/Users/braydondavis/Downloads/activity.csv",header=TRUE,na.strings = "NA")
str(activity_data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
Change date from factor to a date

```r
activity_data_cleaned <- activity_data
activity_data_cleaned$date <- as.Date(activity_data_cleaned$date)
str(activity_data_cleaned)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
Remove NAs from the data

```r
activity_data_cleaned <- na.omit(activity_data_cleaned)
```
Create new variables

```r
activity_data_cleaned$year <- as.numeric(format(activity_data_cleaned$date, "%Y"))
activity_data_cleaned$month <- as.numeric(format(activity_data_cleaned$date, "%m"))
activity_data_cleaned$day <- as.numeric(format(activity_data_cleaned$date, "%d"))
summary(activity_data_cleaned)
```

```
##      steps             date               interval           year     
##  Min.   :  0.00   Min.   :2012-10-02   Min.   :   0.0   Min.   :2012  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8   1st Qu.:2012  
##  Median :  0.00   Median :2012-10-29   Median :1177.5   Median :2012  
##  Mean   : 37.38   Mean   :2012-10-30   Mean   :1177.5   Mean   :2012  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-16   3rd Qu.:1766.2   3rd Qu.:2012  
##  Max.   :806.00   Max.   :2012-11-29   Max.   :2355.0   Max.   :2012  
##      month            day       
##  Min.   :10.00   Min.   : 2.00  
##  1st Qu.:10.00   1st Qu.:10.00  
##  Median :10.00   Median :17.00  
##  Mean   :10.45   Mean   :16.68  
##  3rd Qu.:11.00   3rd Qu.:24.00  
##  Max.   :11.00   Max.   :31.00
```
# Question 1 - What is mean total number of steps taken per day?
Create a histogram of the total number of steps taken per day

```r
steps_per_day <- activity_data_cleaned %>% select(steps, date) %>% group_by(date) %>% summarize(sum_steps = sum(steps))
ggplot(data = steps_per_day,aes(x=sum_steps))+
              geom_histogram(aes(y=..count..,fill=..count..),binwidth=1000,colour="black")+
              labs(x="Step Count",y="Frequency",title = "Frequency of Daily Step Count")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
Find the mean total of number of steps taken

```r
mean(steps_per_day$sum_steps)
```

```
## [1] 10766.19
```
# Question 2 - What is the average daily activity pattern?
Create interval dataset with mean steps

```r
interval <- activity_data_cleaned %>% select(interval,steps) %>% group_by(interval) %>% summarize(mean_steps = mean(steps))
```
Create time series plot of average steps taken by interval

```r
ggplot(data = interval, aes(interval,mean_steps))+
        geom_line(color = "dark red")+
        ggtitle("Average Steps Taken by Interval")+
        labs(x= "Interval",y="Average Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
Find the interval that has the maximum number of steps

```r
interval[which.max(interval$mean_steps),]
```

```
## # A tibble: 1 x 2
##   interval mean_steps
##      <int>      <dbl>
## 1      835       206.
```
# Question 3 - Impute missing values
Find the number of missing values in the dataset

```r
sum(is.na(activity_data$steps))
```

```
## [1] 2304
```
Impute NAs with mean 

```r
activity_data_filled <- activity_data
activity_data_filled <- activity_data_filled %>% group_by(interval) %>%
mutate(steps=ifelse(is.na(steps),mean(steps,na.rm=TRUE),steps))
```
Make a histogram of the total number of steps taken each day

```r
steps_per_day2 <- activity_data_filled %>% select (date,steps) %>% group_by(date) %>% summarize(steps = sum(steps))
```

```
## Adding missing grouping variables: `interval`
```

```r
ggplot(data=steps_per_day2,aes(x=steps))+
        geom_histogram(aes(y=..count..,fill=..count..),binwidth=1000,colour="black")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
Calculate the mean of the full dataset without NAs

```r
mean(steps_per_day2$steps)
```

```
## [1] 10766.19
```
Calculate the median of the full dataset without NAs

```r
median(steps_per_day2$steps)
```

```
## [1] 10766.19
```
# Question 4 - Are there differences in activity patterns between weekdays and weekends?
Create Week_Type column

```r
activity_data_filled$date <- as.Date(activity_data_filled$date)
activity_data_filled <- activity_data_filled %>% mutate(week_type = ifelse(weekdays(date) == "Saturday" | weekdays(date) == "Sunday","Weekend","Weekday"))
```
Average steps taken in 5 minute intervals

```r
interval_full_avg <- activity_data_filled %>% group_by(interval,week_type) %>% summarize(steps = mean(steps))
```
Create a panel plot with geom_line

```r
ggplot(data = interval_full_avg, aes(x=interval,y=steps, color = week_type))+
        geom_line()+
        facet_wrap(~week_type,ncol=1,nrow=2)
```

![](PA1_template_files/figure-html/unnamed-chunk-19-1.png)<!-- -->




