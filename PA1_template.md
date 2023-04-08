---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---




```r
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
Sys.setlocale("LC_TIME","English")
```

```
## [1] "English_United States.1252"
```
## Loading and preprocessing the data  
1. Unzip the 'activity.zip'
2. Read the data from 'activity.data' to 'data' variable.
3. **data** has three columns, the *steps* and *interval* are double format.  
The *date* has convert into date type, by using the *read_csv* function


```r
data = read_csv(unzip('activity.zip', 'activity.csv'))
```

```
## Rows: 17568 Columns: 3
## -- Column specification --------------------------------------------------------------------------------------------------
## Delimiter: ","
## dbl  (2): steps, interval
## date (1): date
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
```
## What is mean total number of steps taken per day?

```r
day_total <- data %>%
    group_by(date) %>%
    summarize(sum(steps, na.rm = TRUE))
names(day_total)[2] <- 'Total_steps_per_day'
hist(day_total$Total_steps_per_day,main = 'Histogram of total steps per day', 
     xlab = 'Total steps per day',breaks = 10)
```

![plot of chunk total_num_steps](figure/total_num_steps-1.png)

```r
day_mean = mean(day_total$Total_steps_per_day)
day_median = median(day_total$Total_steps_per_day)
```
The mean of the total number of steps taken per day is 9354.2295082  
The median of the total number of steps taken per day is 1.0395 &times; 10<sup>4</sup>

## What is the average daily activity pattern?

```r
step_mean <- data %>%
    group_by(interval) %>%
    summarize(mean(steps, na.rm = TRUE))
names(step_mean)[2] <- 'mean_steps_per_interval'
plot(step_mean$interval, step_mean$mean_steps_per_interval, type='l',
     main = 'Mean steps across all days', xlab = 'mean steps per interval',
     ylab = 'mean steps')
```

![plot of chunk means_steps_per_interval](figure/means_steps_per_interval-1.png)

```r
max_step_mean <- step_mean[step_mean$mean_steps_per_interval==
                                        max(step_mean$mean_steps_per_interval),]
max_interval <- max_step_mean[1]
max_steps_num <- max_step_mean[2]
```
The 835th 5-minute interval contains the maximum number of steps.  
The maximum number of steps is 206.1698113.


## Imputing missing values

```r
data_imp <- data
for (i in 1:nrow(data_imp)){
    if (is.na(data_imp[i,1])){
        data_imp[i,1] <- step_mean[step_mean$interval==data_imp[i,3][[1]],2]
    }
}
day_total <- data_imp %>%
    group_by(date) %>%
    summarize(sum(steps, na.rm = TRUE))
names(day_total)[2] <- 'Total_steps_per_day'
hist(day_total$Total_steps_per_day,main = 'Histogram of total steps per day', 
     xlab = 'Total steps per day',breaks = 10)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)

```r
day_mean = mean(day_total$Total_steps_per_day)
day_median = median(day_total$Total_steps_per_day)
```
The mean of the total number of steps taken per day is 1.0766189 &times; 10<sup>4</sup>  
The median of the total number of steps taken per day is 1.0766189 &times; 10<sup>4</sup>  
These values are bigger than the estimates from the first part of the assignment.  
After imputing missing data, the number of the total steps less than 5000 decreased.  
And the total steps that are around 10000 increased

## Are there differences in activity patterns between weekdays and weekends?
According to the figure, the steps in the morning and noon and decreased and  
in the afternoon, the steps are increased.  

```r
data_imp$week <- weekdays(data_imp$date)
data_imp$week[data_imp$week %in% c('Saturday','Sunday')] <- 'weekend'
data_imp$week[data_imp$week != 'weekend'] <- 'weekday'

# calculate the weekday data
step_weekday <- data_imp[data_imp$week == 'weekday',] %>%
    group_by(interval) %>%
    summarize(mean(steps, na.rm = TRUE))
names(step_weekday)[2] <- 'weekday'

# calculate the weekend data
step_weekend <- data_imp[data_imp$week == 'weekend',] %>%
    group_by(interval) %>%
    summarize(mean(steps, na.rm = TRUE))
names(step_weekend)[2] <- 'weekend'
# plot the figure
par(mfrow = c(2, 1), mar=c(0,4,4,1))
plot(step_weekday$interval, step_weekday$weekday, type='l',
     main = 'Mean steps across on weekdays & weekends',
     ylab = 'mean steps', col = 'blue', ylim = c(0,250))
legend('topright', lty = 1,lwd = 2, col='blue',legend = 'Weekdays')
par( mar=c(4,4,1,1))
plot(step_weekend$interval, step_weekend$weekend, type='l',
     xlab='intervals',ylab = 'mean steps',col='red', ylim = c(0,250))
legend('topright', lty = 1,lwd=2, col='red',legend = 'Weekends')
```

![plot of chunk plot_weekday](figure/plot_weekday-1.png)
