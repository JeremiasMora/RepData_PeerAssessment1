---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

```r
library(ggplot2)
```


## Loading and preprocessing the data

The data for this assignment can be downloaded from the course web site:

*Dataset*: Activity monitoring data.

The variables included in this dataset are:

*steps*: Number of steps taking in a 5-minute interval (missing values are coded as NA).

*date*: The date on which the measurement was taken in YYYY-MM-DD format.

*interval*: Identifier for the 5-minute interval in which measurement was taken.


```r
activity = read.csv('activity.csv', header = T)
data = as.data.frame(activity[complete.cases(activity), ])
```







## What is mean total number of steps taken per day?

Calculate the total number of steps taken per day

```r
total.steps = tapply(data$steps, data$date, sum)
```


Make a histogram of the total number of steps taken each day

```r
hist(total.steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


Calculate and report the mean and median of the total number of steps taken per day

```r
mean = mean(total.steps, na.rm = TRUE)
median = median(total.steps, na.rm = TRUE)
```







## What is the average daily activity pattern?

Make a time series plot (i.e.type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
data.plot = aggregate(data$steps, by = list(data$interval), FUN = mean)
plot(data.plot[, 1], data.plot[, 2], type = "l", xlab = "5-minute interval", 
     ylab = "Average Number of Steps", main = "Average Daily Activity Pattern")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
data.plot[which.max(data.plot[, 2]), 1]
```

```
## [1] 835
```







## Imputing missing values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(!complete.cases(activity))
```

```
## [1] 2304
```


Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
missing = activity
missing[is.na(missing$steps), "steps"] = 0
no.missing = aggregate(steps ~ date, missing, sum)
```


Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
hist(no.missing$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->


```r
mean(no.missing$steps)
```

```
## [1] 9354.23
```

```r
median(no.missing$steps)
```

```
## [1] 10395
```







## Are there differences in activity patterns between weekdays and weekends?


Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
days.ends = data.table::fread(input = 'activity.csv')
days.ends[, date := as.POSIXct(date, format = "%Y-%m-%d")]
days.ends[, `Day of Week`:= weekdays(x = date)]
days.ends[grepl(pattern = "lunes|martes|miércoles|jueves|viernes", x = `Day of Week`), "weekday or weekend"] = "weekday"
days.ends[grepl(pattern = "sábado|domingo", x = `Day of Week`), "weekday or weekend"] = "weekend"
days.ends[, `weekday or weekend` := as.factor(`weekday or weekend`)]
head(days.ends, 10)
```

```
##     steps       date interval Day of Week weekday or weekend
##  1:    NA 2012-10-01        0       lunes            weekday
##  2:    NA 2012-10-01        5       lunes            weekday
##  3:    NA 2012-10-01       10       lunes            weekday
##  4:    NA 2012-10-01       15       lunes            weekday
##  5:    NA 2012-10-01       20       lunes            weekday
##  6:    NA 2012-10-01       25       lunes            weekday
##  7:    NA 2012-10-01       30       lunes            weekday
##  8:    NA 2012-10-01       35       lunes            weekday
##  9:    NA 2012-10-01       40       lunes            weekday
## 10:    NA 2012-10-01       45       lunes            weekday
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
days.ends[is.na(steps), "steps"] = 
    days.ends[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
ts = days.ends[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval, `weekday or weekend`)] 
ggplot(ts , aes(x = interval , y = steps, color=`weekday or weekend`)) + 
    geom_line() + 
    labs(title = "Avg. Daily Steps by Weektype", x = "Interval", y = "No. of Steps") +
    facet_wrap(~`weekday or weekend` , ncol = 1, nrow=2)
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->


