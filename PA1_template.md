---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

## What is mean total number of steps taken per day?

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
dailySteps <- group_by(activity,date) %>% summarize(Avg_Steps = mean(steps,na.rm = TRUE), 
                                          Median_Steps = median(steps),na.rm = TRUE)

hist(dailySteps$Avg_Steps )
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
dailySteps
```

```
## # A tibble: 61 x 4
##    date       Avg_Steps Median_Steps na.rm
##  * <date>         <dbl>        <dbl> <lgl>
##  1 2012-10-01   NaN               NA TRUE 
##  2 2012-10-02     0.438            0 TRUE 
##  3 2012-10-03    39.4              0 TRUE 
##  4 2012-10-04    42.1              0 TRUE 
##  5 2012-10-05    46.2              0 TRUE 
##  6 2012-10-06    53.5              0 TRUE 
##  7 2012-10-07    38.2              0 TRUE 
##  8 2012-10-08   NaN               NA TRUE 
##  9 2012-10-09    44.5              0 TRUE 
## 10 2012-10-10    34.4              0 TRUE 
## # ... with 51 more rows
```
## What is the average daily activity pattern?

```r
activityByWeek <- mutate(activity,is_weekend = weekdays(date) %in% c("Saturday","Sunday"))

stepsByInterval <- group_by(activityByWeek,interval,is_weekend) %>% summarize(Avg_Steps = mean(steps,na.rm = TRUE))
```

```
## `summarise()` has grouped output by 'interval'. You can override using the `.groups` argument.
```

```r
overall_activity <- group_by(activityByWeek,interval) %>% summarize(Avg_Steps = mean(steps,na.rm=TRUE))
weekend_activity<- subset(stepsByInterval,is_weekend==TRUE)
weekday_activity <-  subset(stepsByInterval,is_weekend==FALSE)

opar <- par(no.readonly = TRUE)
par(mfrow=c(2,1),mar=c(4,4,2,1))

plot(overall_activity$interval,overall_activity$Avg_Steps, type = "l", col="grey", main="Average Steps by Interval - Weekday",xlab = "interval", ylab = "number of steps",lwd = 6)

lines(weekday_activity$interval,weekday_activity$Avg_Steps, col="blue", lwd = 2)

legend("topleft", inset = .01, title="Average Steps",c("Over All","Weekday"),lty = c(1,1),col = c("grey","blue"))

plot(overall_activity$interval,overall_activity$Avg_Steps, type = "l", col="grey", main="Average Steps by Interval - Wweekend",xlab = "interval", ylab = "number of steps",lwd = 6)

lines(weekend_activity$interval, weekend_activity$Avg_Steps, col = "red", lwd=2)

legend("topleft", inset = .01, title="Average Steps",c("Over All","Weekend"),lty = c(1,1),col = c("grey","red"))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
par(opar)
```
## Inputting missing values

```r
na_table <- is.na(activity$steps)
sum(na_table)
```

```
## [1] 2304
```

```r
valid_activity <- na.omit(activity)
na_activity <- activity[na_table,]
na_activity <- mutate(na_activity,is_weekend = weekdays(date) %in% c("Saturday","Sunday"))
na_fixed_activity <-inner_join(na_activity,stepsByInterval, by=c("interval","is_weekend")) %>% select (steps = Avg_Steps, date, interval)

fixed_activity <-rbind(valid_activity,na_fixed_activity)

fixed_dailySteps <- group_by(fixed_activity,date) %>% summarize(Avg_Steps = mean(steps,na.rm = TRUE),  Median_Steps = median(steps),na.rm = TRUE)

hist(fixed_dailySteps$Avg_Steps, main = "Daily Average Steps", xlab = "Average Steps"  )
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

## Are there differences in activity patterns between weekdays and weekends?

```r
Activity_By_WeekDays <- mutate(fixed_activity,WeekDay = weekdays(date), is_weekend = WeekDay %in% c("Saturday", "Sunday"))
Activity_By_WeekDays$dayofweek <- c("weekday")
Activity_By_WeekDays[Activity_By_WeekDays$is_weekend == TRUE,]$dayofweek <- "weekend"

avg_activity_by_interval <- group_by(Activity_By_WeekDays,interval) %>% summarize(avg_steps = mean(steps))
activity_by_weekofday <- group_by(Activity_By_WeekDays,interval,dayofweek) %>% summarize(avg_steps = mean(steps))
```

```
## `summarise()` has grouped output by 'interval'. You can override using the `.groups` argument.
```

```r
avg_activity_by_weekday <- subset(activity_by_weekofday, dayofweek == "weekday")
avg_activity_by_weekend <- subset(activity_by_weekofday,dayofweek == "weekend")

opar <- par(no.readonly = TRUE)
par(mfrow=c(2,1),mar=c(4,4,2,1))

plot(avg_activity_by_interval$interval,avg_activity_by_interval$avg_steps,main = "Average Steps by Interval - Weekday", xlab = "interval", ylab = "number of steps", col = "grey",lwd=6,type = "l")

lines(avg_activity_by_weekday$interval,avg_activity_by_weekday$avg_steps,col="blue",lwd=2.5)

legend("topleft", inset = .01, title="Average Steps",c("Over All","Week Day"),lty = c(1,1),col = c("grey","blue"))


plot(avg_activity_by_interval$interval,avg_activity_by_interval$avg_steps,main = "Average Steps by Interval - Weekend", xlab = "interval", ylab = "number of steps", col = "grey",lwd=6,type = "l")

lines(avg_activity_by_weekend$interval,avg_activity_by_weekend$avg_steps,col="red",lwd=2)

legend("topleft", inset = .01, title="Average Steps",c("Over All","Weekend"),lty = c(1,1),col = c("grey","red"))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
par(opar)
```
