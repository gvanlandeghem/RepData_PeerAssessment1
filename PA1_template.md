# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
library(data.table)
setwd("~/Documents/Projects/Learn/coursera-data-science/course\ 5\ -\ Reproducible\ Research/assignment1/RepData_PeerAssessment1")
activities <- read.csv(file.path(getwd(), "activity.csv"))
activities <- data.table(activities) 
activities$date <- as.POSIXct(activities$date)
activities$steps <- as.numeric(activities$steps)
setkey(activities, date)
steps_per_day <- aggregate(activities$steps, by=list(activities$date), FUN=sum)
colnames(steps_per_day) <- list("date", "steps")
head(steps_per_day)
```

```
##         date steps
## 1 2012-10-01    NA
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

## The mean total number of steps taken per day:

```r
hist(steps_per_day$steps, main="Total number of steps taken per day", xlab="Total number of steps", breaks=10)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 
### The mean and median total number of steps taken per day:


```r
mean_of_steps <- mean(steps_per_day$steps,na.rm=TRUE)
print(mean_of_steps)
```

```
## [1] 10766
```


```r
median_of_steps <- median(steps_per_day$steps,na.rm=TRUE)
print(median_of_steps)
```

```
## [1] 10765
```

## The average daily activity pattern:

Time series plot of the 5-minute interval (x-axis) and the average number of steps taken averaged across all days


```r
mean_steps_by_interval <- aggregate(activities$steps, by=list(activities$interval), FUN=mean, na.rm=TRUE)
colnames(mean_steps_by_interval) <- list("interval", "steps")
plot(mean_steps_by_interval, type="l")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
print(paste("Maximum 5-minute interval:", mean_steps_by_interval[which.max(mean_steps_by_interval$steps), 1]))
```

```
## [1] "Maximum 5-minute interval: 835"
```


## Imputing missing values

Number of missing values:


```r
print(paste("Total number of missing values in the dataset:",  sum(is.na(activities$steps))))
```

```
## [1] "Total number of missing values in the dataset: 2304"
```

Strategy for filling in missing values to create a new dataset:


```r
activities_improved <- activities 
intervals <- unique(activities$interval) 
for(i in 1:length(intervals)){
    tmp <- activities_improved[interval==intervals[i]]$steps # get steps for this interval
    activities_improved[interval==intervals[i]]$steps <- replace(tmp, is.na(tmp), mean(tmp, na.rm=TRUE)) # replace value if missing
}
```

Histogram of imputed dataset


```r
steps_per_day_improved <- aggregate(activities_improved$steps, by=list(activities_improved$date), FUN=sum)
colnames(steps_per_day_improved) <- list("date", "steps")
hist(steps_per_day_improved$steps, main="Total number of steps taken per day", xlab="Total number of steps", breaks=10)
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 

Mean and median of imputed dataset


```r
mean_of_steps_improved <- mean(steps_per_day_improved$steps,na.rm=TRUE)
print(mean_of_steps_improved)
```

```
## [1] 10766
```


```r
median_of_steps_improved <- median(steps_per_day_improved$steps,na.rm=TRUE)
print(median_of_steps_improved)
```

```
## [1] 10766
```


## Are there differences in activity patterns between weekdays and weekends?

Adding a daytype (weekday or weekend)


```r
activities <- activities_improved
activities$weekday <- as.factor(weekdays(activities$date))
activities$day_type <- ifelse(activities$weekday %in% c("Sunday", "Saturday"), "weekend", "weekday")

mean_intervals <- activities[, list(mean_steps = mean(steps, na.rm=TRUE)), list(day_type, interval)]
```

Plot of interval steps per daytype:


```r
library(ggplot2)
ggplot(mean_intervals, aes(x=interval, y=mean_steps, color=day_type)) +
geom_line() +
facet_wrap(~ day_type, nrow=2) +
xlab("Interval") +
ylab("Mean Steps") +
ggtitle("Interval steps by daytype (weekday/weekend)") +
theme(legend.position="none")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 





