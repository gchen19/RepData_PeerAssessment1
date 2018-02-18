---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

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
library(lattice)
activity_data <- read.csv("activity.csv", colClasses = c("integer","Date", "integer"))
```

```
## Warning in strptime(xx, f <- "%Y-%m-%d", tz = "GMT"): unknown timezone
## 'default/America/Los_Angeles'
```


## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day

```r
totalNumSteps <- group_by(activity_data, date) %>% summarize(totalSteps = sum(steps, na.rm = TRUE))
totalNumSteps <- as.data.frame(totalNumSteps)
totalNumSteps
```

```
##          date totalSteps
## 1  2012-10-01          0
## 2  2012-10-02        126
## 3  2012-10-03      11352
## 4  2012-10-04      12116
## 5  2012-10-05      13294
## 6  2012-10-06      15420
## 7  2012-10-07      11015
## 8  2012-10-08          0
## 9  2012-10-09      12811
## 10 2012-10-10       9900
## 11 2012-10-11      10304
## 12 2012-10-12      17382
## 13 2012-10-13      12426
## 14 2012-10-14      15098
## 15 2012-10-15      10139
## 16 2012-10-16      15084
## 17 2012-10-17      13452
## 18 2012-10-18      10056
## 19 2012-10-19      11829
## 20 2012-10-20      10395
## 21 2012-10-21       8821
## 22 2012-10-22      13460
## 23 2012-10-23       8918
## 24 2012-10-24       8355
## 25 2012-10-25       2492
## 26 2012-10-26       6778
## 27 2012-10-27      10119
## 28 2012-10-28      11458
## 29 2012-10-29       5018
## 30 2012-10-30       9819
## 31 2012-10-31      15414
## 32 2012-11-01          0
## 33 2012-11-02      10600
## 34 2012-11-03      10571
## 35 2012-11-04          0
## 36 2012-11-05      10439
## 37 2012-11-06       8334
## 38 2012-11-07      12883
## 39 2012-11-08       3219
## 40 2012-11-09          0
## 41 2012-11-10          0
## 42 2012-11-11      12608
## 43 2012-11-12      10765
## 44 2012-11-13       7336
## 45 2012-11-14          0
## 46 2012-11-15         41
## 47 2012-11-16       5441
## 48 2012-11-17      14339
## 49 2012-11-18      15110
## 50 2012-11-19       8841
## 51 2012-11-20       4472
## 52 2012-11-21      12787
## 53 2012-11-22      20427
## 54 2012-11-23      21194
## 55 2012-11-24      14478
## 56 2012-11-25      11834
## 57 2012-11-26      11162
## 58 2012-11-27      13646
## 59 2012-11-28      10183
## 60 2012-11-29       7047
## 61 2012-11-30          0
```

2. Make a histogram of the total number of steps taken each day

```r
ggplot(data=totalNumSteps, aes(totalNumSteps$totalSteps)) + xlab("Steps taken in a day") + labs(title ="Total Number of Steps taken each day") +  geom_histogram();
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day

```r
meanNumberOfStepsTakenPerDay <- mean(totalNumSteps$totalSteps)
medianNumberOfStepsTakenPerDay <-median(totalNumSteps$totalSteps)
meanNumberOfStepsTakenPerDay
```

```
## [1] 9354.23
```

```r
medianNumberOfStepsTakenPerDay
```

```
## [1] 10395
```

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
averageStepsPerInterval <- group_by(activity_data, interval) %>% summarize(averageSteps = mean(steps, na.rm = TRUE))
averageStepsPerInterval <- as.data.frame(averageStepsPerInterval)
ggplot(data = averageStepsPerInterval, aes(x=interval, y=averageSteps)) + xlab("5 min interval number") + ylab("average amount of steps") + labs(title= "Average number of steps taken in each 5 minute interval") + geom_line() 
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
intervalWithMaximumSteps <- averageStepsPerInterval[averageStepsPerInterval$averageSteps == max(averageStepsPerInterval$averageSteps),"interval"]
intervalWithMaximumSteps
```

```
## [1] 835
```


## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
numberOfMissingValues <- sum(is.na(activity_data))
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Strategy is to calculate the mean number of steps taken in that interval across all days, and use that value to replace the NA value

```r
averageStepsPerInterval <- group_by(activity_data, interval) %>% summarize(averageSteps = mean(steps, na.rm = TRUE))
averageStepsPerInterval <- as.data.frame(averageStepsPerInterval)
#3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
replaceNAValues <- function(activity_data, averageStepsPerInterval) {
  for (i in 1:dim(activity_data)[1]) {
    if (is.na(activity_data$steps[i])) {
      activity_data$steps[i] <- averageStepsPerInterval$averageSteps[averageStepsPerInterval$interval == activity_data$interval[i]]
    }
  }
  return(activity_data)
}
newActivity_data <- replaceNAValues(activity_data, averageStepsPerInterval)
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 

```r
newTotalNumSteps <- group_by(newActivity_data, date) %>% summarize(totalSteps = sum(steps, na.rm = TRUE))
newTotalNumSteps <- as.data.frame(newTotalNumSteps)
ggplot(data=newTotalNumSteps, aes(newTotalNumSteps$totalSteps)) + xlab("Steps taken in a day") + labs(title ="New Total Number of Steps taken each day") +  geom_histogram();
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
newMeanNumberOfStepsTakenPerDay <- mean(newTotalNumSteps$totalSteps)
newMedianNumberOfStepsTakenPerDay <- median(newTotalNumSteps$totalSteps)
newMeanNumberOfStepsTakenPerDay
```

```
## [1] 10766.19
```

```r
newMedianNumberOfStepsTakenPerDay
```

```
## [1] 10766.19
```

Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
Yes! the mean and median values are different from first part of the assignment. Imputing these new values significantly reduces the amount of data where the total steps were 0, and makes the mean value equal the median"

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
isWeekday <- match(weekdays(newActivity_data$date), c("Monday","Tuesday","Wednesday","Thursday","Friday"), nomatch = 0) > 0
isWeekday <- factor(isWeekday, labels = c("weekday", "weekend"))
 
newActivity_data <- mutate(newActivity_data, weekday = isWeekday)
```
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See theREADME file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
newAverageStepsPerInterval <- group_by(newActivity_data, interval, weekday) %>% summarize(averageSteps = mean(steps, na.rm = TRUE))
newAverageStepsPerInterval <- as.data.frame(newAverageStepsPerInterval)

xyplot(averageSteps~interval|weekday, data = newAverageStepsPerInterval, layout= (c(1,2)), type ="l", 
       main ="Average number of steps per interval", xlab="5 minute interval number", ylab="Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
