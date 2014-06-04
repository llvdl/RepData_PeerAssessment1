# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
The load the data we first unzip the archive and then read the contained csv file. The date entities are parsed as date objects.


```r
unzip('activity.zip')
activity <- read.csv('activity.csv')
activity$date <- as.Date(activity$date)
```

## What is mean total number of steps taken per day?
To get the mean total number of steps taken per day, we first make a list of the step measurements by day, ignoring readings where there are no steps recorded (i.e. steps is NA). The measurements are than added up per day, resulting in the following histogram.


```r
activityNonNa <- activity[!is.na(activity$steps),]
stepMeasurementsByDay <- by(activityNonNa, activityNonNa$date, function(a) { a$steps })
totalStepsByDay <- sapply(stepMeasurementsByDay, sum)

meanStepsByDay <- mean(totalStepsByDay)
medianStepsByDay <- median(totalStepsByDay)

hist(totalStepsByDay)
abline(v=meanStepsByDay, col="blue")
abline(v=medianStepsByDay, col="green", lty=2)
legend("topright", legend=c('mean', 'median'), lty=c(1,2), col=c('blue', 'green'))
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

The **mean** total number of steps taken per day is **10766.19**.  
The **median** total number of steps taken per day is **10765.00**.  

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
