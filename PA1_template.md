# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
To load the data the archive is unzipped and then contained csv file is read. The date entities are parsed as date objects. Some measurements are incomplete, so a separate set (`measurementsComplete`) is created that only has complete measurements (i.e. steps is not NA).


```r
unzip('activity.zip')
measurements <- read.csv('activity.csv')
measurements$date <- as.Date(measurements$date)
measurementsComplete <- measurements[complete.cases(measurements), ]
```

The loaded and cleaned measurements have the following fields:

* steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

* date: The date on which the measurement was taken as Date

* interval: Identifier for the 5-minute interval in which measurement was taken, coded as hhmm (e.g. "5" for 00:05 and 1450 for 14:50)


## What is mean total number of steps taken per day?
First, a list of the sum of steps per day is created. So, for each day with one or more complete measurements, the list contains an entry with the total amount of steps recorded.  


```r
totalStepsPerDay <- by(measurementsComplete, measurementsComplete$date, function(m) { 
    sum(m$steps) 
})
hist(totalStepsPerDay, xlab='Total steps per day', main='Histogram of total steps per day')
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

From this list **the mean and median of total steps per day** can be calculated:


```r
meanStepsPerDay <- mean(totalStepsPerDay)
medianStepsPerDay <- median(totalStepsPerDay)
list(
    mean_steps_per_day=meanStepsPerDay,
    median_steps_per_day=medianStepsPerDay
)
```

```
## $mean_steps_per_day
## [1] 10766
## 
## $median_steps_per_day
## 2012-11-12 
##      10765
```

## What is the average daily activity pattern?
For the average daily activity pattern we look at the mean of the steps measured by interval.


```r
meansByInterval <- by(measurementsComplete, measurementsComplete$interval, function(m) { 
    mean(m$steps)
})
intervals <- names(meansByInterval)
plot(x=intervals, meansByInterval, type='l')
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

The maximum mean is:


```r
list(
    maximum_mean=max(meansByInterval),
    interval_with_maximum_mean=intervals[meansByInterval == max(meansByInterval)]
)
```

```
## $maximum_mean
## [1] 206.2
## 
## $interval_with_maximum_mean
## [1] "835"
```

## Imputing missing values
Some measurements are incomplete. There should only be missings steps, and no missing values for date or interval.



Some missing values are:


```r
head(measurements[is.na(measurements$steps),], n=3)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
```

The missing values will be imputed with the mean of the step measurements. The following filler function is used:


```r
filler <- function(step, interval, meansByIntervalList) {
    index <- as.character(interval)
    ifelse(is.na(step), meansByIntervalList[[index]], step)
}
```

Using the filler function a data set with imputed values is created: 


```r
imputed <- measurements
imputed$steps <- mapply(filler, imputed$steps, imputed$interval, MoreArgs=list(as.list(meansByInterval)))

head(imputed[is.na(measurements$steps), ], n=3)
```

```
##    steps       date interval
## 1 1.7170 2012-10-01        0
## 2 0.3396 2012-10-01        5
## 3 0.1321 2012-10-01       10
```

### Testing the impact of imputing data
To get a sense of the impact of imputing, a histogram is created as shown below, to compare the results side by side.


```r
totalStepsPerDayImputed <- by(imputed, imputed$date, function(m) { 
    sum(m$steps)
})

totalStepsByDayImputed <- by(imputed, imputed$date, function(m) { 
    sum(m$steps) 
})

par(mfrow=c(1,2))
hist(totalStepsPerDay, xlab='Total steps per day', main='Without incomplete data', ylim=c(0,40))
hist(totalStepsByDayImputed, xlab='Total steps per day', main='With imputed data', ylim=c(0,40))
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

Only the frequency of the the of the average amount of steps per day is increased, presumably since the missing values where filled with the average amounts. Also, because average values were added, it is expected that the mean will not change and the median may shift a little towards the mean.


```r
meanStepsPerDayImputed <- mean(totalStepsPerDayImputed)
medianStepsPerDayImputed <- median(totalStepsPerDayImputed)

list(
    mean_steps_per_day=meanStepsPerDay,
    mean_steps_per_day_imputed=meanStepsPerDayImputed,
    median_steps_per_day=medianStepsPerDay,
    median_steps_per_day_imputed=medianStepsPerDayImputed
)
```

```
## $mean_steps_per_day
## [1] 10766
## 
## $mean_steps_per_day_imputed
## [1] 10766
## 
## $median_steps_per_day
## 2012-11-12 
##      10765 
## 
## $median_steps_per_day_imputed
## 2012-11-04 
##      10766
```

I suspect that the impact of imputing missing data means that estimates move toward mean of the non-missing values.

## Are there differences in activity patterns between weekdays and weekends?
To see if there are differences between activity patterns in weekdays and weekends, a factor is added to indicate that the measurement was taken in the weekend or on a weekday. Note that the non imputed, complete measurements are used and not the incomplete or imputed list.


```r
inWeekend <- weekdays(measurementsComplete$date) %in% c('Saturday', 'Sunday')
dayFactor <- factor(ifelse(inWeekend, 'weekend', 'weekday'))
measurementsComplete$dayFactor <- dayFactor

# show amounts of measurements grouped by day factor
table(measurementsComplete$dayFactor)
```

```
## 
## weekday weekend 
##   11232    4032
```

The dataset can now be split by interval and day factor. For each interval and day factor, the mean of measured steps is calculated. In the diagram below the means of the steps per interval are plotted for both weekend and weekday days.


```r
# calculate amount of step means by interval and day factor
library(plyr)
steps <- ddply(measurementsComplete, .(interval, dayFactor), function(piece) {
    mean(piece$steps)
})
names(steps)[3] <- 'steps'

# plot steps versus interval grouped by day factor
library(lattice)
xyplot(steps ~ interval | dayFactor, steps, layout=c(1,2), type='l', ylab='Number of steps')
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 
