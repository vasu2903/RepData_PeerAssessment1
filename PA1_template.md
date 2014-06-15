# Reproducible Research: Peer Assessment 1
By Vasudevan Santhanam
Github Repo Information:
https://github.com/vasu2903/RepData_PeerAssessment1

## Loading and preprocessing the data
I have downloaded the Zip file in to my R Home Directory and Unzip the file in same folder 
Reading the file useing read.csv method

```r
activity <-  read.csv("activity.csv" , header=TRUE ,  colClasses = c("numeric", "Date", "numeric"))
```

## What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day


```r
StepsPerDay = aggregate(steps ~ date, data = activity, FUN = sum)
colnames(StepsPerDay) <- c("date","steps")
barplot(StepsPerDay$steps, names.arg=StepsPerDay$date, xlab="date", ylab="steps",col = "red", border = "black")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 
2. Calculate and report the mean and median total number of steps taken per day : 

```r
mean(StepsPerDay$steps , na.rm = TRUE)
```

```
## [1] 10766
```

```r
median(StepsPerDay$steps , na.rm = TRUE)
```

```
## [1] 10765
```
## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
AvgActivity = aggregate(activity$steps, by=list(activity$interval), mean, na.rm=TRUE)
colnames(AvgActivity) <- c("interval","steps")
plot(AvgActivity, type="l")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
AvgActivity$Interval[which.max(AvgActivity$steps)]
```

```
## NULL
```
## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(activity))
```

```
## [1] 2304
```
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. To use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
ActivityUpd <- merge(activity, AvgActivity, by = "interval", suffixes = c("",".mean"))
ActivityUpd$steps = ifelse(is.na(ActivityUpd$steps), ActivityUpd$steps.mean, ActivityUpd$steps)
sum(is.na(ActivityUpd))
```

```
## [1] 0
```
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? 

```r
totalStepsTakenPerDayWithActivityUpd = aggregate(steps ~ date, data = ActivityUpd, FUN = sum)
colnames(totalStepsTakenPerDayWithActivityUpd) <- c("date","steps")
barplot(totalStepsTakenPerDayWithActivityUpd$steps, names.arg=totalStepsTakenPerDayWithActivityUpd$date, xlab="date", ylab="steps",col = "red", border = "black")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 
Calculating the mean and median for the updated activity data set.


```r
mean(totalStepsTakenPerDayWithActivityUpd$steps , na.rm = TRUE)
```

```
## [1] 10766
```


```r
median(totalStepsTakenPerDayWithActivityUpd$steps , na.rm = TRUE)
```

```
## [1] 10766
```
### What is the impact of imputing missing data on the estimates of the total daily number of steps?
Impact of missing data on the calculation is very low atleast in the total daily number of steps**

## Are there differences in activity patterns between weekdays and weekends?

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.0.3
```

```r
# Compute a vector of days
days <- weekdays(as.Date(activity$date , format='%Y-%m-%d'))
# add this column to the activity dataset
activity$days <- days
# Create a data set of weekdays
activityWeekDays <- subset(activity , activity$days!="Saturday" & activity$days != "Sunday")
# Find the average steps by interval in weekday
avgStepsByIntervalInWeekDays = aggregate(activityWeekDays$steps, by=list(activityWeekDays$interval), mean, na.rm=TRUE)
colnames(avgStepsByIntervalInWeekDays) <- c("interval","steps")
# Add a new column called type and set the value as "weekday"
avgStepsByIntervalInWeekDays$type = "weekday"



# Create a data set of weekends
activityWeekends <- subset(activity , activity$days=="Saturday" | activity$days == "Sunday")
# Find the average steps by interval on activityWeekend data set
avgStepsByIntervalInWeekEnds = aggregate(activityWeekends$steps, by=list(activityWeekends$interval), mean, na.rm=TRUE)
colnames(avgStepsByIntervalInWeekEnds) <- c("interval","steps")
# Add a new column type and set the value as "weekend"
avgStepsByIntervalInWeekEnds$type = "weekend"

# Create a new dataset that contains the mean steps by date and type.
activityAvgDataSetByWeek <- rbind(avgStepsByIntervalInWeekDays , avgStepsByIntervalInWeekEnds)
colnames(activityAvgDataSetByWeek) <- c("interval" , "steps" , "type")

ggplot(activityAvgDataSetByWeek, aes(interval, steps)) + geom_line()  + facet_grid( type ~ .)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 
