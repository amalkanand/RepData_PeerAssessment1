---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.4.4
```

```r
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.4.4
```

```r
library(scales)
```

```
## Warning: package 'scales' was built under R version 3.4.4
```

## Loading and preprocessing the data

```r
activity <- read.csv("activity.csv")

activity$day <- weekdays(as.Date(activity$date))
activity$DateTime<- as.POSIXct(activity$date, format="%Y-%m-%d")
```


## What is mean total number of steps taken per day?

```r
TotalStepsPerDay <- tapply(activity$steps, activity$date, sum, na.rm=TRUE)
```

##### 1. Make a histogram of the total number of steps taken each day

```r
hist(TotalStepsPerDay, xlab = "No of Steps", main = "Steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

##### 2. Calculate and report the mean and median total number of steps taken per day

```r
MeanPerDay <- mean(TotalStepsPerDay, na.rm = TRUE)
MedianPerDay <- median(TotalStepsPerDay, na.rm = TRUE)
```

* Mean: 9354.2295082
* Median:  10395

## What is the average daily activity pattern?


```r
AvgStepsTimeBlock <- aggregate(x=list(MeanSteps=activity$steps), by=list(interval=activity$interval), FUN=mean, na.rm=TRUE)
```

##### 1. Make a time series plot

```r
ggplot(data=AvgStepsTimeBlock, aes(x=interval, y=MeanSteps)) +
    geom_line() +
    xlab("5-min interval") +
    ylab("Avg no of steps taken") 
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

##### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
MaxSteps <- which.max(AvgStepsTimeBlock$MeanSteps)
TimeMaxSteps <-  gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", AvgStepsTimeBlock[MaxSteps,'interval'])
```

* Maximum number of Steps at: 8:35



## Imputing missing values


##### 1. Calculate and report the total number of missing values in the dataset 

```r
NoofMissingValues <- length(which(is.na(activity$steps)))
```

* No of missing values: 2304

##### 2. Devise a strategy for filling in all of the missing values in the dataset.The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
##### I will impute by Mean


##### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
StepsPerInterval <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)
ActivityDataSplit <- split(activity, activity$interval)
for(i in 1:length(ActivityDataSplit)){
    ActivityDataSplit[[i]]$steps[is.na(ActivityDataSplit[[i]]$steps)] <- StepsPerInterval[i]
}
ActivityDataImputed <- do.call("rbind", ActivityDataSplit)
ActivityDataImputed <- ActivityDataImputed[order(ActivityDataImputed$date) ,]
```

##### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps? 

```r
StepsDayImputed <- tapply(ActivityDataImputed$steps, ActivityDataImputed$date, sum)
hist(StepsDayImputed, xlab = "No of Steps", main = "Steps per Day for Imputed data")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
StepsMeanImputed <- mean(StepsDayImputed, na.rm = TRUE)
StepsMedianImputed <- median(StepsDayImputed, na.rm = TRUE)

DiffMean <- StepsMeanImputed - MeanPerDay
DiffMedian <- StepsMedianImputed - MedianPerDay
```

* Mean (Imputed): 1.0766189\times 10^{4}
* Median (Imputed):  1.0766189\times 10^{4}
* Difference of Mean:  1411.959171
* Difference of Median:  371.1886792


## Are there differences in activity patterns between weekdays and weekends?


##### 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
ActivityDataImputed$dateType <-  ifelse(as.POSIXlt(ActivityDataImputed$date)$wday %in% c(0,6), 'Weekend', 'Weekday')
```

##### 2. Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
AvgActivityDataImputed <- aggregate(steps ~ interval + dateType, data=ActivityDataImputed, mean)
ggplot(AvgActivityDataImputed, aes(interval, steps)) + 
    geom_line() + 
    facet_grid(dateType ~ .) +
    xlab("5-minute interval") + 
    ylab("Avg no of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->


