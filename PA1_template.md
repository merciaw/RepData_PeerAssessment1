---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
1. Load the data (i.e. read.csv())



```r
activity <- read.csv("activity.csv")
```

2. Process/transform the data (if necessary) into a format suitable for your analysis: 

- Check the class of each column
- Change date's class to Date using as.Date
- Change interval's class to POSIXct's time


```r
class(activity$steps)
```

```
## [1] "integer"
```

```r
class(activity$date)
```

```
## [1] "factor"
```

```r
class(activity$interval)
```

```
## [1] "integer"
```

```r
activity$date <- as.Date(activity$date, "%m/%d/%Y")
activity$interval <- as.factor(activity$interval)
library(stringr)
activity$interval <- str_pad(activity$interval, 4, pad = "0")
activity$interval <- substr(as.POSIXct(activity$interval, format='%H%M'), 12, 16)
```

## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day


```r
sumsteps <- tapply(activity$steps,activity$date,sum, na.rm=TRUE)
```

2. Make a histogram of the total number of steps taken each day


```r
hist(sumsteps,breaks=10)
```

![](figure/figure_4-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day


```r
sumsteps <- tapply(activity$steps,activity$date,sum, na.rm=TRUE)
summary(sumsteps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10395    9354   12811   21194
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
intervalstep <- tapply(activity$steps,activity$interval,mean,na.rm=TRUE)
steptime <- cbind(rownames(intervalstep),data.frame(intervalstep,row.names=NULL))
names(steptime) <- c("interval","meanstep")
steptime$interval <- as.POSIXct(steptime$interval, format='%H:%M')
plot(steptime$interval,steptime$meanstep,type="l",xlab="time interval",ylab=" average number of steps", main="Average number of steps across all days")
```

![](figure/figure_6-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

The interval starting from 08:35 to 08:40



```r
maxint <- substr(steptime[steptime$meanstep==max(steptime$meanstep),1], 12,16)
maxint
```

```
## [1] "08:35"
```

## Inputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

2. Fill all of the missing values in the with the mean for that 5-minute interval.


```r
intmean <- tapply(activity$steps,activity$interval,mean, na.rm=TRUE)
intmean2 <- cbind(rownames(intmean), data.frame(intmean,row.names=NULL))
names(intmean2) <- c("interval","intmean")
library(plyr)
activity2 <- merge(activity,intmean2,by="interval",all.x=TRUE)
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activity2$steps <- ifelse(is.na(activity2$steps),activity2$intmean, activity2$steps)
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

- Calculate the total number of steps taken per day


```r
sumsteps2 <- tapply(activity2$steps,activity2$date,sum, na.rm=TRUE)
```

- Make a histogram of the total number of steps taken each day


```r
par(mfrow=c(1,2))
hist(sumsteps,breaks=10, main="With NA", xlab="daily steps")
hist(sumsteps2,breaks=10, main="Without NA", xlab="daily steps")
```

![](figure/figure_12-1.png)<!-- -->

With NA replaced by the mean steps of a given interval throughout all days, there are higher frequency of data close to center (near where mean and median are), hence the distribution looks more normal

- Calculate and report the mean and median of the total number of steps taken per day


```r
summary(sumsteps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10395    9354   12811   21194
```

```r
summary(sumsteps2)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```
The mean and median increase after removing NAs.

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
library(lubridate)
activity2$day <- wday(activity2$date,label=TRUE)
activity2$week <- ifelse(activity2$day=="Sat" | activity2$day=="Sun","weekend","weekday")
activity2$week <- as.factor(activity2$week)
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
par(mfrow=c(2,1))

activity2$interval <- as.POSIXct(activity2$interval, format='%H:%M')

wday <- activity2[activity2$week=="weekday",]
wend <- activity2[activity2$week=="weekend",]
wdaymean <- tapply(wday$steps,wday$interval,mean)
wendmean <- tapply(wend$steps,wend$interval,mean)

intervalstepwday <- tapply(wday$steps,wday$interval,mean,na.rm=TRUE)
steptimewday <- cbind(rownames(intervalstepwday),data.frame(intervalstepwday,row.names=NULL))
names(steptimewday) <- c("interval","meanstep")
steptimewday$interval <- as.POSIXct(steptimewday$interval,format='%Y-%m-%d %H:%M:%S')
plot(steptimewday$interval,steptimewday$meanstep,type="l",xlab="time interval",ylab=" average number of steps", main="Average number of steps on weekday")

intervalstepwend <- tapply(wend$steps,wend$interval,mean,na.rm=TRUE)
steptimewend <- cbind(rownames(intervalstepwend),data.frame(intervalstepwend,row.names=NULL))
names(steptimewend) <- c("interval","meanstep")
steptimewend$interval <- as.POSIXct(steptimewend$interval,format='%Y-%m-%d %H:%M:%S')
plot(steptimewend$interval,steptimewend$meanstep,type="l",xlab="time interval",ylab=" average number of steps", main="Average number of steps on weekend")
```

![](figure/figure_15-1.png)<!-- -->
