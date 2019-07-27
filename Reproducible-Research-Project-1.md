---
title: "Reproducible Research Project 1"
author: "Edric Kaw"
date: "7/27/2019"
output:
  html_document: 
    keep_md: yes
  word_document: default
---



## Assignment Instructions

1.Code for reading in the dataset and/or processing the data  
2.Histogram of the total number of steps taken each day  
3.Mean and median number of steps taken each day  
4.Time series plot of the average number of steps taken  
5.The 5-minute interval that, on average, contains the maximum number of steps  
6.Code to describe and show a strategy for imputing missing data  
7.Histogram of the total number of steps taken each day after missing values are imputed  
8.Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
9.All of the R code needed to reproduce the results (numbers, plots, etc.) in the report  


## 1. Code for reading in the dataset and/or processing the data


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
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
activity <- read.csv("activity.csv", header = TRUE ,sep=",")
head(activity, n=10)
```

```
##    steps       date interval
## 1     NA 2012-10-01        0
## 2     NA 2012-10-01        5
## 3     NA 2012-10-01       10
## 4     NA 2012-10-01       15
## 5     NA 2012-10-01       20
## 6     NA 2012-10-01       25
## 7     NA 2012-10-01       30
## 8     NA 2012-10-01       35
## 9     NA 2012-10-01       40
## 10    NA 2012-10-01       45
```

```r
tail(activity, n=10)
```

```
##       steps       date interval
## 17559    NA 2012-11-30     2310
## 17560    NA 2012-11-30     2315
## 17561    NA 2012-11-30     2320
## 17562    NA 2012-11-30     2325
## 17563    NA 2012-11-30     2330
## 17564    NA 2012-11-30     2335
## 17565    NA 2012-11-30     2340
## 17566    NA 2012-11-30     2345
## 17567    NA 2012-11-30     2350
## 17568    NA 2012-11-30     2355
```

```r
activity$date <- ymd(activity$date)
activity <- mutate(activity, date_1 = wday(activity$date, label=TRUE) )
activity$date_2 <- as.factor(ifelse((activity$date_1) %in% c("Sat", "Sun"), "weekend", "weekday"))
summary(activity)
```

```
##      steps             date               interval      date_1    
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0   Sun:2304  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8   Mon:2592  
##  Median :  0.00   Median :2012-10-31   Median :1177.5   Tue:2592  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5   Wed:2592  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2   Thu:2592  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0   Fri:2592  
##  NA's   :2304                                           Sat:2304  
##      date_2     
##  weekday:12960  
##  weekend: 4608  
##                 
##                 
##                 
##                 
## 
```

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  5 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ date_1  : Ord.factor w/ 7 levels "Sun"<"Mon"<"Tue"<..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ date_2  : Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
```


## 2.Histogram of the total number of steps taken each day  

Aggregation of steps by date


```r
activity_ttl_steps <- aggregate(steps ~ date, data=activity, FUN=sum )
head(activity_ttl_steps)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

Histogram of steps per day


```r
library(ggplot2)
png("plot1.png")
g <- ggplot(activity_ttl_steps, aes(date, steps))
g + geom_bar(stat="identity") +
    xlab("Date") + 
    ylab("Total number of steps") +
    ggtitle("Total number of steps per day") +
    theme(plot.title= element_text(hjust= 0.5))
dev.off()
```

```
## png 
##   2
```

```r
g <- ggplot(activity_ttl_steps, aes(date, steps))
g + geom_bar(stat="identity") +
    xlab("Date") + 
    ylab("Total number of steps") +
    ggtitle("Total number of steps per day") +
    theme(plot.title= element_text(hjust= 0.5))
```

![](Reproducible-Research-Project-1_files/figure-html/Histogram-1.png)<!-- -->


## 3.Mean and median number of steps taken each day  

Frequency of total number steps per day


```r
png("plot2.png")
hist(activity_ttl_steps$steps, xlab="Steps", main="Total Steps per Day")
dev.off()
```

```
## png 
##   2
```

```r
hist(activity_ttl_steps$steps, xlab="Steps", main="Total Steps per Day")
```

![](Reproducible-Research-Project-1_files/figure-html/Frequency steps per day-1.png)<!-- -->


Descriptive statistic for steps by day


```r
summary(activity_ttl_steps$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10765   10766   13294   21194
```

```r
as.integer(mean(activity_ttl_steps$steps))
```

```
## [1] 10766
```

```r
as.integer(median(activity_ttl_steps$steps))
```

```
## [1] 10765
```


## 4.Time series plot of the average number of steps taken  

Aggregation of steps by interval


```r
activity_interval_steps <- aggregate(steps ~ interval, data=activity, FUN=mean)
```

Time Series plot


```r
png("plot3.png")
g1 <- ggplot(activity_interval_steps, aes(interval, steps))
g1 + geom_line() +
     ylab("Average number of steps") +
     xlab("Interval") +
     ggtitle("Average No. of Steps by Interval") +
     theme(plot.title = element_text(hjust = 0.5))
dev.off()
```

```
## png 
##   2
```

```r
g1 <- ggplot(activity_interval_steps, aes(interval, steps))
g1 + geom_line() +
     ylab("Average number of steps") +
     xlab("Interval") +
     ggtitle("Average No. of Steps by Interval") +
     theme(plot.title = element_text(hjust = 0.5))
```

![](Reproducible-Research-Project-1_files/figure-html/Time Series-1.png)<!-- -->


## 5.The 5-minute interval that, on average, contains the maximum number of steps 


```r
max_steps <- max(activity_interval_steps$steps)
activity_interval_steps[activity_interval_steps$steps == max_steps,"interval"]
```

```
## [1] 835
```


## 6.Code to describe and show a strategy for imputing missing data  

For strategy, Will using the mean of 5 minute interval to impute the missing values


```r
NAdata <- activity[is.na(activity$steps),]
Cleandata <- activity[is.na(activity$steps) == FALSE,]
Meandata <- aggregate( steps ~interval , data=Cleandata, FUN=mean)

Newdata <- merge( NAdata[,c("date","interval")], Meandata, by=c("interval") )
Newdata <- Newdata[,c("steps","date","interval")]
Newdata <- mutate(Newdata, date_1 = wday(Newdata$date, label=TRUE) )
Newdata$date_2 <- as.factor(ifelse((Newdata$date_1) %in% c("Sat", "Sun"), "weekend", "weekday"))
head(Newdata)
```

```
##      steps       date interval date_1  date_2
## 1 1.716981 2012-10-01        0    Mon weekday
## 2 1.716981 2012-11-30        0    Fri weekday
## 3 1.716981 2012-11-04        0    Sun weekend
## 4 1.716981 2012-11-09        0    Fri weekday
## 5 1.716981 2012-11-14        0    Wed weekday
## 6 1.716981 2012-11-10        0    Sat weekend
```

```r
mergedata <- rbind(Cleandata, Newdata) %>% arrange(date, interval)
impute_activity_ttl_steps <- aggregate( steps ~ date, data=mergedata, FUN=sum)
summary(impute_activity_ttl_steps$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```

```r
as.integer(mean(impute_activity_ttl_steps$steps))
```

```
## [1] 10766
```

```r
as.integer(median(impute_activity_ttl_steps$steps))
```

```
## [1] 10766
```

The new mean of imputed data is 10766 steps compared to old mean of 10766 steps.
The new median of imputed data is 10766 steps compared to old median of 10765 steps.
However, the overall distribution is roughly the same.

## 7.Histogram of the total number of steps taken each day after missing values are imputed  


```r
png("plot4.png")
hist(impute_activity_ttl_steps$steps, xlab="Steps", main="Total Steps per Day", col="Red")
hist(activity_ttl_steps$steps, xlab="Steps", main="Total Steps per Day", col="Blue" , add=T)
legend("topright", c("Imputed Data", "Non-NA Data"), fill=c("Red", "Blue") )
dev.off()
```

```
## png 
##   2
```

```r
hist(impute_activity_ttl_steps$steps, xlab="Steps", main="Total Steps per Day", col="Red")
hist(activity_ttl_steps$steps, xlab="Steps", main="Total Steps per Day", col="Blue" , add=T)
legend("topright", c("Imputed Data", "Non-NA Data"), fill=c("Red", "Blue") )
```

![](Reproducible-Research-Project-1_files/figure-html/histogram-1.png)<!-- -->

## 8.Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends


```r
activity_interval_steps_2 <- aggregate(steps ~ interval + date_2, data=activity, FUN=mean)
png("plot5.png")
g2 <- ggplot(activity_interval_steps_2, aes(interval, steps, fill=date_2) )
g2 + geom_line() + 
     facet_grid(date_2~.) +
     ggtitle("Mean of Steps by Interval Based on Weekday and Weekend") +
     xlab("Interval") +
     ylab("Average number of steps") +
     theme(plot.title = element_text(hjust = 0.5))
dev.off()
```

```
## png 
##   2
```

```r
g2 <- ggplot(activity_interval_steps_2, aes(interval, steps, fill=date_2) )
g2 + geom_line() + 
     facet_grid(date_2~.) +
     ggtitle("Mean of Steps by Interval Based on Weekday and Weekend") +
     xlab("Interval") +
     ylab("Average number of steps") +
     theme(plot.title = element_text(hjust = 0.5))
```

![](Reproducible-Research-Project-1_files/figure-html/compare weekday and weekend-1.png)<!-- -->

From the graph plotted, step activity differ between weekday and weekend.
This may resulted from higher opportunity of getting activity on weekend compared to weekday.

