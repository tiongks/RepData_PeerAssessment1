---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Overview

This report outlines metrics gathered from two months of activity data gathered from 
an anonymous individual collected during the months of October and November, 2012.

## Setup



## Loading and preprocessing the data



```r
library(ggplot2)
all_activities <- read.csv(unzip("activity.zip", "activity.csv"))
file.remove("activity.csv")
```

```
## [1] TRUE
```

```r
activity <- all_activities[!is.na(all_activities$steps),]
activity$date <- as.Date(activity$date, "%Y-%m-%d")
summary(activity)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-02   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-29   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-30   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-16   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-29   Max.   :2355.0
```

## What is mean total number of steps taken per day?


```r
mean(activity$steps, na.rm = TRUE )
```

```
## [1] 37.3826
```


```r
# summarize the steps by day
# steps_per_day <- setNames(aggregate(activity$steps, by=list(date=activity$date), FUN=sum), c("date", "steps"))
# steps_per_day <- setNames(aggregate(activity$steps ~ activity$date, FUN=sum), c("date", "steps"))
steps_per_day <- aggregate(list(steps = activity$steps), list(date = activity$date), FUN=sum)
gs <- ggplot(steps_per_day)
gs + geom_histogram(aes(x = steps), fill = "blue", color="green", bins =25) + labs(x = "steps per day")
```

![](PA1_template_files/figure-html/steps per day-1.png)<!-- -->

```r
summary(steps_per_day$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10765   10766   13294   21194
```

## What is the average daily activity pattern?


```r
average_steps_per_day <- aggregate(list(steps = activity$steps), list(date = activity$date), FUN=mean)
ga <- ggplot(data = average_steps_per_day)
ga + geom_line(aes(x = date, y = steps), color = "blue") + 
  geom_hline(yintercept = mean(average_steps_per_day$steps), linetype = "dotted")
```

![](PA1_template_files/figure-html/daily activity pattern-1.png)<!-- -->

```r
activity[activity$steps == max(activity$steps, na.rm = TRUE), ]
```

```
##       steps       date interval
## 16492   806 2012-11-27      615
```

## Imputing missing values


```r
nrow(all_activities[is.na("steps"),])
```

```
## [1] 0
```


## Are there differences in activity patterns between weekdays and weekends?
