---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
require(dplyr)
```

```
## Loading required package: dplyr
```

```
## Warning: package 'dplyr' was built under R version 3.4.4
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
dat=read.csv("C:/Users/U410DA/Desktop/coursera/RepData/activity/activity.csv")
data=dat%>%group_by(date)
by_date=data%>%summarise(steps=sum(steps))
data2=dat%>%group_by(interval)
by_interval=data2%>%summarise(steps=mean(steps,na.rm=TRUE))
```

## What is mean total number of steps taken per day?

```r
hist(by_date$steps,main ="Total Steps Per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
mean(by_date$steps,na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(by_date$steps,na.rm = TRUE)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

```r
plot(by_interval,type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
by_interval$interval[which.max(by_interval$steps)]
```

```
## [1] 835
```
## Imputing missing values

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

```r
n=length(dat$steps)/length(by_interval$steps)
reps = rep(by_interval$steps,times=n)
newdata=dat
for (i in 1:length(dat$steps)){
  if(is.na(dat$steps)[i]){
    newdata$steps[i]=reps[i]
  } else{
    newdata$steps[i]=dat$steps[i]
  }
}
newdata2=newdata%>%group_by(date)
newdata_by_date=newdata2%>%summarise(steps=sum(steps))
hist(newdata_by_date$steps,main ="Total Steps Per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
mean(newdata_by_date$steps)
```

```
## [1] 10766.19
```

```r
median(newdata_by_date$steps)
```

```
## [1] 10766.19
```
THe mean of total steps per day is unchanged but the median is increased to now equal the mean. 


## Are there differences in activity patterns between weekdays and weekends?

```r
newdata$day_of_week=weekdays(as.Date(  newdata$date))
for (i in 1:length(newdata$steps)){
  if(newdata$day_of_week[i]=='Saturday'|newdata$day_of_week[i]=='Sunday'){
    newdata$weekday_weekend[i]='weekend'
  } else{
    newdata$weekday_weekend[i]='weekday'
  }
}
newdata$weekday_weekend=as.factor(newdata$weekday_weekend)
newdata3=newdata%>%group_by(interval,weekday_weekend)
newdata_by_interval=newdata3%>%summarise(steps=mean(steps,na.rm=TRUE))
par(mfrow=c(2,1))
plot(y=newdata_by_interval$steps[newdata_by_interval$weekday_weekend=='weekday'],x=newdata_by_interval$interval[newdata_by_interval$weekday_weekend=='weekday'], type="l",ylab='Steps',xlab='Interval',main="Weekdays")
plot(y=newdata_by_interval$steps[newdata_by_interval$weekday_weekend=='weekend'],x=newdata_by_interval$interval[newdata_by_interval$weekday_weekend=='weekend'], type="l",ylab='Steps',xlab='Interval',main="Weekends")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
