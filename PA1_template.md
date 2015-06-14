---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    number_sections: yes
    theme: cosmo
    toc: yes
input:
  Rmd_document: PA1_template.RMD
---

## Introduction

This assignment makes use of data from a personal activity monitoring device. 
This device collects data at 5 minute intervals through out the day. 
The data consists of two months of data from an anonymous individual collected 
during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.


## Loading and preprocessing the data

Loading zip file, unzipping to the temporary file and assigning to the data frame.


```r
uRl<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"                   # Set the data url
temp <- tempfile()                                                                           # used as name for temporary files 
download.file(uRl,temp)                                                                      # download file to temp file 
d  <- read.csv(unzip(temp),sep=",", header=T,stringsAsFactors=FALSE, na.strings="NA")        # unzip and read mesured data
unlink(temp)        
```


## What is mean total number of steps taken per day?

For this part of the assignment, we ignore the missing values in the dataset. (We can do it by next parameter na.rm = TRUE)


**Calculate the total number of steps taken per day**


```r
total <- tapply(d$steps, d$date, FUN=sum, na.rm=TRUE)
print("Total steps per day")
```

```
## [1] "Total steps per day"
```

```r
total
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##          0        126      11352      12116      13294      15420 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##      11015          0      12811       9900      10304      17382 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##      12426      15098      10139      15084      13452      10056 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##      11829      10395       8821      13460       8918       8355 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##       2492       6778      10119      11458       5018       9819 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##      15414          0      10600      10571          0      10439 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##       8334      12883       3219          0          0      12608 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##      10765       7336          0         41       5441      14339 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##      15110       8841       4472      12787      20427      21194 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##      14478      11834      11162      13646      10183       7047 
## 2012-11-30 
##          0
```
**If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day**

*Histogram*  (I use ggplot2 library,)


```r
library(ggplot2)
p<-qplot(total, binwidth=850)
t<-element_text(face = "bold", color = "blue")
p + labs(title = "Total steps per day", x = "Steps", y = "Count") + theme(title = t, axis.title = t)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

*Barplot*


```r
barplot(total,main = paste("Total Number of Steps Taken per Day"), xlab="Day")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

**Calculate and report the mean and median of the total number of steps taken per day**

```r
mean(total, na.rm=TRUE)
```

```
## [1] 9354.23
```

```r
median(total, na.rm=TRUE)
```

```
## [1] 10395
```


## What is the average daily activity pattern?

- Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)



```r
avg<- tapply(d$steps,d$interval,mean, na.rm=TRUE) # average activity (steps) per interval
plot(names(avg), avg, type="l", xlab="Intervals", ylab="Avg. number steps")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 
- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

Maximum number of steps

```r
max(avg)
```

```
## [1] 206.1698
```

Interval contains the maximum

```r
maxInt<-names(which.max(avg))
print(paste("Interval which contains the maximum is", maxInt))
```

```
## [1] "Interval which contains the maximum is 835"
```


## Imputing missing values

**Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)**

I calculate the total number of rows with NAs by using the "complete.cases" command, (retun a logical vector indicating which cases are complete, i.e., have no missing values.)

```r
sumNA<- sum(!complete.cases(d))

print(paste("The total number of missing values in the dataset is", sumNA))
```

```
## [1] "The total number of missing values in the dataset is 2304"
```

**Devise a strategy for filling in all of the missing values in the dataset.**

I use the mean for that 5-minute interval.


**Create a new dataset that is equal to the original dataset but with the missing data filled in.**


```r
fill <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps))
        filled <- c(steps)
    else
        filled <- avg[which(names(avg)==interval)]
    return(filled)
}
d_fill <- d
d_fill$steps <- mapply(fill, d_fill$steps, d_fill$interval)
```


**Make a histogram of the total number of steps taken each day**


```r
total_fill <- tapply(d_fill$steps, d_fill$date, FUN=sum)
qplot(total_fill, binwidth=1000, xlab="Number of steps per day (with filled data)")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

**Calculate and report the mean and median total number of steps taken per day.**


```r
mean(total_fill)
```

```
## [1] 10766.19
```

```r
median(total_fill)
```

```
## [1] 10766.19
```

Do these values differ from the estimates from the first part of the assignment?  - Yes, mean and median values are higher after imputing missing data.

What is the impact of imputing missing data on the estimates of the total daily number of steps?

Ater replacing missing values by using the mean for that 5-minute interval, total number of steps taken each day was changed.


## Are there differences in activity patterns between weekdays and weekends?

**Create a new factor variable in the dataset with two levels – “weekday” and “weekend”**

```r
Sys.setlocale("LC_TIME", "eng") 
```

```
## [1] "English_United Kingdom.1252"
```

```r
week <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
}
d_fill$date <- as.Date(d_fill$date)
d_fill$day <- sapply(d_fill$date, FUN=week)
```
**Make a panel plot.**


```r
avg_fill <- aggregate(steps ~ interval + day, data=d_fill, mean)
ggplot(avg_fill, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
    xlab("Interval") + ylab("Steps")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png) 


