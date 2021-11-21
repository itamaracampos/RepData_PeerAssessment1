---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


```r
knitr::opts_chunk$set(warning = FALSE)
knitr::opts_chunk$set(message = FALSE)
```


To read to whole *Peer Assessment 1* text click [here](https://github.com/itamaracampos/RepData_PeerAssessment1).
In this file is only show main points.


```r
#install.packages("tidyverse")
#install.packages("Hmisc")
```



```r
library(tidyverse) 
library(Hmisc)
library(lattice)
```


Note: the dataset and this file (plus code) must be in the same directory.

Save the plots in a directory. First create if don't exist.


```r
dir <- "figure"

if (!dir.exists(dir)){
dir.create(dir)
} else {
    print("Dir already exists!")
}
```

```
## [1] "Dir already exists!"
```




#### **1. Loading and preprocessing the data**

 **<span style="color: blue"> Solution  </span>**
 
Download the dataset from the cloud:


```r
 if(!file.exists("getdata-projectfiles-UCI HAR Dataset.zip")) {
        temp <- tempfile()
        download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
        unzip(temp)
        unlink(temp)
}
```


Or download the dataset from gihub directory, click in [View raw](https://github.com/itamaracampos/RepData_PeerAssessment1/blob/master/activity.zip).


```r
unzip("./activity.zip")
activityData <- read.csv("./activity.csv")
activity <- read.csv("activity.csv")
```



```r
head(activityData)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
summary(activityData)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```

```r
summary(activity)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```

 <span style="color: Green"> Download both ways show the same results. Chose one of those to continue. </span>


```r
describe(activity)
```

```
## activity 
## 
##  3  Variables      17568  Observations
## --------------------------------------------------------------------------------
## steps 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##    15264     2304      617    0.624    37.38    66.75      0.0      0.0 
##      .25      .50      .75      .90      .95 
##      0.0      0.0     12.0     86.0    252.8 
## 
## lowest :   0   1   2   3   4, highest: 786 789 794 802 806
## --------------------------------------------------------------------------------
## date 
##        n  missing distinct 
##    17568        0       61 
## 
## lowest : 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05
## highest: 2012-11-26 2012-11-27 2012-11-28 2012-11-29 2012-11-30
## --------------------------------------------------------------------------------
## interval 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##    17568        0      288        1     1178    799.5    110.0    220.0 
##      .25      .50      .75      .90      .95 
##    588.8   1177.5   1766.2   2135.0   2245.0 
## 
## lowest :    0    5   10   15   20, highest: 2335 2340 2345 2350 2355
## --------------------------------------------------------------------------------
```



#### **2. What is mean total number of steps taken per day?**

 **<span style="color: blue"> Solution </span>**

1. For this part of the assignment, ignore the missing values in the dataset. 1. Calculate the total number of steps taken per day.


```r
totalStepsByDay<-aggregate(steps~date, activity, na.rm = TRUE, sum )
```


2. Make a histogram of the total number of steps taken each day.ow same data, display using histogram. Depending how is display the plot can lead to wrong conclusion. 



```r
par(mfrow=c(1,2))

plot2a <- hist(totalStepsByDay$steps, xlab=" Total Number of Steps", ylab="Number of Days", 
     main="Number of Steps",
     col="lightblue1",
     border="dodgerblue3", ylim = c(0,30), labels=TRUE) 

plot2b <- hist(totalStepsByDay$steps, xlab=" Total Number of Steps", ylab="Number of Days", 
     main="Number of Steps",
     col="lightblue1",
     border="dodgerblue3", ylim = c(0,20),
     breaks = seq(0,30000, by=3000), labels=TRUE) #if you the test, play with this factor  breaks = seq(0,30000, by=3000)
```

![](FinalP01_files/figure-html/unnamed-chunk-10-1.png)<!-- -->



```
## png 
##   2
```

The **mean** of the total number of steps taken per day:


```r
mean(totalStepsByDay$steps)
```

```
## [1] 10766.19
```

**Median** of the total number of steps taken per day:


```r
median(totalStepsByDay$steps)
```

```
## [1] 10765
```




#### **3. What is the average daily activity pattern?**

 **<span style="color: blue"> Solution </span>**

1. Make a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
average_daily_activity <- aggregate(activity$steps, by=list(activity$interval), FUN=mean, na.rm=TRUE)

names(average_daily_activity) <- c("interval", "mean")

plot(average_daily_activity$interval, average_daily_activity$mean, type = "l", 
     col="blue", lwd = 2, xlab="Interval", ylab="Average number of steps", main="Average number of steps per intervals")
```

![](FinalP01_files/figure-html/unnamed-chunk-14-1.png)<!-- -->




```
## png 
##   2
```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
average_daily_activity[which.max(average_daily_activity$mean), ]$interval
```

```
## [1] 835
```





#### **4. Imputing missing values**

 **<span style="color: blue"> Solution </span>**

1. To remove the possibility of bias, we can remove the missing values. Which are:


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset.



```r
all_steps <- average_daily_activity$mean[match(activity$interval, average_daily_activity$interval)]

glimpse(all_steps)
```

```
##  num [1:17568] 1.717 0.3396 0.1321 0.1509 0.0755 ...
```

```r
describe(all_steps)
```

```
## all_steps 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##    17568        0      254        1    37.38    39.44   0.0000   0.1698 
##      .25      .50      .75      .90      .95 
##   2.4858  34.1132  52.8349  82.9057 103.7170 
## 
## lowest :   0.0000000   0.0754717   0.1132075   0.1320755   0.1509434
## highest: 177.3018868 179.5660377 183.3962264 195.9245283 206.1698113
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in


```r
activity_all <- transform(activity, steps = ifelse(is.na(activity$steps), yes = all_steps, no = activity$steps))

total_steps_all <- aggregate(steps ~ date, activity_all, sum)

names(total_steps_all) <- c("date", "daily_steps")

describe(total_steps_all)
```

```
## total_steps_all 
## 
##  2  Variables      61  Observations
## --------------------------------------------------------------------------------
## date 
##        n  missing distinct 
##       61        0       61 
## 
## lowest : 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05
## highest: 2012-11-26 2012-11-27 2012-11-28 2012-11-29 2012-11-30
## --------------------------------------------------------------------------------
## daily_steps 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##       61        0       54    0.998    10766     4245     3219     5441 
##      .25      .50      .75      .90      .95 
##     9819    10766    12811    15098    15420 
## 
## lowest :    41   126  2492  3219  4472, highest: 15414 15420 17382 20427 21194
## --------------------------------------------------------------------------------
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. The **mean** and **median** total number of steps taken per day.



```r
par(mfrow=c(1,2))

hist(total_steps_all$daily_steps, col = "pink", xlab = "Total steps per day", 
     main="Number of Steps",
     border="dodgerblue3", ylim = c(0,40), labels=TRUE) 

hist(total_steps_all$daily_steps, col = "pink", xlab = "Total steps per day", 
     main="Number of Steps",
     border="dodgerblue3", ylim = c(0,30),
     breaks = seq(0,30000, by=3000), labels=TRUE) #if you want to test, play with this factor  breaks = seq(0,30000, by=3000)
```

![](FinalP01_files/figure-html/unnamed-chunk-20-1.png)<!-- -->




```
## png 
##   2
```


```r
mean(total_steps_all$daily_steps)
```

```
## [1] 10766.19
```

```r
median(total_steps_all$daily_steps)
```

```
## [1] 10766.19
```



#### **5. Are there differences in activity patterns between weekdays and weekends?**


 **<span style="color: blue"> Solution </span>**

Use weekdays() function.Below two ways to plot the same data.

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend"


```r
activity$date <- as.Date(strptime(activity$date, format="%Y-%m-%d"))

activity$datetype <- sapply(activity$date, function(x) {
  if (weekdays(x) == "Saturday" | weekdays(x) =="Sunday") 
        {y <- "Weekend"} else 
            {y <- "Weekday"}
            y
  })

describe(activity$datetype)
```

```
## activity$datetype 
##        n  missing distinct 
##    17568        0        2 
##                           
## Value      Weekday Weekend
## Frequency    12960    4608
## Proportion   0.738   0.262
```

2. Make a panel plot containing a time series. Using i.e. type = "l. The plot should look something like the following, which was created using **simulated data**


```r
activity_by_date <- aggregate(steps~interval + datetype, activity, mean, na.rm = TRUE)

plot5a <- ggplot(activity_by_date, aes(x = interval , y = steps, color = datetype)) + geom_line() +
  labs(title = "Average daily steps by type of date", x = "Interval", y = "Average number of steps") +
   facet_wrap(~datetype, ncol = 1, nrow=2)
print(plot5a )
```

![](FinalP01_files/figure-html/unnamed-chunk-24-1.png)<!-- -->



```r
plot5b <- with(activity_by_date, 
      xyplot(steps ~ interval | datetype, 
      type = "l",      
      main = "Total Number of Steps within Intervals by dayType",
      xlab = "Daily Intervals",
      ylab = "Average Number of Steps"))
print(plot5b )
```

![](FinalP01_files/figure-html/unnamed-chunk-25-1.png)<!-- -->



```
## png 
##   2
```

```
## png 
##   2
```
