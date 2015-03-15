## Data Science - Reproductible Research - Assignment 1

### Purpose

Write a report that answers the questions detailed below.

### Load and preprocess the data


```r
df_activity=read.csv("data/activity.csv")
```

### What is the mean of total number of steps taken per day?


Calculate the total number of steps taken per day.


```r
StepsDay=aggregate(steps ~ date, df_activity, sum, na.rm = "TRUE")
```
Make a histogram of the total number of steps taken each day.


```r
png(filename="figure/hist_steps.png", width=480,height=480, units="px") 

hist(StepsDay$steps, main="Histogram of total number of steps taken each day",
     xlab="Total Number of steps")

dev.off()
```

```
## png 
##   2
```
Calculate and report the mean and median of the total number of steps taken per day.


```r
mean(StepsDay$steps)
```

```
## [1] 10766.19
```
The mean of the total number of steps taken per day is 10,766.19 steps.


```r
median(StepsDay$steps)
```

```
## [1] 10765
```
The median of the total number of steps taken per day is 10,765 steps.

### What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).


```r
png(filename="figure/activity_pattern.png", width=480,height=480, units="px") 

with(df_activity,
    plot(aggregate(steps, by=list(interval), mean, na.rm = "TRUE"),
         xlab="5 minutes interval", ylab="Average Number of steps",
         main="Average daily activity pattern", type= "l"))


dev.off()
```

```
## png 
##   2
```
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
StepsInt=aggregate(steps ~ interval, df_activity, mean, na.rm = "TRUE")
#
StepsInt[which.max(StepsInt$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```
We see that this is the **835th** interval.


### Imputing missing values

There are a number of days/invervals where there are missing values.The presence of missing days may introduce bias into some calculations or summaries of the data.


```r
summary(df_activity)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

```r
table(is.na(df_activity$steps))
```

```
## 
## FALSE  TRUE 
## 15264  2304
```

```r
mean(is.na(df_activity$steps))
```

```
## [1] 0.1311475
```

We see that the missing values are present in the 'step' variable only and that they represent **13 percent** of the total values.


```r
sum(is.na(df_activity$steps))
```

```
## [1] 2304
```

The total number of missing values in the dataset is **2304**.

#### How are these NAs distributed?


```r
data=aggregate(is.na(df_activity$steps) ~ date, df_activity, sum)

colnames(data)=c("date", "sumNA")

data[data$sumNA > 0,]
```

```
##          date sumNA
## 1  2012-10-01   288
## 8  2012-10-08   288
## 32 2012-11-01   288
## 35 2012-11-04   288
## 40 2012-11-09   288
## 41 2012-11-10   288
## 45 2012-11-14   288
## 61 2012-11-30   288
```
We notice that all intervals for the full day (24*60/5=288) are missing for 8 dates.

#### Strategy for filling in all the missing values.

The strategy that I have chosen is to use the mean for that particular missing 5-minute interval.

Create a new dataset that is equal to the original dataset but with the missing data filled in.



```r
df_activity2=df_activity

df_activity2$steps = ifelse(is.na(df_activity2$steps),StepsInt$steps[match(df_activity2$interval, StepsInt$interval)],df_activity2$steps)
```

Make a histogram of the total number of steps taken each day.


```r
StepsDay2=aggregate(steps ~ date, df_activity2, sum, na.rm = "TRUE")

png(filename="figure/hist_steps2.png", width=480,height=480, units="px") 

hist(StepsDay2$steps, main="Histogram of total number of steps taken each day",
     xlab="Total Number of steps")

dev.off()
```

```
## png 
##   2
```

Calculate and report the mean and median of the total number of steps taken per day.


```r
mean(StepsDay2$steps)
```

```
## [1] 10766.19
```

```r
median(StepsDay2$steps)
```

```
## [1] 10766.19
```

####  Do these values differ from the estimates from the first part of the assignment?

The mean and the median are equal to **10,766.19 steps***. 

We notice that our imputation strategy slightly impacted the median, although the mean remained equal.

#### What is the impact of imputing missing data on the estimates of the total daily number of steps?

We would have expected that the imputation would have introduced a bias.

In the chosen strategy the bias is negligible for the given dataset.

### Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
MyLocale <- Sys.getlocale("LC_TIME")

Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

```r
IsWeekDay=function(date){
    givenday=weekdays(date)
    if(givenday %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) 
        return("weekday")
    else if (givenday %in% c("Saturday", "Sunday"))
        return("weekend")
}
df_activity2$daytype=sapply(as.Date(df_activity2$date), IsWeekDay)
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
AvSteps=aggregate(steps ~ interval + daytype, df_activity2, mean)

library(ggplot2)

png(filename="figure/activity_pattern3.png", width=480,height=480, units="px") 

p=ggplot(AvSteps, aes(interval, steps)) + geom_line() + facet_grid(daytype ~ .) + 
    xlab("5 minutes interval") + ylab("Average Number of steps")

print(p)

dev.off()
```

```
## png 
##   2
```

```r
Sys.setlocale("LC_TIME", MyLocale)
```

```
## [1] "English_United States.1252"
```


