# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

Let's simple read file, convert data into data format  and see result.


```r
DateType  <- "%Y-%m-%d"
FitbitData <- read.csv(unz("activity.zip", "activity.csv"), header = T)
FitbitData$date <- as.Date(FitbitData$date, DateType)
#FitbitData$date <- as.factor(FitbitData$date)
summary(FitbitData)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```


## What is mean total number of steps taken per day?

For this part I've calculated total numbers of steps for each day.

```r
a <- aggregate(FitbitData$steps, by=list(Category=FitbitData$date), FUN=sum)
```
So, mean and median value are:

```r
print(paste0("Mean: ", as.character(mean(a$x, na.rm = T))))
```

```
## [1] "Mean: 10766.1886792453"
```

```r
print(paste0("Median: ", as.character(median(a$x, na.rm = T))))
```

```
## [1] "Median: 10765"
```
And there is histogram:


```r
#summary(a)
hist(a$x, main = "Total number of steps per day", xlab = "Steps per day", col = "cadetblue2")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 


## What is the average daily activity pattern?

Add the plot with averaged steps per interval:


```r
b <- aggregate(FitbitData$steps, by=list(Category=FitbitData$interval), FUN=mean, na.rm=TRUE)
plot(b, type="l", col="cadetblue4", lwd=2.5, 
     main="Average activity pattern", 
     xlab="Interval", ylab="Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?