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
print(paste0("Mean: ", as.character(sum(a$x, na.rm = T))))
```

```
## [1] "Mean: 570608"
```

An
a <- as.data.frame(a)
summary(a)
hist(a$x)
```



## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
