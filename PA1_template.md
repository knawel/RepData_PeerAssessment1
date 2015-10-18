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
So, mean and median values are:

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
Let's fill NA values.

The total number of missing values in the dataset

```r
sum(is.na(FitbitData$steps))
```

```
## [1] 2304
```

```NA``` are located only in ```steps``` column. I replace NA by averages steps per 5-min interval. And save this new dataset as ```FitbitData_woNA```.



```r
FitbitData_woNA <- FitbitData
NA_intervals <- FitbitData_woNA$interval[is.na(FitbitData_woNA$steps)]
NA_steps <- numeric()
for (i in NA_intervals){
    NA_steps <- c(NA_steps, b$x[b$Category == i])
}
FitbitData_woNA$steps[is.na(FitbitData_woNA$steps)] <- NA_steps
head(FitbitData_woNA)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

```r
head(FitbitData)
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
This is a histogram of the total number of steps per each day for this new dataset:


```r
c <- aggregate(FitbitData_woNA$steps, by=list(Category=FitbitData_woNA$date), FUN=sum)
hist(c$x, main = "Total number of steps per day", xlab = "Steps per day", col = "cadetblue2")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

Mean and median values for this new dataset are:

```r
print(paste0("Mean for old dataset: ", as.character(mean(a$x, na.rm = T))))
```

```
## [1] "Mean for old dataset: 10766.1886792453"
```

```r
print(paste0("Mean for new dataset: ", as.character(mean(c$x))))
```

```
## [1] "Mean for new dataset: 10766.1886792453"
```

```r
print(paste0("Median for old dataset: ", as.character(median(a$x, na.rm = T))))
```

```
## [1] "Median for old dataset: 10765"
```

```r
print(paste0("Median  for new dataset: ", as.character(median(c$x))))
```

```
## [1] "Median  for new dataset: 10766.1886792453"
```

Mean and median value didn't change. It can be due to ```na``` values present only for one day (1st Oct). And steps for *every* intervals of this day is ```na```. And when I replaced these values by mean values nothing changes (at least for calculation of mean and median).

## Are there differences in activity patterns between weekdays and weekends?

At the first set up locale

```r
Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

And devide dataset into two:


```r
wd <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")

FitbitData_woNA$day <- weekdays(FitbitData_woNA$date)
head(FitbitData_woNA$day )
```

```
## [1] "Monday" "Monday" "Monday" "Monday" "Monday" "Monday"
```

```r
FitbitData_weekday<-FitbitData_woNA[FitbitData_woNA$day %in% wd, ]
FitbitData_weekend<-FitbitData_woNA[!(FitbitData_woNA$day %in% wd), ]


print(FitbitData_weekend$day)
```

```
##    [1] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##    [7] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##   [13] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##   [19] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##   [25] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##   [31] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##   [37] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##   [43] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##   [49] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##   [55] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##   [61] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##   [67] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##   [73] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##   [79] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##   [85] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##   [91] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##   [97] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [103] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [109] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [115] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [121] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [127] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [133] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [139] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [145] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [151] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [157] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [163] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [169] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [175] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [181] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [187] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [193] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [199] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [205] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [211] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [217] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [223] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [229] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [235] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [241] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [247] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [253] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [259] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [265] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [271] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [277] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [283] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [289] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [295] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [301] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [307] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [313] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [319] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [325] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [331] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [337] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [343] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [349] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [355] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [361] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [367] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [373] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [379] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [385] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [391] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [397] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [403] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [409] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [415] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [421] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [427] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [433] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [439] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [445] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [451] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [457] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [463] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [469] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [475] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [481] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [487] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [493] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [499] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [505] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [511] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [517] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [523] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [529] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [535] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [541] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [547] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [553] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [559] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [565] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [571] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [577] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [583] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [589] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [595] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [601] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [607] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [613] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [619] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [625] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [631] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [637] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [643] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [649] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [655] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [661] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [667] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [673] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [679] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [685] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [691] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [697] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [703] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [709] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [715] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [721] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [727] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [733] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [739] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [745] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [751] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [757] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [763] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [769] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [775] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [781] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [787] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [793] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [799] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [805] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [811] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [817] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [823] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [829] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [835] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [841] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [847] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [853] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [859] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
##  [865] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [871] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [877] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [883] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [889] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [895] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [901] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [907] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [913] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [919] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [925] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [931] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [937] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [943] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [949] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [955] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [961] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [967] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [973] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [979] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [985] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [991] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
##  [997] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1003] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1009] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1015] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1021] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1027] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1033] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1039] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1045] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1051] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1057] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1063] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1069] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1075] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1081] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1087] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1093] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1099] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1105] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1111] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1117] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1123] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1129] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1135] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1141] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1147] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1153] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1159] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1165] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1171] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1177] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1183] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1189] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1195] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1201] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1207] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1213] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1219] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1225] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1231] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1237] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1243] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1249] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1255] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1261] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1267] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1273] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1279] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1285] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1291] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1297] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1303] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1309] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1315] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1321] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1327] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1333] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1339] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1345] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1351] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1357] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1363] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1369] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1375] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1381] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1387] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1393] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1399] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1405] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1411] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1417] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1423] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1429] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1435] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1441] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1447] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1453] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1459] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1465] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1471] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1477] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1483] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1489] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1495] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1501] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1507] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1513] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1519] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1525] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1531] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1537] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1543] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1549] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1555] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1561] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1567] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1573] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1579] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1585] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1591] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1597] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1603] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1609] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1615] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1621] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1627] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1633] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1639] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1645] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1651] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1657] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1663] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1669] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1675] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1681] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1687] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1693] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1699] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1705] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1711] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1717] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1723] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [1729] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1735] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1741] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1747] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1753] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1759] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1765] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1771] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1777] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1783] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1789] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1795] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1801] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1807] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1813] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1819] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1825] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1831] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1837] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1843] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1849] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1855] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1861] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1867] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1873] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1879] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1885] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1891] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1897] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1903] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1909] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1915] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1921] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1927] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1933] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1939] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1945] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1951] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1957] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1963] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1969] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1975] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1981] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1987] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1993] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [1999] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2005] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2011] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2017] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2023] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2029] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2035] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2041] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2047] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2053] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2059] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2065] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2071] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2077] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2083] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2089] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2095] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2101] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2107] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2113] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2119] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2125] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2131] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2137] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2143] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2149] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2155] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2161] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2167] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2173] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2179] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2185] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2191] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2197] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2203] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2209] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2215] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2221] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2227] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2233] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2239] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2245] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2251] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2257] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2263] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2269] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2275] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2281] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2287] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2293] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2299] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2305] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2311] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2317] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2323] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2329] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2335] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2341] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2347] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2353] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2359] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2365] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2371] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2377] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2383] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2389] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2395] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2401] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2407] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2413] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2419] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2425] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2431] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2437] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2443] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2449] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2455] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2461] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2467] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2473] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2479] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2485] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2491] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2497] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2503] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2509] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2515] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2521] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2527] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2533] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2539] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2545] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2551] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2557] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2563] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2569] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2575] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2581] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2587] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2593] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2599] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2605] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2611] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2617] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2623] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2629] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2635] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2641] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2647] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2653] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2659] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2665] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2671] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2677] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2683] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2689] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2695] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2701] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2707] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2713] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2719] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2725] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2731] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2737] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2743] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2749] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2755] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2761] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2767] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2773] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2779] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2785] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2791] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2797] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2803] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2809] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2815] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2821] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2827] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2833] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2839] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2845] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2851] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2857] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2863] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2869] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2875] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [2881] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2887] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2893] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2899] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2905] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2911] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2917] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2923] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2929] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2935] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2941] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2947] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2953] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2959] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2965] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2971] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2977] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2983] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2989] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [2995] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3001] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3007] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3013] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3019] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3025] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3031] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3037] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3043] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3049] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3055] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3061] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3067] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3073] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3079] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3085] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3091] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3097] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3103] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3109] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3115] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3121] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3127] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3133] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3139] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3145] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3151] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3157] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3163] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3169] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3175] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3181] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3187] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3193] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3199] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3205] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3211] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3217] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3223] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3229] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3235] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3241] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3247] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3253] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3259] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3265] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3271] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3277] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3283] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3289] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3295] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3301] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3307] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3313] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3319] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3325] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3331] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3337] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3343] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3349] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3355] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3361] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3367] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3373] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3379] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3385] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3391] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3397] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3403] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3409] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3415] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3421] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3427] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3433] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3439] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3445] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3451] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3457] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3463] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3469] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3475] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3481] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3487] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3493] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3499] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3505] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3511] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3517] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3523] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3529] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3535] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3541] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3547] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3553] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3559] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3565] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3571] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3577] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3583] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3589] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3595] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3601] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3607] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3613] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3619] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3625] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3631] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3637] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3643] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3649] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3655] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3661] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3667] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3673] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3679] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3685] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3691] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3697] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3703] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3709] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3715] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3721] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3727] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3733] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3739] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [3745] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3751] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3757] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3763] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3769] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3775] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3781] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3787] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3793] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3799] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3805] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3811] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3817] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3823] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3829] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3835] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3841] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3847] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3853] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3859] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3865] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3871] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3877] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3883] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3889] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3895] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3901] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3907] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3913] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3919] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3925] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3931] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3937] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3943] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3949] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3955] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3961] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3967] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3973] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3979] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3985] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3991] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [3997] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4003] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4009] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4015] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4021] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4027] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4033] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4039] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4045] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4051] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4057] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4063] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4069] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4075] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4081] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4087] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4093] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4099] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4105] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4111] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4117] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4123] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4129] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4135] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4141] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4147] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4153] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4159] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4165] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4171] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4177] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4183] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4189] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4195] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4201] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4207] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4213] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4219] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4225] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4231] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4237] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4243] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4249] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4255] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4261] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4267] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4273] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4279] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4285] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4291] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4297] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4303] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4309] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4315] "Saturday" "Saturday" "Saturday" "Saturday" "Saturday" "Saturday"
## [4321] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4327] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4333] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4339] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4345] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4351] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4357] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4363] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4369] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4375] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4381] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4387] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4393] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4399] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4405] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4411] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4417] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4423] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4429] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4435] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4441] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4447] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4453] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4459] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4465] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4471] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4477] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4483] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4489] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4495] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4501] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4507] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4513] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4519] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4525] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4531] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4537] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4543] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4549] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4555] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4561] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4567] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4573] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4579] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4585] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4591] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4597] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"  
## [4603] "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"   "Sunday"
```

```r
# activity_weekday <- transform(activity_weekday, interval = factor(interval))
# activity_weekday<-group_by(activity_weekday,interval)
# mean_steps_interval_weekday <- summarise(activity_weekday, steps = mean(steps,na.rm=TRUE))
# 
# activity_weekend<-subset(activity_noNAs,date_day==1 | date_day==7)
# activity_weekend <- transform(activity_weekend, interval = factor(interval))
# activity_weekend<-group_by(activity_weekend,interval)
# mean_steps_interval_weekend <- summarise(activity_weekend, steps = mean(steps,na.rm=TRUE))
```




