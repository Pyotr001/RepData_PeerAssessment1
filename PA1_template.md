# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
Code for reading in the dataset and/or processing the data

1. Load the data (i.e. 𝚛𝚎𝚊𝚍.𝚌𝚜𝚟())

```r
data <- read.csv(unzip("activity.zip"))
```

2. Process/transform the data (if necessary) into a format suitable for your analysis

Перводим время в POSIXlt


```r
data$date <- as.POSIXct(data$date)
```




## What is mean total number of steps taken per day?
Histogram of the total number of steps taken each day

1. Calculate the total number of steps taken per day

```r
steps_number <- tapply(data$steps, data$date, sum)
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
hist(steps_number, xlab = "number of steps", main = "the total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day

Mean 

```r
meanDay <- mean(tapply(data$steps, data$date, sum), na.rm = T)
print(meanDay)
```

```
## [1] 10766.19
```

Median

```r
 medianDay <- median(tapply(data$steps, data$date, sum), na.rm = T)
print(medianDay)
```

```
## [1] 10765
```


## What is the average daily activity pattern?
Mean and median number of steps taken each day
Time series plot of the average number of steps taken
The 5-minute interval that, on average, contains the maximum number of steps

1. Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
mean_interval <- tapply(data$steps, data$interval, mean, na.rm = T)

plot(mean_interval, type = "l", main = "time series plot", xlab = "the 5-minute interval", ylab = "the average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps? 

```r
which.max(mean_interval)
```

```
## 835 
## 104
```


## Imputing missing values
Code to describe and show a strategy for imputing missing data
Histogram of the total number of steps taken each day after missing values are imputed

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)

```r
stepsNA <- sum(is.na(data$steps))
dateNA <- sum(is.na(data$date))
intervalNA <- sum(is.na(data$interval))
```
Missing values in steps is 2304.

Missing values in date is 0.

Missing values in interval is 0


NA pattern explore

```r
table(is.na(data$steps), data$interval)
```

```
##        
##          0  5 10 15 20 25 30 35 40 45 50 55 100 105 110 115 120 125 130
##   FALSE 53 53 53 53 53 53 53 53 53 53 53 53  53  53  53  53  53  53  53
##   TRUE   8  8  8  8  8  8  8  8  8  8  8  8   8   8   8   8   8   8   8
##        
##         135 140 145 150 155 200 205 210 215 220 225 230 235 240 245 250
##   FALSE  53  53  53  53  53  53  53  53  53  53  53  53  53  53  53  53
##   TRUE    8   8   8   8   8   8   8   8   8   8   8   8   8   8   8   8
##        
##         255 300 305 310 315 320 325 330 335 340 345 350 355 400 405 410
##   FALSE  53  53  53  53  53  53  53  53  53  53  53  53  53  53  53  53
##   TRUE    8   8   8   8   8   8   8   8   8   8   8   8   8   8   8   8
##        
##         415 420 425 430 435 440 445 450 455 500 505 510 515 520 525 530
##   FALSE  53  53  53  53  53  53  53  53  53  53  53  53  53  53  53  53
##   TRUE    8   8   8   8   8   8   8   8   8   8   8   8   8   8   8   8
##        
##         535 540 545 550 555 600 605 610 615 620 625 630 635 640 645 650
##   FALSE  53  53  53  53  53  53  53  53  53  53  53  53  53  53  53  53
##   TRUE    8   8   8   8   8   8   8   8   8   8   8   8   8   8   8   8
##        
##         655 700 705 710 715 720 725 730 735 740 745 750 755 800 805 810
##   FALSE  53  53  53  53  53  53  53  53  53  53  53  53  53  53  53  53
##   TRUE    8   8   8   8   8   8   8   8   8   8   8   8   8   8   8   8
##        
##         815 820 825 830 835 840 845 850 855 900 905 910 915 920 925 930
##   FALSE  53  53  53  53  53  53  53  53  53  53  53  53  53  53  53  53
##   TRUE    8   8   8   8   8   8   8   8   8   8   8   8   8   8   8   8
##        
##         935 940 945 950 955 1000 1005 1010 1015 1020 1025 1030 1035 1040
##   FALSE  53  53  53  53  53   53   53   53   53   53   53   53   53   53
##   TRUE    8   8   8   8   8    8    8    8    8    8    8    8    8    8
##        
##         1045 1050 1055 1100 1105 1110 1115 1120 1125 1130 1135 1140 1145
##   FALSE   53   53   53   53   53   53   53   53   53   53   53   53   53
##   TRUE     8    8    8    8    8    8    8    8    8    8    8    8    8
##        
##         1150 1155 1200 1205 1210 1215 1220 1225 1230 1235 1240 1245 1250
##   FALSE   53   53   53   53   53   53   53   53   53   53   53   53   53
##   TRUE     8    8    8    8    8    8    8    8    8    8    8    8    8
##        
##         1255 1300 1305 1310 1315 1320 1325 1330 1335 1340 1345 1350 1355
##   FALSE   53   53   53   53   53   53   53   53   53   53   53   53   53
##   TRUE     8    8    8    8    8    8    8    8    8    8    8    8    8
##        
##         1400 1405 1410 1415 1420 1425 1430 1435 1440 1445 1450 1455 1500
##   FALSE   53   53   53   53   53   53   53   53   53   53   53   53   53
##   TRUE     8    8    8    8    8    8    8    8    8    8    8    8    8
##        
##         1505 1510 1515 1520 1525 1530 1535 1540 1545 1550 1555 1600 1605
##   FALSE   53   53   53   53   53   53   53   53   53   53   53   53   53
##   TRUE     8    8    8    8    8    8    8    8    8    8    8    8    8
##        
##         1610 1615 1620 1625 1630 1635 1640 1645 1650 1655 1700 1705 1710
##   FALSE   53   53   53   53   53   53   53   53   53   53   53   53   53
##   TRUE     8    8    8    8    8    8    8    8    8    8    8    8    8
##        
##         1715 1720 1725 1730 1735 1740 1745 1750 1755 1800 1805 1810 1815
##   FALSE   53   53   53   53   53   53   53   53   53   53   53   53   53
##   TRUE     8    8    8    8    8    8    8    8    8    8    8    8    8
##        
##         1820 1825 1830 1835 1840 1845 1850 1855 1900 1905 1910 1915 1920
##   FALSE   53   53   53   53   53   53   53   53   53   53   53   53   53
##   TRUE     8    8    8    8    8    8    8    8    8    8    8    8    8
##        
##         1925 1930 1935 1940 1945 1950 1955 2000 2005 2010 2015 2020 2025
##   FALSE   53   53   53   53   53   53   53   53   53   53   53   53   53
##   TRUE     8    8    8    8    8    8    8    8    8    8    8    8    8
##        
##         2030 2035 2040 2045 2050 2055 2100 2105 2110 2115 2120 2125 2130
##   FALSE   53   53   53   53   53   53   53   53   53   53   53   53   53
##   TRUE     8    8    8    8    8    8    8    8    8    8    8    8    8
##        
##         2135 2140 2145 2150 2155 2200 2205 2210 2215 2220 2225 2230 2235
##   FALSE   53   53   53   53   53   53   53   53   53   53   53   53   53
##   TRUE     8    8    8    8    8    8    8    8    8    8    8    8    8
##        
##         2240 2245 2250 2255 2300 2305 2310 2315 2320 2325 2330 2335 2340
##   FALSE   53   53   53   53   53   53   53   53   53   53   53   53   53
##   TRUE     8    8    8    8    8    8    8    8    8    8    8    8    8
##        
##         2345 2350 2355
##   FALSE   53   53   53
##   TRUE     8    8    8
```


```r
barplot(is.na(data$steps))
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->


2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.



3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
dataFix <- data
for (i in unique(dataFix$interval)) {
        dataFix$steps[is.na(dataFix$steps) & dataFix$interval == i] <- round(mean(dataFix$steps[data$interval == i], na.rm = T))
}
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
steps_numberFix <- tapply(dataFix$steps, dataFix$date, sum)

hist(steps_numberFix, xlab = "number of steps", main = "the total number of steps taken each day Fix")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
meanDayFix <- mean(steps_numberFix)
medianDayFix <- median(steps_numberFix)
```

```r
meanDay - meanDayFix
```

```
## [1] 0.549335
```

```r
medianDay - medianDayFix
```

```
## [1] 3
```


## Are there differences in activity patterns between weekdays and weekends?
Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

For this part the 𝚠𝚎𝚎𝚔𝚍𝚊𝚢𝚜() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
dataFix$weekD <- as.POSIXlt(data$date)$wday == 0 | as.POSIXlt(data$date)$wday == 6

dataFix$weekD <- factor(dataFix$weekD, levels = c(F, T), labels = c("weekday", "weekend"))
```


2. Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
stepsWeekD <- tapply(dataFix$steps[dataFix$weekD == "weekday"], dataFix$interval[dataFix$weekD == "weekday"], mean)

stepsWeekE <- tapply(dataFix$steps[dataFix$weekD == "weekend"], dataFix$interval[dataFix$weekD == "weekend"], mean)
```


```r
par(mfrow = c(2,1))
plot(stepsWeekD, type = "l", main = "weekdays"
     ,xlab = "the 5-minute interval"
     ,ylab = "the average number of steps")

plot(stepsWeekE, type = "l", main = "weekdends"
     ,xlab = "the 5-minute interval"
     ,ylab = "the average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png)<!-- -->



