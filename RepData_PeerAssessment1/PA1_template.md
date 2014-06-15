Peer Assessment nº1 - Reproducible Research
========================================================
by Javier Martínez

## Loading and preprocessing the data

**Load the data: [Activity monitoring data](https://d396qusza40orc.cloudfront.
                                            net/repdata%2Fdata%2Factivity.zip)**

For the first step of the assessment we need:
* Get the url where we can find the data
* Download the data, in this case data are in a .zip file
* Unzip the file in the selected destiny
* Load the data in a dataframe
* No need to preprocess the data fro our final purpose

```r
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
dest <- getwd()
dest <- paste(dest, sep = "", "/repdata-data-activity.zip")
download.file(url, destfile = dest, method = "curl")
unzip(dest, files = "activity.csv")
orig <- getwd()
orig <- paste(orig, sep = "", "/activity.csv")
model <- read.csv(orig, header = T)
```



## What is mean total number of steps taken per day?


**Make a histogram of the total number of steps taken each day**
  
  In this histogram we can see that there are a lot of days in which no steps were
  taken because of the device was not used or because of no activity for the user, 
  less probably.The distribution shows asymmetry due to this reason although if
  we ignore this fact we could see a kind of bimodal distribution or even some kind 
  of positive asymmetry. We could think that maybe there are some days where 
  the user makes another activity in his daily routine or take 
  the public transport or go to work in car or maybe a mix of all of them. 
  

```r
hist(tapply(model$steps, model$date, sum, na.rm = TRUE), breaks = 50, ylab = "Days", 
    xlab = "Steps", main = "Histogram of Steps per Day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

  
  
  We can observe also when we launch the tapply function that there are two 
  phases, the first one where the user discover the device and afterwards a 
  decline of the activity and another period next where the user gets the best
  results and the subsequent decline as well.

```r
tapply(model$steps, model$date, sum)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##         NA        126      11352      12116      13294      15420 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##      11015         NA      12811       9900      10304      17382 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##      12426      15098      10139      15084      13452      10056 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##      11829      10395       8821      13460       8918       8355 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##       2492       6778      10119      11458       5018       9819 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##      15414         NA      10600      10571         NA      10439 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##       8334      12883       3219         NA         NA      12608 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##      10765       7336         NA         41       5441      14339 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##      15110       8841       4472      12787      20427      21194 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##      14478      11834      11162      13646      10183       7047 
## 2012-11-30 
##         NA
```



**Calculate and report the mean and median total number of steps taken per day**
  
  Although the greater median than the mean would show a negative asymmetry,
  is very difficult to accept this fact because of the huge number of days 
  with 0 steps and the big number of missing values also. We would expect 
  some kind of asymmetry maybe in the inverse side due to it is more probably
  more days with less steps that with more of them in the average of population.
  I think that the missing values makes the distribution biased at the right side.
  

```r
c <- tapply(model$steps, model$date, sum, na.rm = T)

median(c)
```

```
## [1] 10395
```

```r

mean(c)
```

```
## [1] 9354
```



## What is the average daily activity pattern?


**Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
and the average number of steps taken, averaged across all days (y-axis)**

  The plot shows the interval of the day where are taken the greater number of
  steps maybe that is due to the momment of the day in which the user go to work
  We could think that the user go by walk to work or maybe he goes to have a lunch
  and afterwards he pass a period resting, continues with his normal activity 
  during the day and he comes back at home by bus or with a fellow, or maybe 
  he comes back in his own car.
  

```r
mean_steps_per_interval <- tapply(model$steps, model$interval, mean, na.rm = T)

ts.plot(mean_steps_per_interval, ylab = "Mean_Steps per 5 minutes interval", 
    xlab = "Time", main = "Average number of steps taken in a 5 minutes interval")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 



**Which 5-minute interval, on average across all the days in the dataset, 
contains the maximum number of steps?**
  
  The 835th interval is in which the user takes the greatest number of steps.
  If we divide this number between 60 we get approx. 14, and maybe this hour 
  corresponds with the lunch time, depends on the momment in which we start 
  to count intervals of 5 minutes.
  

```r
m_steps <- tapply(model$steps, model$interval, mean, na.rm = T)

m_steps[which(m_steps == max(m_steps))]
```

```
##   835 
## 206.2
```



## Imputing missing values


**Calculate and report the total number of missing values in the dataset 
(i.e. the total number of rows with NAs)**

  There are a huge a mount of missing values maybe tht is the reason because
  we have obtained a greater median than mean.  

```r
bad <- is.na(model[, ])
na_value <- model[which(bad[, ] == TRUE), ]

nrow(na_value)
```

```
## [1] 2304
```



**Devise a strategy for filling in all of the missing values in the dataset. 
The strategy does not need to be sophisticated. For example, you could use 
the mean/median for that day, or the mean for that 5-minute interval, etc.**

  I use the mean of each interval to refill the missing values in it.

```r
model1 <- model
mean.int <- tapply(model$steps, model$interval, mean, na.rm = T)
inter <- as.numeric(levels(factor(model$interval)))
mean.int <- as.data.frame(cbind(inter, mean.int))
colnames(mean.int) <- c("interval", "means")
```



**Create a new dataset that is equal to the original dataset but with the 
missing data filled in**
  
  I make a loop to create the new dataset with the missing values refilled.

```r
for (i in 1:nrow(model)) {
    if (is.na(model[i, 1])) {
        interval <- model[i, 3]
        media <- mean.int[which(mean.int[, 1] == interval), 2]
        model1[i, 1] <- media
    }
}
```



**Make a histogram of the total number of steps taken each day and calculate 
and report the mean and median total number of steps taken per day. 
Do these values differ from the estimates from the first part of the assignment? 
What is the impact of imputing missing data on the estimates of the total 
daily number of steps?**

  We observe a more simmetric distribution although with some kurtosis and 
  big tails, making more probably extreme values than in a more normal distribution.
  

```r
hist(tapply(model1$steps, model1$date, sum, na.rm = TRUE), breaks = 50, ylab = "Days", 
    xlab = "Steps", main = "Histogram of Steps per Day - No Missing Values")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 



## Are there differences in activity patterns between weekdays and weekends?


**Create a new factor variable in the dataset with two levels – “weekday” 
and “weekend” indicating whether a given date is a weekday or weekend day**

  I'm come from Spain so my weekends days are "sábado" and "domingo" as 
  "saturday" and "sunday" in english spoken countries.
  

```r
days <- factor(weekdays(as.Date(model[, 2])))
days <- as.character(days)
for (i in 1:length(days)) {
    
    if (days[i] == "sábado" | days[i] == "domingo") {
        
        days[i] <- "weekend"
    } else {
        days[i] <- "weekday"
    }
    
}
```



**Make a panel plot containing a time series plot (i.e. type = "l") of the 
5-minute interval (x-axis) and the average number of steps taken, averaged 
across all weekday days or weekend days (y-axis). The plot should look something
like the following, which was creating using simulated data:**

  In this plot we observe how during weekend the activity is realized
  during all day, and more constantly. We can see also how the activity starts 
  later and during the weekdays the activity starts before and the 
  greatest one is taken at the beginning and tends to reduce
  during the day.


```r
par(mfrow = c(2, 1))
for (type in c("weekend", "weekday")) {
    steps.type <- aggregate(steps ~ interval, data = model, subset = days == 
        type, FUN = mean)
    plot(steps.type, type = "l", main = type)
}
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 


