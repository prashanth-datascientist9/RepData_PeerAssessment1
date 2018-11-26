---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



###Code for reading in the dataset and/or processing the data

Show any code that is needed to

1. Load the data (i.e., read.csv())
2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
library(data.table)
library(ggplot2)

if(!file.exists("repdata%2Fdata%2Factivity.zip")) {
  temp <- tempfile()
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", temp)
  file <- unzip(temp)
  unlink(temp)
} else {
  file <- unzip("repdata%2Fdata%2Factivity.zip")
}

df <- read.csv(file, header = TRUE, sep = ",")
head(df, nrow = 5)
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

###What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day
2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
3. Calculate and report the mean and median of the total number of steps taken per day


```r
# Aggregate the steps by date and get a sum of those steps
aggdf <- aggregate(df$steps~df$date, df, sum)
hist(aggdf$`df$steps`, col = 2, xlab = "Total Steps per day", main = "Histogram of total steps in a day")
```

![](figure/fig-unnamed-chunk-2-1.png)<!-- -->

```r
#Mean of total steps per day
mean(aggdf$`df$steps`, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
#Median of total steps per day
median(aggdf$`df$steps`, na.rm = TRUE)
```

```
## [1] 10765
```

###What is the average daily activity pattern?
1. Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
aggdf_steps_int <- aggregate(df$steps ~ df$interval, df, mean, na.action = na.omit)

plot(aggdf_steps_int$`df$interval`, aggdf_steps_int$`df$steps`, col = 2, type = "l",
     xlab = "Interval", ylab = "Average Steps", main = "Average steps in a day, averaged across all days")
```

![](figure/fig-unnamed-chunk-3-1.png)<!-- -->

```r
avg_max_steps_id <- which.max(aggdf_steps_int$`df$steps`)

aggdf_steps_int[avg_max_steps_id, ]
```

```
##     df$interval df$steps
## 104         835 206.1698
```

```r
print(paste("The interval with the max number of steps is ", 
            aggdf_steps_int[avg_max_steps_id, ]$`df$interval`,
            " and the number if steps for that interval is ", 
            round(aggdf_steps_int[avg_max_steps_id, ]$`df$steps`)))
```

```
## [1] "The interval with the max number of steps is  835  and the number if steps for that interval is  206"
```

###Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
missing_values <- df[!complete.cases(df),]
nrow(missing_values)
```

```
## [1] 2304
```

```r
for(i in 1:nrow(df)) {
  if(is.na(df$steps[i])) {
    steps_value <- aggdf_steps_int$`df$steps`[which(aggdf_steps_int$`df$interval` == df$interval[i])]
    df$steps[i] <- steps_value
  }
}

#Aggregate the steps per day with the imputed values
steps_per_day <- aggregate(steps ~ date, df, sum)

#Draw a histogram of the imputed values
hist(steps_per_day$steps, col = 4, main = "Total number of steps per day Imputed", xlab = "Steps per day")
```

![](figure/fig-unnamed-chunk-4-1.png)<!-- -->

```r
#Mean of the imputed values (steps)
mean(steps_per_day$steps)
```

```
## [1] 10766.19
```

```r
#Median of the imputed values (steps)
median(steps_per_day$steps)
```

```
## [1] 10766.19
```
#####We note that there is NO IMPACT of imputing missing date on the estimates of the total daily number of steps.

###Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
2. Make a panel plot containing a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
week_day_check <- function(date_value) {
  weekday <- weekdays(as.Date(date_value, '%Y-%m-%d'))
  if(!(weekday == 'Saturday' || weekday == 'Sunday')) {
    day_type <- 'Weekday'
  } else {
    day_type <- 'Weekend'
  }
  day_type
}

df$day_type <- as.factor(sapply(df$date, week_day_check))

steps_per_day <- aggregate(steps ~ interval+day_type, df, mean)

newplot <- ggplot(steps_per_day, aes(interval, steps)) +
  geom_line(stat = "identity", aes(colour = day_type)) +
  theme_light() +
  facet_grid(day_type ~ ., scales = "fixed", space = "fixed") +
  labs(x = "Interval", y = expression("Number of Steps")) +
  ggtitle("Number of Steps per Interval by Weekday/Weekend")

print(newplot)
```

![](figure/fig-unnamed-chunk-5-1.png)<!-- -->

##### We do notice that there is a slight difference in the user's steps on weekends compared to weekdays. His/her steps are comparitively fewer on the weekends. 
