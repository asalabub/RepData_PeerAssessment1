---
title: "Reproducible Research: Peer Assessment 1"
output: 
       html_document: 
         keep_md: true
---


## Loading and preprocessing the data




```r
data <- read.csv("activity.csv")

library(dplyr)
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
library(tidyverse)
```

```
## -- Attaching packages ---------------------------------------------- tidyverse 1.3.0 --
```

```
## v ggplot2 3.3.0     v purrr   0.3.4
## v tibble  3.0.1     v stringr 1.4.0
## v tidyr   1.0.3     v forcats 0.5.0
## v readr   1.3.1
```

```
## -- Conflicts ------------------------------------------------- tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(ggplot2)
```

## What is mean total number of steps taken per day?

```r
steps_day <- data %>%
              group_by(date) %>%
              summarise(total = sum(steps, na.rm = T))

mean_steps <- mean(steps_day$total)
median_steps <- median(steps_day$total)
```

**The mean number of steps taken per day was 9354.2295082, and the median was 10395.**


## What is the average daily activity pattern?


```r
steps_interval <- data %>%
       group_by(interval) %>%
       summarise(average = mean(steps, na.rm = T))
```


### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maxim <- steps_interval[which(steps_interval$average == max(steps_interval$average)),]
```


**The interval with the highest mean number of steps is 835, 206.1698113.**


## Imputing missing values

### Number of incomplete rows



```r
incomplete <- sum(!complete.cases(data))
```

**In the original dataset there were a total of 2304 incomplete rows. The strategy was to fill the NAs with the average of steps taken during the same interval in other days.**


```r
data2 <- data

data2$steps[is.na(data2$steps)] <- ave(data2$steps,
                                 data2$interval,
                                 FUN=function(x)median(x,
                                                     na.rm = T))[is.na(data2$steps)]
```

### Total number of steps taken each day


```r
steps_day2 <- data2 %>%
       group_by(date) %>%
       summarise(total = sum(steps, na.rm = T),
                 mean = mean(steps, na.rm = T),
                 median = median(steps, na.rm = T))

print(steps_day2)
```

```
## # A tibble: 61 x 4
##    date       total   mean median
##    <chr>      <int>  <dbl>  <dbl>
##  1 2012-10-01  1141  3.96       0
##  2 2012-10-02   126  0.438      0
##  3 2012-10-03 11352 39.4        0
##  4 2012-10-04 12116 42.1        0
##  5 2012-10-05 13294 46.2        0
##  6 2012-10-06 15420 53.5        0
##  7 2012-10-07 11015 38.2        0
##  8 2012-10-08  1141  3.96       0
##  9 2012-10-09 12811 44.5        0
## 10 2012-10-10  9900 34.4        0
## # ... with 51 more rows
```

```r
hist(steps_day2$total, main = "Total steps per day", xlab = "count", 
     ylab = "frequency", col = "lightblue", breaks = 50)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
mean_noNA <- mean(steps_day2$total)
median_noNA <- median(steps_day2$total)


plot(steps_interval$interval, steps_interval$average, type = "l",
     xlab = "Interval", ylab = "Steps average", main = "Average of steps per interval",
     col = "darkblue", lwd = 2)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-2.png)<!-- -->

**Without missing values, the mean number of steps taken per day was 9503.8688525 and the median was 10395.**


### Differences between data with and without NAs


```r
toplot2 <- data.frame(test = c("mean", "median", "mean", "median"),
                      ddbb = c("withNA", "withNA", "withoutNA",  "withoutNA"), 
                      value = c(mean(steps_day$total), median(steps_day$total),
                                mean(steps_day2$total), median(steps_day2$total)))

ggplot(toplot2, aes(x = ddbb, y = value, col = test)) + geom_point()
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->


**As the plot shows, there are differences in the values between the datasets with and without missing values.**


## Are there differences in activity patterns between weekdays and weekends?



```r
data2$date <- as.Date(data2$date, "%Y-%m-%d")

data2$days <- weekdays(data2$date)
data2$days <- as.factor(data2$days)
data2$week <- recode(data2$days, 
                     "lunes" = "weekday", "martes" = "weekday",
                     "miércoles" = "weekday", "jueves" = "weekday",
                     "viernes" = "weekday", "sábado" = "weekend",
                     "domingo" = "weekend")

Days <- data2 %>%
                group_by(week) %>%
                        summarise(mean = mean(steps, na.rm = T),
                        median = median(steps, na.rm = T))

print(Days)
```

```
## # A tibble: 2 x 3
##   week     mean median
##   <fct>   <dbl>  <dbl>
## 1 weekend  38.2      0
## 2 weekday  31.2      0
```

```r
int_days <- data2 %>%
        group_by(week, interval) %>%
        summarise(mean = mean(steps, na.rm = T))

ggplot(data = int_days, aes(interval, mean)) + 
        geom_line(col = "darkblue", lwd = 0.75) +
        facet_grid(rows = vars(week)) +
        labs(title = "Mean steps taken by interval on weekdays and weekends",
             x = "Interval", 
             y = "Number of steps") +
        theme(plot.title = element_text(hjust = 0.5))
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

**Yes, there are differences in the patterns of activity between weekdays and weekends. Weekends tend to have higher activity and more evenly distributed throughtout the day, whereas weekdays peak before midday and then the activity reduces and stays very low until the late afternoon.**

