---
title: 'Reproducible Research: Peer Assessment 1'
date: "2025-09-24"
output: html_document
---




##Loading and preprocessing the data

```r
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readxl)

setwd("C:/Users/golnaz/OneDrive/Documents/Agro-ecosystem/RepData_PeerAssessment1/")
# Read the data (CSV must be in the working directory)
activity <- read.csv("activity.csv")
list.files()
```

```
## [1] "activity"                      "activity.csv"                 
## [3] "activity.zip"                  "doc"                          
## [5] "instructions_fig"              "PA1_template.html"            
## [7] "PA1_template.Rmd"              "README.md"                    
## [9] "RepData_PeerAssessment1.Rproj"
```

```r
# Convert date to Date
activity$date <- as.Date(activity$date)

# Inspect structure
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```



```r
# Keep only non-NA rows for steps
activity_no_na <- subset(activity, !is.na(steps))

# Total steps per day
steps_per_day <- aggregate(steps ~ date, data = activity_no_na, sum)

# Histogram
hist(steps_per_day$steps,
     main = "Total Steps per Day",
     xlab = "Total steps",
     ylab = "Frequency")
```

![plot of chunk unnamed-chunk-31](figure/unnamed-chunk-31-1.png)

```r
# Mean and median
mean_steps <- mean(steps_per_day$steps)
median_steps <- median(steps_per_day$steps)

mean_steps
```

```
## [1] 10766.19
```

```r
median_steps
```

```
## [1] 10765
```



```r
# Average steps for each 5-minute interval across all days
avg_by_interval <- aggregate(steps ~ interval, data = activity_no_na, mean)

# Time series plot
plot(avg_by_interval$interval, avg_by_interval$steps, type = "l",
     xlab = "5-minute interval", ylab = "Average steps",
     main = "Average Daily Activity Pattern")
```

![plot of chunk unnamed-chunk-32](figure/unnamed-chunk-32-1.png)

```r
# 5-minute interval with the maximum average steps
max_row <- avg_by_interval[which.max(avg_by_interval$steps), ]
max_row
```



```r
# Total number of rows with NA in steps
total_na <- sum(is.na(activity$steps))
total_na
```

```
## [1] 2304
```


```r
# Compute mean steps per interval (ignoring NAs)
interval_means <- aggregate(steps ~ interval, data = activity, FUN = function(x) mean(x, na.rm = TRUE))
names(interval_means)[2] <- "interval_mean"

# Merge to attach the interval mean to each row
activity_imputed <- merge(activity, interval_means, by = "interval", all.x = TRUE)

# Replace NA steps with interval_mean
activity_imputed$steps <- ifelse(is.na(activity_imputed$steps),
                                 activity_imputed$interval_mean,
                                 activity_imputed$steps)

# Drop helper column
activity_imputed$interval_mean <- NULL

# Sanity check: no NAs remain
sum(is.na(activity_imputed$steps))
```

```
## [1] 0
```



```r
# Total steps per day after imputation
steps_per_day_imp <- aggregate(steps ~ date, data = activity_imputed, sum)

# Histogram
hist(steps_per_day_imp$steps,
     main = "Total Steps per Day (After Imputation)",
     xlab = "Total steps",
     ylab = "Frequency")
```

![plot of chunk unnamed-chunk-35](figure/unnamed-chunk-35-1.png)

```r
# Mean and median after imputation
mean_steps_imp <- mean(steps_per_day_imp$steps)
median_steps_imp <- median(steps_per_day_imp$steps)

mean_steps_imp
```

```
## [1] 10766.19
```

```r
median_steps_imp
```

```
## [1] 10766.19
```

```r
# Compare to original
c(original_mean = mean_steps,
  imputed_mean  = mean_steps_imp,
  original_median = median_steps,
  imputed_median  = median_steps_imp,
  mean_difference  = mean_steps_imp - mean_steps,
  median_difference = median_steps_imp - median_steps)
```

```
##     original_mean      imputed_mean   original_median    imputed_median 
##      10766.188679      10766.188679      10765.000000      10766.188679 
##   mean_difference median_difference 
##          0.000000          1.188679
```




```r
# Create factor for weekday/weekend (locale-independent)
wday <- as.POSIXlt(activity_imputed$date)$wday  # 0 = Sunday, 6 = Saturday
activity_imputed$day_type <- ifelse(wday %in% c(0, 6), "weekend", "weekday")
activity_imputed$day_type <- factor(activity_imputed$day_type, levels = c("weekday", "weekend"))

# Average steps by interval and day_type
avg_by_interval_daytype <- aggregate(steps ~ interval + day_type, data = activity_imputed, mean)

# Panel plot using base graphics
par(mfrow = c(2,1), mar = c(4,4,2,1))
with(subset(avg_by_interval_daytype, day_type == "weekday"),
     plot(interval, steps, type = "l", main = "Weekday", xlab = "Interval", ylab = "Average steps"))
with(subset(avg_by_interval_daytype, day_type == "weekend"),
     plot(interval, steps, type = "l", main = "Weekend", xlab = "Interval", ylab = "Average steps"))
```

![plot of chunk unnamed-chunk-36](figure/unnamed-chunk-36-1.png)

```r
par(mfrow = c(1,1))
```

