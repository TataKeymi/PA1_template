---
title: "PA1_template.Rmd"
author: "TataKeymi"
date: "2023-12-10"
output: html_document
---

```r
knitr::opts_chunk$set(echo = TRUE)
```


```r
library(readr)
```

```
## Warning: package 'readr' was built under R version 4.3.2
```

```r
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
library(ggplot2)
```


```r
library(lubridate)
```

```
## Warning: package 'lubridate' was built under R version 4.3.2
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```
##Loading and preprocessing the data


```r
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
destfile <- "./data/05_assignment1_data.zip"
download.file(url, destfile)
unzip("./data/05_assignment1_data.zip", list = TRUE)
```

```
##           Name Length                Date
## 1 activity.csv 350829 2014-02-11 10:08:00
```


```r
activity_df <- read_csv(unzip("./data/05_assignment1_data.zip", "activity.csv", exdir = "./data"))
```

```
## Rows: 17568 Columns: 3
## ── Column specification ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
## Delimiter: ","
## dbl  (2): steps, interval
## date (1): date
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```
##What is the mean total number of steps taken per day?

```r
str(activity_df)
```

```
## spc_tbl_ [17,568 × 3] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
##  $ steps   : num [1:17568] NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date[1:17568], format: "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: num [1:17568] 0 5 10 15 20 25 30 35 40 45 ...
##  - attr(*, "spec")=
##   .. cols(
##   ..   steps = col_double(),
##   ..   date = col_date(format = ""),
##   ..   interval = col_double()
##   .. )
##  - attr(*, "problems")=<externalptr>
```

```r
head(activity_df)
```

```
## # A tibble: 6 × 3
##   steps date       interval
##   <dbl> <date>        <dbl>
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
steps_day <- activity_df %>% 
        group_by(date) %>% 
        summarise(
        steps = sum(steps)
)
steps_day
```

```
## # A tibble: 61 × 2
##    date       steps
##    <date>     <dbl>
##  1 2012-10-01    NA
##  2 2012-10-02   126
##  3 2012-10-03 11352
##  4 2012-10-04 12116
##  5 2012-10-05 13294
##  6 2012-10-06 15420
##  7 2012-10-07 11015
##  8 2012-10-08    NA
##  9 2012-10-09 12811
## 10 2012-10-10  9900
## # ℹ 51 more rows
```

```r
summary(steps_day$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10765   10766   13294   21194       8
```

```r
ggplot(steps_day, aes(x=steps)) + 
        geom_histogram(bins = 22, color = "black", fill = "steelblue") + 
        ggtitle("Distribution of total steps per day")
```

```
## Warning: Removed 8 rows containing non-finite values (`stat_bin()`).
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)
##What is the average daily activity pattern?

```r
by_interval <- activity_df %>%
        group_by(interval) %>%
        summarise(
                steps = mean(steps, na.rm = TRUE)
        )
by_interval[which.max(by_interval$steps), ]
```

```
## # A tibble: 1 × 2
##   interval steps
##      <dbl> <dbl>
## 1      835  206.
```


```r
ggplot(by_interval, aes(x=interval, y=steps)) + 
        geom_line(color = "steelblue") + 
        ggtitle("Average steps per 5-minute interval") + 
        scale_x_continuous(breaks=seq(0,2400,300))
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)
##Imputing missing values

```r
sum(is.na(activity_df$steps))
```

```
## [1] 2304
```


```r
replaced_missing <- activity_df %>%
        group_by(interval) %>%
        mutate_if(is.numeric,
                 function(x) ifelse(is.na(x),
                                    mean(x, na.rm = TRUE),
                                    x)) 
```

```
## `mutate_if()` ignored the following grouping variables:
## • Column `interval`
```

```r
steps_day_no_na <- replaced_missing %>%
        ungroup(interval) %>%
        group_by(date) %>%
        summarise(
        steps = sum(steps)
)
summary(steps_day_no_na$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```

```r
ggplot(steps_day_no_na, aes(x=steps)) + 
        geom_histogram(bins = 22, color = "black", fill = "steelblue") + 
        ggtitle("Distribution of total steps per day with imputed missing values")
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png)
##Are there differences in activity patterns between weekdays and weekends?

```r
replaced_missing_wd <- replaced_missing %>%
        mutate(day_of_week = wday(date, week_start = 1)) %>%
        mutate(day_of_week = ifelse(day_of_week < 6, "weekday", "weekend"))
```

```r
by_interval_wd <- replaced_missing_wd %>%
        group_by(interval, day_of_week) %>%
        summarise(
                steps = mean(steps, na.rm = TRUE)
        )
```

```
## `summarise()` has grouped output by 'interval'. You can override using the `.groups` argument.
```

```r
ggplot(by_interval_wd, aes(x=interval, y=steps, group=day_of_week)) +
        geom_line(color = "steelblue") + 
        ggtitle("Average steps per 5-minute interval") + 
        scale_x_continuous(breaks=seq(0,2400,300)) +
        facet_grid(day_of_week ~ .)
```

![plot of chunk unnamed-chunk-19](figure/unnamed-chunk-19-1.png)



