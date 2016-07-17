# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

1. The steps below load the data. The original data in a zipped format is available at
[https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip). I unzipped it to a file `activity.csv`, and then used `read.csv()` to read the data into R.

```r
activity_data <- read.csv("activity.csv")
```

2. I use the `na.omit()` function to omit the data with `NA` values:

```r
activity_data_complete<-na.omit(activity_data)
```


## What is mean total number of steps taken per day?

First we use `group_by()` function, in the `dplyr` package, to group the data according to the date.

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
##group data according to date, store the result in by_day
by_day<-group_by(activity_data_complete, date) 

##calculate average number of steps by date, store result in total_step_by_day
total_step_by_day<-summarize(by_day, sum(steps))
```

Next we make a histogram of the total number of steps taken each day

```r
##make histogram
hist(total_step_by_day$'sum(steps)', breaks=30, xlab="steps", col="blue", main="Histogram of steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

The code below calculates the mean and median number of total steps per day.

```r
mean_daily_steps<-mean(total_step_by_day$'sum(steps)')
mean_daily_steps
```

```
## [1] 10766.19
```


```r
median_daily_steps<-median(total_step_by_day$'sum(steps)')
median_daily_steps
```

```
## [1] 10765
```

The average number of steps taken per day is 10766.19, and the median number of steps per day is 10765.

## What is the average daily activity pattern?
1. First we group the data by interval 

```r
by_interval<-group_by(activity_data_complete, interval)
```
Then we calculated the average daily number of steps, grouped by interval, and plot the result.

```r
mean_steps_by_interval<-summarize(by_interval, mean(steps))
plot(mean_steps_by_interval, type="l", main="Average Daily Steps by time interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

2. The following code determines which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps. 

```r
##determine which row contains the maximum of the number of step
max_steps_row<-which.max(mean_steps_by_interval$'mean(steps)') 

## determine interval corresponding to row found above, store result in max_steps_interval
max_steps_interval<-mean_steps_by_interval[max_steps_row,]$interval 
max_steps_interval
```

```
## [1] 835
```

The 835 interval contains the maximum number of steps.

## Imputing missing values

1. The following code calculate the total number of missing values in the dataset (i.e. the total number of rows with `NA`'s)


```r
missing_data<-sum(is.na(activity_data$steps))
missing_data
```

```
## [1] 2304
```

The number of missing values is 2304.

2. I decided to replace the missing `NA`'s by the mean for the corresponding 5-minute interval. The  code below does this by looping through each observation in `activity_data`.
3. It creates a new dataset `activity_data_impute` that is equal to the original dataset but with the missing data filled in.



```r
## activity_data_impute will contain the filled in values; initialize it to activity_date
activity_data_impute<-activity_data

## calculate the total number of observations
num_obs<-length(activity_data$steps)

## loop through all observations, replace with mean for interval if steps == NA
for(i in 1:num_obs){
  if (is.na(activity_data$steps[i])) ## check if the index i has NA in steps variable 
    {
      ##find interval corresponding to index i
      interval_of_i<-activity_data$interval[i]
      
      ##find row in mean_steps_by_interval corresponding to the interval of index i
      row_of_interval_of_i<-match(interval_of_i, mean_steps_by_interval$interval) 
      
      ## replace steps of index i observation with mean number of steps for the corresponding interval
      activity_data_impute$steps[i]<-mean_steps_by_interval$'mean(steps)'[row_of_interval_of_i]
  }
}
```


4. Using the filled in data, I make a histogram of the total number of steps taken each day: 


```r
## Group filled in data by date
by_day_impute<-group_by(activity_data_impute, date)

## Calculate total number of steps by date
total_step_by_day_impute<-summarize(by_day_impute, sum(steps))

## make histogram of total number of steps by date
hist(total_step_by_day_impute$'sum(steps)', breaks=30, xlab="steps", col="blue", main="Histogram of steps per day by interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

The code below calculates and the mean and median total number of steps taken per day for the filled in data.  

```r
mean_daily_steps_impute<-mean(total_step_by_day_impute$'sum(steps)')
mean_daily_steps_impute
```

```
## [1] 10766.19
```
The mean number of daily steps for the filled in data is 10766.19



```r
median_daily_steps_impute<-median(total_step_by_day_impute$'sum(steps)')
median_daily_steps_impute
```

```
## [1] 10766.19
```
The median number of daily steps for the filled in data is 10766.19.

The mean number of steps per day for the original data and the filled in data are the same. The median number of steps per day has increased by about `1` step. There does not seem to be a great impact of imputing missing data on the estimates of the total daily number of steps.

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor dataset `type_of_day` with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
## use weekdays to determine day of week in filled in data activity_data_impute
days<-weekdays(as.Date(activity_data_impute$date))

##initialize to type of day to be same as days; we will replace in next for loop
type_of_day<-days

## loop through each day in days vector, determine if it a weekend or weekday, and update type_of_day accordingly
for(i in 1:num_obs){
  if(days[i]=="Sunday" | days[i]=="Saturday")
  {
    type_of_day[i]<-"weekend"
  }
  else type_of_day[i]<-"weekday"
}

## make type_of day a factor vector
type_of_day<-as.factor(type_of_day)
```
2. I make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
##attach type_of_day to filled in activity data
activity_data_impute<-cbind(activity_data_impute, type_of_day)

## extract filled in activity data on weekdays
activity_data_impute_weekday<-filter(activity_data_impute, type_of_day=='weekday')

##extract filled in activity data on weekends
activity_data_impute_weekend<-filter(activity_data_impute, type_of_day=='weekend')

##group weekday and weekend data by interval
by_interval_weekday<-group_by(activity_data_impute_weekday, interval)
by_interval_weekend<-group_by(activity_data_impute_weekend, interval)

## compute mean steps by interval for weekday and weekend
mean_steps_interval_weekday<-summarise(by_interval_weekday, mean(steps))
mean_steps_interval_weekend<-summarise(by_interval_weekend, mean(steps))
```


```r
##make plots of mean steps for weekend and weekday
par(mfcol=c(2,1))
plot(mean_steps_interval_weekend, type="l", main="Average Daily Steps on weekends by time interval", ylab="Number of steps")
plot(mean_steps_interval_weekday, type="l", main="Average Daily Steps on weekends by time interval", ylab="Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
