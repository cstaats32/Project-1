# Activity Monitoring Data Project
Cathy Staats  
March 9, 2017  



#### Read in activity monitoring dataset and produce print a summary


```r
activity <- read.csv("activity.csv")
summary(activity)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

#### Summarize steps by date


```r
library(dplyr)
StepsbyDate <- select(activity, date, steps) %>%
             group_by(date) %>%
             summarize(steps = sum(steps, na.rm=TRUE))
dim(StepsbyDate)
```

```
## [1] 61  2
```

### Plot histogram of number of days by steps


```r
library(ggplot2)
ggplot(StepsbyDate, aes(steps)) + 
        geom_histogram(binwidth = 1000) +
        scale_x_continuous("Total Daily Steps") +
        scale_y_continuous("Number of Days") +
        ggtitle("Days by Number of Steps") 
```

![](figure/plothist-1.png)<!-- -->

### Display mean and median of steps taken by day

```r
mean(StepsbyDate$steps, na.rm = TRUE)
```

```
## [1] 9354.23
```

```r
median(StepsbyDate$steps, na.rm = TRUE)
```

```
## [1] 10395
```

#### Summarize data to get average steps by 5-minute interval

```r
library(dplyr)
StepsbyInt <- select(activity, interval, steps) %>%
             group_by(interval) %>%
             summarize(steps = mean(steps, na.rm = TRUE))
```
### Plot average steps taken by 5-minute interval averaged across days


```r
library(ggplot2)
ggplot(StepsbyInt, aes(interval, steps)) + 
     geom_line() +
     scale_x_continuous("5-minute Interval of Day") +
     scale_y_continuous("Average Steps") +
     ggtitle("Steps by 5-Minute Interval") 
```

![](figure/plotbyint-1.png)<!-- -->

### Determine which interval has the max number of steps


```r
maxint <- which.max(StepsbyInt$steps)
StepsbyInt$interval[maxint]
```

```
## [1] 835
```

#### Determine number of missing values in data


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

### Inpute missing data
Split dataframe into two: rows with missing data and
rows without missing data. Replace missing data with average based
on the average for that 5-minute interval across all of the days. Using sqldf to join based on interval.


```r
library(dplyr)
NArows <- filter(activity, is.na(steps))
dim(NArows)
```

```
## [1] 2304    3
```

```r
NotNArows <- filter(activity, !is.na(steps))
dim(NotNArows)
```

```
## [1] 15264     3
```


```r
StepsbyInt <- transform(StepsbyInt, 
                steps = as.integer(round(steps, digits = 0)))
library(sqldf)

join_string <- "select
                StepsbyInt.steps,
                NArows.date,
                NArows.interval
from NArows
left join StepsbyInt
on NArows.interval = StepsbyInt.interval"

NAfilled <- sqldf(join_string,stringsAsFactors = FALSE)

## Combine with rows that did not having missing values 
activitynm <- as.data.frame(rbind(NAfilled, NotNArows))
dim(activitynm)
```

```
## [1] 17568     3
```

```r
## double check that there are not no missing values
sum(is.na(activitynm$steps))
```

```
## [1] 0
```

#### Summarize steps by date using data after imputing missing values


```r
library(dplyr)
StepsbyDate2 <- select(activitynm, date, steps) %>%
             group_by(date) %>%
             summarize(steps = sum(steps))
dim(StepsbyDate2)
```

```
## [1] 61  2
```

### Plot histogram of number of days by steps using data after imputing missing values


```r
library(ggplot2)
ggplot(StepsbyDate2, aes(steps)) + 
        geom_histogram(binwidth = 1000) +
        scale_x_continuous("Total Daily Steps") +
        scale_y_continuous("Number of Days") +
        ggtitle("Days by Number of Steps") 
```

![](figure/plothistnm-1.png)<!-- -->

#### Display mean and median of steps taken by day before imputation
  

```r
mean(StepsbyDate$steps, na.rm = TRUE)
```

```
## [1] 9354.23
```

```r
median(StepsbyDate$steps, na.rm = TRUE)
```

```
## [1] 10395
```

#### Display mean and median of steps taken by day after imputation

```r
mean(StepsbyDate2$steps)
```

```
## [1] 10765.64
```

```r
median(StepsbyDate2$steps)
```

```
## [1] 10762
```

#### Display differnce in mean and median of steps taken by day
####  after imputing missing values minus before

```r
mean(StepsbyDate2$steps) - mean(StepsbyDate$steps, na.rm = TRUE)
```

```
## [1] 1411.41
```

```r
median(StepsbyDate2$steps) - median(StepsbyDate$steps, na.rm = TRUE)
```

```
## [1] 367
```
##### Mean number of steps increased by about 1400 steps while median increased by 367 steps.

#### Add weekday / weekend as a factor variable to the dataset

```r
library(dplyr)

activitynm <- transform(activitynm, date = as.Date(date))

activitynm <- 
  mutate(activitynm, dt = weekdays(date))

Weekends <- 
   filter(activitynm, dt == 'Sunday' | dt == 'Saturday') %>%
   mutate (dt = 'Weekend')

Weekdays <- 
   filter(activitynm, dt != 'Sunday' & dt != 'Saturday') %>%
   mutate (dt = 'Weekday')

allweek <- rbind(Weekends, Weekdays)
```

#### Summarize data to get average steps by 5-minute interval
   by Weekend vs weekday

```r
library(dplyr)
StepsbyInt <- select(allweek, dt, interval, steps) %>%
             group_by(dt, interval) %>%
             summarize(steps = mean(steps))
```

### Plot average steps taken by 5-minute interval averages across days
#### by weekend vs. weekday

```r
library(ggplot2)
ggplot(StepsbyInt, aes(interval, steps)) + 
     geom_line() +
     facet_wrap(~dt, ncol=1) +
     scale_x_continuous("5-minute Interval of Day") +
     scale_y_continuous("Average Steps") +
     ggtitle("Steps by 5-Minute Interval") 
```

![](figure/plotbydaytype-1.png)<!-- -->


