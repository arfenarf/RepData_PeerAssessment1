---
title: "PA1_template.Rmd"
author: "KGW"
date: "June 18, 2015"
output: html_document
---

### Loading and preprocessing

First, we open the data file and drop the NAs


```r
setwd("~/datacourse/Reproducible Research/RepData_PeerAssessment1/")
library(dplyr)
library(ggplot2)
par(mfrow = c(1,1))
activity <- read.csv("data/activity.csv")
complete <- activity[complete.cases(activity), ]
```


### What is mean total number of steps taken per day?

First we calculate the statistical summaries ... started with tapply
(daytotals <- tapply(activity\$steps, activity\$date, sum)) but have become spoilt
by dplyr

Then we also calculate out the mean and median total number of steps per day.


```r
daytotals <- complete %>% group_by(date) %>% summarise(total_steps = sum(steps))

hist(daytotals$total_steps)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
print(paste("Mean daily steps: ", mean(daytotals$total_steps)))
```

```
## [1] "Mean daily steps:  10766.1886792453"
```

```r
print(paste("Median daily steps: ", median(daytotals$total_steps)))
```

```
## [1] "Median daily steps:  10765"
```


### What is the average daily activity pattern?

So now let's make a different table to play with summarising average steps by interval.
The line plot is simple and follows.  We'll extract the single interval with the
maximum number of steps at the end.


```r
intervalsteps <- complete %>% group_by(interval) %>% summarise(avg_steps = mean(steps))
plot(intervalsteps$interval, intervalsteps$avg_steps, type = "l")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
print(paste("The interval with the highest average number of steps is: ",
            intervalsteps[which.max(intervalsteps$avg_steps),1]))
```

```
## [1] "The interval with the highest average number of steps is:  835"
```


### Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)


```r
nas <- activity[is.na(activity$steps),]
narows <- nrow(nas)
print(paste("The number of missing rows is:", narows))
```

```
## [1] "The number of missing rows is: 2304"
```


2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc. (I chose to go wtih the mean for that interval since it was right there.  But to keep things tidy, I made it
look like an integer)


```r
nas <- left_join(nas, intervalsteps, by = "interval")
nas$steps <- round(nas$avg_steps, 0)
```


3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
merged <- bind_rows(complete, nas)
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
completedaytotals <- merged %>% group_by(date) %>% summarise(total_steps = sum(steps))

hist(completedaytotals$total_steps)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

```r
print(paste("Mean daily steps: ", mean(completedaytotals$total_steps)))
```

```
## [1] "Mean daily steps:  10765.6393442623"
```

```r
print(paste("Median daily steps: ", median(completedaytotals$total_steps)))
```

```
## [1] "Median daily steps:  10762"
```

Let's compare:


```r
completedaytotals$type <- 'complete'
daytotals$type <- 'nas out'
rptdaytotals <- bind_rows(completedaytotals, daytotals)

ggplot(data = rptdaytotals, aes(rptdaytotals$total_steps)) + geom_histogram() + facet_grid(. ~ type)
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

```r
rm(rptdaytotals)
```


### Are there differences in activity patterns between weekdays and weekends?

For this part the `weekdays()` function may be of some help here. Use
the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
merged$weekend <- ifelse((weekdays(as.POSIXct(merged$date)) == "Saturday") | 
                         (weekdays(as.POSIXct(merged$date)) == "Sunday"), TRUE, FALSE)
merged$weekend <- factor(merged$weekend,labels = c("weekend", "weekday"))
```


1. Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using **simulated data**:

![Sample panel plot](instructions_fig/sample_panelplot.png) 


```r
intervalwk <- merged %>% group_by(weekend, interval) %>% summarise(mean(steps))
names(intervalwk)[3] <- 'steps'
ggplot(intervalwk, aes(x = interval, y = steps)) + geom_line() + facet_grid(weekend~.)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 


**Your plot will look different from the one above** because you will
be using the activity monitor data. Note that the above plot was made
using the lattice system but you can make the same version of the plot
using any plotting system you choose.


## Submitting the Assignment

To submit the assignment:

1. Commit your completed `PA1_template.Rmd` file to the `master` branch of your git repository (you should already be on the `master` branch unless you created new ones)

2. Commit your `PA1_template.md` and `PA1_template.html` files produced by processing your R markdown file with the `knit2html()` function in R (from the **knitr** package)

3. If your document has figures included (it should) then they should have been placed in the `figure/` directory by default (unless you overrode the default). Add and commit the `figure/` directory to your git repository.

4. Push your `master` branch to GitHub.

5. Submit the URL to your GitHub repository for this assignment on the course web site.

In addition to submitting the URL for your GitHub repository, you will
need to submit the 40 character SHA-1 hash (as string of numbers from
0-9 and letters from a-f) that identifies the repository commit that
contains the version of the files you want to submit. You can do this
in GitHub by doing the following:

1. Go into your GitHub repository web page for this assignment

2. Click on the "?? commits" link where ?? is the number of commits you have in the repository. For example, if you made a total of 10 commits to this repository, the link should say "10 commits".

3. You will see a list of commits that you have made to this repository. The most recent commit is at the very top. If this represents the version of the files you want to submit, then just click the "copy to clipboard" button on the right hand side that should appear when you hover over the SHA-1 hash. Paste this SHA-1 hash into the course web site when you submit your assignment. If you don't want to use the most recent commit, then go down and find the commit you want and copy the SHA-1 hash.

A valid submission will look something like (this is just an **example**!)

https://github.com/rdpeng/RepData_PeerAssessment1

7c376cc5447f11537f8740af8e07d6facc3d9645
