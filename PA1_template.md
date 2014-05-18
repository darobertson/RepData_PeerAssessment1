# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data

### Load activity data

activity <- read.csv("activity.csv", header=TRUE,sep=",")

### Remove NA's before daily or interval segments

no.na.activity <- complete.cases(activity)
complete.activity <- activity[no.na.activity,]



## What is mean total number of steps taken per day?


### Calculate number of steps taken per day

activity.by.day <- aggregate(complete.activity$steps, by = list(complete.activity$date), sum)
names(activity.by.day)[1] <- "day"
names(activity.by.day)[2] <- "steps"

### Plot data for histogram of frequency of daily step counts

png(file="StepHistogram.png",width=600,height=480)

hist(activity.by.day$steps)

dev.off()

### Plot data for histogram of steps taken each day

png(file="StepsByDay.png",width=600,height=480)

plot(activity.by.day$day, activity.by.day$steps, col = "black", type = "h", ylab = "Daily Total 
Number of Steps", xlab = "Date")

dev.off()

### Calculate average steps taken each day

mean(activity.by.day$steps)
median(activity.by.day$steps)



## What is the average daily activity pattern?

### Calculate average steps taken each interval

activity.by.interval <- aggregate(complete.activity$steps, by = list(complete.activity
$interval), mean)
names(activity.by.interval)[1] <- "interval"
names(activity.by.interval)[2] <- "steps"

### Plot data for time series of average number of steps taken each interval

png(file="StepsbyInterval.png",width=600,height=480)

plot(activity.by.interval$interval, activity.by.interval$steps, type = "l", ylab = "Average 
Number of Steps", xlab = "Interval")

dev.off()
### Calculate interval with maximum average steps taken

activity.by.interval[activity.by.interval$steps >= max(activity.by.interval$steps), ]


## Imputing missing values

### Determine a strategy for inputting missing data
####All days with missing step data are missing the step data for all intervals of those days. Thus, the average interval data will evenly divide into the missing interval data, in the order of the missing interval data. This will serve to replace the mising data for each interval with the average step data for that particular interval.

### Identify rows with NAs
#### Previously defined: no.na.activity <- complete.cases(activity)
activity.missing <- activity[!no.na.activity,]

### Calculate number of Rows with Missing Step Data
print("Number of Rows with Missing Step Data: ")
print(nrow(activity)-nrow(activity.clean))

### Use Average Interval Data to fill in missing step data (correponding to that interval) 

activity.missing.new <- data.frame(steps=activity.by.interval$steps, date=activity.missing$date, interval=activity.missing$interval)

### Combine the table of complete step data from the original dataset with the table of estimated step data for the rows that originally contained NAs

combined.activity <- rbind(complete.activity, activity.missing.new)
new.steps <- append(complete.activity$steps, activity.missing.new$steps)

date.info.complete <- as.POSIXlt(complete.activity$date)
date.info.missing <- as.POSIXlt(activity.missing.new$date)
new.dates <- append(date.info.complete, date.info.missing)
wday <- date.info$wday

new.interval <- append(complete.activity$interval, activity.missing.new$interval)

combined.activity <- data.frame(steps=new.steps, day.type=wday, interval=new.interval)


## Are there differences in activity patterns between weekdays and weekends?

### Separate weekday data from weekend dataIdentify and classify data points by day of week

weekday <- wday <= 5
weekend <- wday >= 6

weekend.activity <- combined.activity[weekend, ]
weekday.activity <- combined.activity[weekday, ]

## Panel Plot

png(file="Weekend-Weekday.Activity.png",width=480,height=600)
par(mfrow=c(2,1))

plot(weekend.activity$interval, weekend.activity$steps, col = "blue", type = "l", ylab = "Number of Steps", xlab = "Interval", main = "Weekend Activity")

plot(weekday.activity$interval, weekday.activity$steps, col = "blue", type = "l", ylab = "Number of Steps", xlab = "Interval", main = "Weekday Activity")

dev.off()


