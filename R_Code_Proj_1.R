##Read the data after unzipping the file
unzip("activity.zip")
activity <- read.csv("activity.csv", header = TRUE)
#Format date column
activity$date <- as.Date(activity$date, "%Y-%m-%d")

#Calculate total number of steps taken per day. Remove NA values (got same result without removing NA values)
steps_by_day <- aggregate(steps ~ date, data = activity, sum, na.rm = TRUE)

hist(steps_by_day$steps, main = "Total steps taken by day", xlab = "Steps", col = "red", breaks=25, xlim=c(0,25000))

steps_by_day_mean <- mean(steps_by_day$steps)
steps_by_day_median <- median(steps_by_day$steps)


time_series <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)
plot(row.names(time_series), time_series,type = "l", xlab = "5-minute interval", ylab = "Average steps taken across all days", main = "Average Daily Activity Pattern", col= "blue")
max_interval <- names(which.max(time_series))
max_interval<- as.integer(max_interval)
max_interval_5 <- max_interval+5

Total_NAs <- sum(is.na(activity))

##Calculate the average number of steps for each interval over all of the days
steps_by_interval <- aggregate(steps ~ interval, data = activity, mean)

##Create a new dataset as a copy of activity
activity_filled <- activity

##Replace NA values for the average steps for that given 5-min interval in new dataset
for (i in 1:nrow(activity_filled)){
    if(is.na(activity_filled$steps[i])){
        activity_filled$steps[i] <- steps_by_interval[which(steps_by_interval$interval==activity_filled$interval[i]),]$steps
     }
}

##Calculate the number of steps by day using the completed dataset
steps_by_day2 <- aggregate(steps ~ date, data = activity_filled, sum)

hist(steps_by_day2$steps, main = "Total steps taken by day (completed data)", xlab = "Steps", col = "red", breaks=25, xlim=c(0,25000))

steps_by_day_mean2 <- mean(steps_by_day2$steps)
steps_by_day_median2 <- median(steps_by_day2$steps)

## Create new column for weekday vs weekend factor
activity_filled$weekday <- NA
## Add value Weekday or Weekend according to day of the week
for (i in 1:nrow(activity_filled)){
    if(weekdays(activity_filled$date[i])=="Saturday" | weekdays(activity_filled$date[i])=="Sunday") {
        activity_filled$weekday[i] <- "Weekend"
    } else { 
        activity_filled$weekday[i] <- "Weekday"
        }
}
activity_filled$weekday <- factor(activity_filled$weekday)

steps_by_interval2 <- aggregate(steps ~ interval + weekday, data = activity_filled, mean)

library(lattice)
xyplot(steps ~ interval | weekday, steps_by_interval2, type = "l", layout = c(1, 2), xlab = "Interval", ylab = "Number of steps")
