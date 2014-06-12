## Part 1: Read the data
unzip("activity.zip") #unzip
activity <- read.csv("activity.csv") #read
activity$date <- as.Date(activity$date)


## Part 2
## calculate mean and median number of steps per day
stepsDay <- aggregate(steps ~ date, activity, sum)
hist(stepsDay$steps, col="cadetblue")
mean(stepsDay$steps, na.rm = T) #mean Day
median(stepsDay$steps) # median Day

## Part 3
## average daily activity
stepsInterval <- aggregate(steps ~ interval, activity, mean, na.rm=T)
plot(stepsInterval, type="l", col="blue", lwd=2)
stepsInterval$interval[which.max(stepsInterval$steps)] #maxStepsInterval


## Part 4
## Imputing values
sum(is.na(activity$steps)) # how many missing


# my imputing strategy will be to replace the NA values with mean
# for day-of-the-week + interval.
# (e.g. I am likely to repeat my steps similarly from Monday to Monday)
activityI <- activity
activityI$weekday <- weekdays(activityI$date)
stpsIntvWday <- aggregate(steps ~ interval+weekday,
                          activityI, mean, na.rm=T)
activityI$merger <- with(activityI, paste0(weekday,as.character(interval)))
stpsIntvWday$merger <- with(stpsIntvWday, paste0(weekday,as.character(interval)))

activityI <- merge(activityI, stpsIntvWday, by="merger", suffixes=c("",".y"))

nal <- is.na(activityI$steps)
activityI$steps[nal] <- activityI$steps.y[nal]
activityI <- activityI[,2:4]
activityI <- activityI[with(activityI, order(-as.numeric(date), interval)),]
row.names(activityI) <- NULL

# repeat same histogram as for activity
stepsDayI <- aggregate(steps ~ date, activityI, sum)
hist(stepsDayI$steps, col="darkgoldenrod")
mean(stepsDayI$steps, na.rm = T) #mean Day
median(stepsDayI$steps) # median Day

## Part 5
## Separating Weekdays and Weekends
weekendList <- c("Saturday", "Sunday")
dayLabel <- c("weekday", "weekend")
activityI$dayGroup <- with(activityI,
                    factor(weekdays(date) %in% weekendList, labels = dayLabel))
activityW <- split(activityI, activityI$dayGroup, drop=T)

# calculate means for intervals for weekdays and weekends separatelly
aggrStepsInterval <- function(df){
    aggregate(steps ~ interval, df, mean, na.rm=T)
}
stepsIntervalW <- lapply(activityW, aggrStepsInterval)

# plot means for intervals for weekdays and weekends
par(mfrow = c(2, 1), mar = c(4.1, 4.5, 2, 2))
plot(stepsIntervalW[[1]], type="l", main="Weekdays",
     col="blue", lwd=2, ylab="number of steps")
plot(stepsIntervalW[[2]], type="l", main="Weekends",
     col="red", lwd=2, ylab="number of steps")


