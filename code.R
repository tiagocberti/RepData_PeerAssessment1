##Loading and preprocessing the data
setwd("C:\\Users\\Tiago\\RepData_PeerAssessment1")
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip","data.zip")
unzip("data.zip")
df <- read.csv("activity.csv",na.strings = "NA")
df$date <- as.Date(df$date)

##What is mean total number of steps taken per day?
##1. Make a histogram of the total number of steps taken each day
dfSteps<-aggregate(steps~date,data=df,sum,na.rm=TRUE)
barplot(dfSteps$steps, names.arg = dfSteps$date, xlab = "Date", ylab = "Count of Steps")
##2. Calculate and report the mean and median total number of steps taken per day
mean(dfSteps$steps)
median(dfSteps$steps)

##What is the average daily activity pattern?
##1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
dfInter<-aggregate(steps~interval,data=df,mean)
plot(dfInter,type="l")

##2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
dfInter$interval[which.max(dfInter$steps)]


##Imputing missing values
##1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
sum(is.na(df))

##2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
##3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
df_re.na <- merge(df, dfInter, by = "interval", suffixes = c("","_ag"))
nas <- is.na(df_re.na$steps)
df_re.na$steps[nas] <- df_re.na$steps_ag[nas]
df_re.na <- df_re.na[, c(1:3)]
sum(is.na(df_re.na))
##4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
dfSteps2<-aggregate(steps~date,data=df_re.na,sum,na.rm=TRUE)
barplot(dfSteps2$steps, names.arg = dfSteps2$date, xlab = "Date", ylab = "Count of Steps")
mean(dfSteps2$steps)
median(dfSteps2$steps)

##Are there differences in activity patterns between weekdays and weekends?
##1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
weekdaytype <- function(x) {
  if (weekdays(x) %in% c("s�bado", "domingo","Saturday","Sunday")) {
    "weekend"
  } else {
    "weekday"
  }
}
df_re.na$weekdaytype <- as.factor(sapply(df_re.na$date, weekdaytype))

##2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
par(mfrow = c(2, 1))
for (type in c("weekend", "weekday")) {
  dfstepstype <- aggregate(steps ~ interval, data = df_re.na, subset = df_re.na$weekdaytype == 
                            type, FUN = mean)
  plot(dfstepstype, type = "l", main = type)
}
