## Read Data
url<- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
fileName <- "activity.csv"
zipFile <- "activity.zip"
if (!file.exists(fileName)) {
    if (!file.exists(zipFile)) { download.file(url,destfile=zipFile) }
    unzip(zipFile)
}
activityData <- read.csv(fileName, colClasses = c("integer","character","integer"))
activityData$date <- as.Date(activityData$date, "%Y-%m-%d")

## Mean total number of steps taken per day
#dailySteps <- aggregate(activityData$steps, by=list(activityData$date), FUN=sum, na.rm=TRUE)
dailySteps <- aggregate(steps ~ date, data=activityData, FUN=sum, na.action=na.omit)

### Histogram
meanTotalSteps <- mean(dailySteps$steps)
hist(dailySteps$steps, xlab="Number of steps", ylab="Frequency", main="Histogram total steps by day",
     freq=TRUE, col="lightgray", ylim=c(0,30))
abline(v=meanTotalSteps, col="blue", lwd=3)
abline(v=median(dailySteps$steps), col="green", lwd=2)
text(x=meanTotalSteps-1300, y=30, labels=c("Mean"), col="blue")
text(x=median(dailySteps$steps)+1400, y=30, labels=c("Median"), col="green")

### Mean
formattedMeanTotal <- format(meanTotalSteps)

### Median
median(dailySteps$steps)


## Average interval-daily activity pattern
intervalSteps <- aggregate(steps ~ interval, data=activityData, FUN=mean, na.action=na.omit)

### Plot the time series
intervalMax <- intervalSteps[which.max(intervalSteps$steps),"interval"]
maxSteps <- intervalSteps[which.max(intervalSteps$steps), "steps"]
plot(intervalSteps, type="l", xlab="Interval", ylab="Average steps", main="Average activity by interval across days")
abline(v=intervalSteps[which.max(intervalSteps$steps),"interval"], col="red", lty=2)
points(x=intervalMax, y=maxSteps, col="red", type="o")
text(x=intervalMax-190, y=maxSteps, col="red", labels=c("Max value"))

intervalMax <- intervalSteps[which.max(intervalSteps$steps),"interval"]
maxSteps <- intervalSteps[which.max(intervalSteps$steps), "steps"]
formatIntervalMax <- format(intervalMax)
formatMaxValue <- format(maxSteps)


## Imputing missing values

### Total rows with missing information
rowsNA <- sum(is.na(activityData))

### Days with missing information, and how many rows are missing each day
with(subset(activityData,is.na(steps)), {table(date)})

### Replace missing data with mean for the interval
intervalSteps <- aggregate(steps ~ interval, data=activityData, FUN=mean, na.action=na.omit)
naIndex <- which(is.na(activityData$steps))
correctedData <- activityData
missingIntervals <- correctedData[naIndex,]
missingValues <- merge(missingIntervals, intervalSteps, by="interval")
correctedData[naIndex, "steps"] <- missingValues[,"steps.y"]


### Histogram with correctedData
dailyCorrSteps <- aggregate(steps ~ date, data=correctedData, FUN=sum, na.action=na.omit)
hist(dailyCorrSteps$steps, xlab="Number of steps", ylab="Frequency", main="Histogram total steps by day", freq=TRUE, col="lightgray", ylim=c(0,35))
abline(v=mean(dailyCorrSteps$steps), col="blue", lwd=3)
abline(v=median(dailyCorrSteps$steps), col="green", lwd=2)
text(x=mean(dailyCorrSteps$steps)-1300, y=35, labels=c("Mean"), col="blue")
text(x=median(dailyCorrSteps$steps)+1400, y=33, labels=c("Median"), col="green")

### Mean
mean(dailyCorrSteps$steps)

### Median
median(dailyCorrSteps$steps)


## Activity patterns between weekdays and weekends (with corrected data)
Sys.setlocale("LC_TIME", "en_GB.UTF-8") # Use english day names

correctedData <- transform(correctedData, weekday=factor(weekdays(date)))
levels(correctedData$weekday)[levels(correctedData$weekday)!="Saturday" & levels(correctedData$weekday)!="Sunday"]<-"weekday"
levels(correctedData$weekday)[levels(correctedData$weekday)=="Saturday" | levels(correctedData$weekday)=="Sunday"]<-"weekend"

### Panel plot
library(lattice)
intervalDayData <- aggregate(steps ~ interval + weekday, data = correctedData, mean)
xyplot(steps ~ interval | weekday, data=intervalDayData, type = "l", layout = c(1, 2), 
    xlab = "Interval", ylab = "Number of steps")

## Compare statistics
tapply(intervalDayData$steps, intervalDayData$weekday, mean)
tapply(intervalDayData$steps, intervalDayData$weekday, median)
tapply(intervalDayData$steps, intervalDayData$weekday, max)