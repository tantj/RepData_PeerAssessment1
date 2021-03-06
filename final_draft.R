# Load libraries
library(reshape2)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(lattice)

# Load data, omit NA, summarise by date
activity <- read.csv("activity.csv", stringsAsFactors = FALSE)
activityPlus <- activity
activity <- activity[complete.cases(activity), ]
activityByDate <- group_by(activity, date)
activity_sumDate <- summarise(activityByDate, steps = sum(steps))

# Plot chart
png("StepsPerDay.png")
stepPlot <- ggplot(activity_sumDate, aes(steps)) + 
  geom_histogram(breaks = seq(0, 25000, 1000), col = "white", fill = "orangered2") +
  labs(title = "Total steps per day", x = "Steps", y = "Frequency") +
  theme(text = element_text(size = 10, family = "Avenir"))
ggsave("StepsPerDay.png", stepPlot)
dev.off()

# Find mean and median of steps daily
stepMean <- mean(activity_sumDate$steps)
stepMedian <- median(activity_sumDate$steps)

# Summarise by time intervals
activityByTime <- group_by(activity, interval)
activity_sumTime <- summarise(activityByTime, steps = mean(steps))

# Plot time-series
png("StepsPerInterval.png")
stepPlotTime <- ggplot(activity_sumTime, aes(x = interval, y = steps)) + 
  geom_line() + 
  labs(title = "Average steps per interval", x = "Time", y = "Average steps") + 
  theme(text = element_text(size = 10, family = "Avenir"))
ggsave("StepsPerInterval.png", stepPlotTime)
dev.off()

# Find interval with max steps
timeMax <- activity_sumTime[which.max(activity_sumTime$steps), ]$interval

# Replace NAs with mean steps for that time interval
numberNA <- sum(is.na(activityPlus$step))
for (i in 1:nrow(activityPlus)) {
  if (is.na(activityPlus$steps[i])) {
    activityPlus$steps[i] <- activity_sumTime[activity_sumTime$interval == activityPlus$interval[i], 2]
  }
}
activityPlus$steps <- as.integer(activityPlus$steps)

# Plot chart for steps per day
activityByDatePlus <- group_by(activityPlus, date)
activity_sumDatePlus <- summarise(activityByDatePlus, steps = sum(steps))
png("StepsPerDayPlus.png")
stepPlotPlus <- ggplot(activity_sumDatePlus, aes(steps)) + 
  geom_histogram(breaks = seq(0, 25000, 1000), col = "white", fill = "orangered2") +
  labs(title = "Total steps per day", x = "Steps", y = "Frequency") +
  theme(text = element_text(size = 10, family = "Avenir"))
ggsave("StepsPerDayPlus.png", stepPlotPlus)
dev.off()

# Find mean and median of steps daily
stepMeanPlus <- mean(activity_sumDatePlus$steps)
stepMedianPlus <- median(activity_sumDatePlus$steps)
stepMeanDiff <- stepMeanPlus - stepMean
stepMedianDiff <- stepMedianPlus - stepMedian

# Divide into weekday/weekend
activityPlus$day <- ifelse(as.POSIXlt(as.Date(activityPlus$date))$wday%%6 == 0, "weekend", "weekday")
activityPlus$day <- factor(activityPlus$day, levels = c("weekend", "weekday"))
activityPlusInt <- aggregate(steps ~ interval + day, data = activityPlus, FUN = mean)

# Plot lattice plot
activityPlusPlot <- xyplot(steps ~ interval | factor(day), data = activityPlusInt, aspect = 1/2, type = "l", ylab = "Number of steps", xlab = "Interval")
trellis.device(device = "png", filename = "StepsPerIntervalWeek2.png")
print(activityPlusPlot)
dev.off()


