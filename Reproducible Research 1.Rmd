# Code for reading in the dataset and/or processing the data

setwd("C:/Users/Nina/Desktop/Essays/Coursera/Data Science/Reproducible Research/Programming Assignment 1")
dataset <- read.csv("C:/Users/Nina/Desktop/Essays/Coursera/Data Science/Reproducible Research/Programming Assignment 1/activity.csv")

# 1
# Histogram of the total number of steps taken each day
hist(dataset$steps, na.rm = TRUE)

# 2
# Mean and median number of steps taken each day
mean(dataset$steps, na.rm = TRUE)
median(dataset$steps, na.rm = TRUE)


# 3
# Time series plot of the average number of steps taken 
# Load in the required packages
library(ggplot2)
library(dplyr)
# Make and display the plot
p <- ggplot(dataset, aes(x = date, y = steps)) + geom_line()
p


# 4
# The 5-minute interval that, on average, contains the maximum number of steps 
# Assign a variable to the maximum steps value
max_steps <- max(dataset$steps, na.rm = TRUE)
# Find the interval matching max steps and assign it to a value
max_interval <- dataset$interval[which(dataset$steps == max_steps)]
# Display max interval
max_interval



# 5
# Code to describe and show a strategy for imputing missing data 
# Check how many values are missing
sum(is.na(dataset$steps))
# Make a subset of the missing data
missing_steps <- subset(dataset, is.na(steps)) 
# Set up the display so that there are 2 tables, for easier comparison
par(mfrow = c(2,1), mar = c(2,2,1,1))
# Make histograms of the missing steps (per interval and per date) for easier comparison
hist(missing_steps$interval)
hist(as.numeric(missing_steps$date))

# Group up the missing and non-missing values
activity_na <- dataset[is.na(dataset$steps),]
activity_not_na <- dataset[!is.na(dataset$steps),]
# Calculate the mean of the steps
steps_mean <- tapply(dataset$steps, dataset$interval, mean, na.rm = TRUE)
steps_mean

# Assign the mean value to the missing values
activity_na$steps <- as.factor(activity_na$interval)
levels(activity_na$steps) <- steps_mean
levels(activity_na$steps) <- round(as.numeric(levels(activity_na$steps)))
activity_na$steps <- as.integer(as.vector(activity_na$steps))

# Bind the missing values (modified to be equal to mean) and non-missing values
final_activity <- rbind(activity_na, activity_not_na)




# 6
# Histogram of the total number of steps taken each day after missing values are imputed 
# Make and display histogram
steps_per_day <- aggregate(steps ~ date, data = final_activity, FUN = sum, na.rm = TRUE)
hist(steps_per_day$steps)



# 7
# Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
# Load in the necessary package
library(ggplot2)
# Separate the data into weekdays and weekend days
dataset$which_day <- ifelse(weekdays(as.Date(final_activity$date)) == "Samstag", "Sonntag" | 
 	weekdays(as.Data(final_activity$date)) == "weekend", "weekday")
dataset$which_day <- factor(dataset$which_day)
# Make a variable of steps per interval per day
steps_which_day <- aggregate(steps ~ interval + which_day, data = final_activity, FUN = mean)
plot <- ggplot(steps_which_day, aes(interval, mean_steps))
plot + geom_line + facet_grid(which_day~.)


