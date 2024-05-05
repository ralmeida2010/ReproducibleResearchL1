library(ggplot2)




# Loading and preprocessing the data
data <- read.csv("C:/user/rui/Doutoramento/DataScienceSpecialization/Reproducible Research/Trabalho1/activity.csv")
# summary file in order to see if all data as been read
summary(data)
# What is mean total number of steps taken per day?
# Histogram of the total number of steps taken each day

# Remove rows with NA values in the 'steps' column
data2 <- na.omit(data)

# Convert the 'date' column to Date format
data2$date <- as.Date(data2$date)

# Calculate total steps per day
total_steps_per_day <- aggregate(steps ~ date, data = data2, FUN = sum)

# Create a histogram
ggplot(total_steps_per_day, aes(x = steps)) +
        geom_histogram(binwidth = 1000, fill = "blue", color = "black") +  # Adjust binwidth as needed
        labs(title = "Total Steps Taken Each Day",
             x = "Total Steps",
             y = "Frequency")
# Mean and median number of steps taken each day
summary(total_steps_per_day)


#Calculate the total number of steps taken per day

#If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

#Calculate and report the mean and median of the total number of steps taken per day



# What is the average daily activity pattern?


# Calculate the average number of steps for each 5-minute interval
average_steps <- aggregate(steps ~ interval, data = data2, FUN = mean)

# Create a time series plot
ggplot(average_steps, aes(x = interval, y = steps)) +
        geom_line() +
        labs(title = "Average Number of Steps Taken (across all days) by 5-minute Interval",
             x = "5-minute Interval",
             y = "Average Steps")

# Find the interval with the maximum average number of steps
max_interval <- average_steps[which.max(average_steps$steps), "interval"]
max_average_steps <- max(average_steps$steps)

cat("Interval", max_interval, "has the maximum average number of steps:", max_average_steps, "\n")






