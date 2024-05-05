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
data$date <- as.Date(data$date)

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


# Imputing missing values

#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NANAs)

# Count total number of missing values
total_missing <- sum(is.na(data$steps))
cat("Total number of missing values:", total_missing, "\n")



#Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.






#Create a new dataset that is equal to the original dataset but with the missing data filled in.


# Calculate mean for each 5-minute interval
mean_interval <- aggregate(steps ~ interval, data = data, FUN = mean, na.rm = TRUE)

# Merge mean_interval with original dataset to fill missing values
filled_data <- merge(data, mean_interval, by = "interval", suffixes = c("", ".mean"))

# Fill missing values with mean steps for each interval
filled_data$steps <- ifelse(is.na(filled_data$steps), filled_data$steps.mean, filled_data$steps)

# Remove unnecessary columns
filled_data <- filled_data[, !(names(filled_data) %in% c("steps.mean"))]



# Calculate mean for each 5-minute interval
mean_data <- data %>%
        group_by(date) %>%
        summarise(mean_steps = round(mean(steps, na.rm = TRUE)))

# Merge mean_interval with original dataset to fill missing values
filled_data <- data %>%
        left_join(mean_data, by = "date") %>%
        mutate(steps = ifelse(is.na(steps), mean_steps, steps)) %>%
        select(-mean_steps)

# Calculate mean for each 5-minute interval
mean_interval <- data %>%
        group_by(interval) %>%
        summarise(mean_steps = round(mean(steps, na.rm = TRUE)))

# Merge mean_interval with original dataset to fill missing values
filled_data <- data %>%
        left_join(mean_interval, by = "interval") %>%
        mutate(steps = ifelse(is.na(steps), mean_steps, steps)) %>%
        select(-mean_steps)

#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps

# Calculate total steps per day for filled data
total_steps_per_day_filled <- aggregate(steps ~ date, data = filled_data, FUN = sum)

# Create histogram
hist(total_steps_per_day_filled$steps, main = "Total Steps Taken Each Day (Filled Data)", xlab = "Total Steps", col = "skyblue", border = "black")


# Create a time series plot
ggplot(total_steps_per_day_filled, aes(x = interval, y = steps)) +
        geom_line() +
        labs(title = "Average Number of Steps Taken (across all days) by 5-minute Interval",
             x = "5-minute Interval",
             y = "Average Steps")

# Calculate mean and median
mean_filled <- mean(total_steps_per_day_filled$steps)
median_filled <- median(total_steps_per_day_filled$steps)

cat("Mean total number of steps taken per day (filled data):", mean_filled, "\n")
cat("Median total number of steps taken per day (filled data):", median_filled, "\n")

# Are there differences in activity patterns between weekdays and weekends?

# Create a new factor variable indicating weekdays and weekends


filled_data$Weekday <- weekdays(data$date, abbreviate = TRUE) 
filled_data$day_type <- ifelse(weekdays(data$date, abbreviate = TRUE) %in% c("sáb", "dom"), "weekend", "weekday")
filled_data$day_type <- factor(filled_data$day_type, levels = c("weekday", "weekend"))

# Calculate the average number of steps for each 5-minute interval, grouped by day type
average_steps_day_type <- aggregate(steps ~ interval + day_type, data = filled_data, FUN = mean)

# Create panel plot
ggplot(filled_data, aes(x = interval, y = steps, group = day_type, color = day_type)) +
        geom_line() +
        facet_wrap(~ day_type, ncol = 1) +
        labs(title = "Average Number of Steps Taken by 5-minute Interval",
             x = "5-minute Interval",
             y = "Average Steps",
             color = "Day Type") +
        theme_minimal()
