## Download the ZIP file and then unzip it. Check if the files exist before processing.
zipname <- "repdata_data_activity.zip"
fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
if (!file.exists(zipname)){
        download.file(fileURL, zipname, method = "curl")
}
filename <- "activity.csv"
if (!file.exists(filename)){
        unzip(zipname)
}

## Read the data into R as data.frames
actData <- read.csv(filename, sep = ",", header = TRUE)

## Histogram of the total number of steps taken each day
actData_perday <- aggregate(steps ~ date, actData, sum)
hist(actData_perday$steps)

## Mean and median number of steps taken each day
actData_mean <- mean(actData_perday$steps)
actData_median <- median(actData_perday$steps)

## Time series plot of the average number of steps taken
actData_bytime <- aggregate(steps ~ interval, actData, sum)
with(actData_bytime, plot(interval, steps, type = "l"))

## The 5-minute interval that, on average, contains the maximum number of steps
max_interval <- actData_bytime$interval[which.max(actData_bytime$steps)]

## Code to describe and show a strategy for imputing missing data
        ## Calculate and report the total number of missing values in the dataset 
        ## (i.e. the total number of rows with NAs)
        TotalNA <- sum(is.na(actData$steps))

        ## Devise a strategy for filling in all of the missing values in the dataset. 
        ## The strategy does not need to be sophisticated. I use the mean for that interval.
        meanbyInterval <- aggregate(steps ~ interval, actData, mean)

        ## Create a new dataset that is equal to the original dataset but with the missing data filled in.
        actData_Imputed <- actData
        for (i in 1: nrow(actData_Imputed)){
                if (is.na(actData_Imputed$steps[i])){
                        meanvalue <- meanbyInterval$steps[which(meanbyInterval$interval == actData_Imputed$interval[i])]
                        actData_Imputed$steps[i] <- meanvalue
                }
        }

        ## Make a histogram of the total number of steps taken each day and Calculate and report 
        ## the mean and median total number of steps taken per day. Do these values differ from 
        ## the estimates from the first part of the assignment? What is the impact of imputing 
        ## missing data on the estimates of the total daily number of steps?
        actDataImputed_perday <- aggregate(steps ~ date, actData_Imputed, sum)
        hist(actDataImputed_perday$steps)
        
        actDataImputed_mean <- mean(actDataImputed_perday$steps)
        actDataImputed_median <- median(actDataImputed_perday$steps)
        
## Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends.
        ## Convert the data to date format.
        actData_Imputed$date <- as.Date(actData_Imputed$date, "%Y-%m-%d")
        ## Add columns define the day and type ("weekday" or "weekend").
        actData_Imputed$day <- weekdays(actData_Imputed$date)
        actData_Imputed$daytype <- c("weekday")
        for (i in 1:nrow(actData_Imputed)){
                if (actData_Imputed$day[i] == "Saturday" || actData_Imputed$day[i] == "Sunday"){
                        actData_Imputed$daytype[i] <- "weekend"
                }
        }
        actDataImputed_bytime <- aggregate(steps ~ interval + daytype, actData_Imputed, sum)
        library(ggplot2)
        g <- ggplot(actDataImputed_bytime, aes(interval, steps, col = daytype))
        g + geom_line() + facet_grid(rows = vars(daytype)) + 
                ggtitle("Average steps taken per 5-minute interval across weekdays and weekends") +
                ylab("Average steps") + xlab("Interval")
        