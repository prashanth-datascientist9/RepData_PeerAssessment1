library(data.table)
library(ggplot2)

if(!file.exists("repdata%2Fdata%2Factivity.zip")) {
  temp <- tempfile()
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", temp)
  file <- unzip(temp)
  unlink(temp)
} else {
  file <- unzip("repdata%2Fdata%2Factivity.zip")
}

df <- read.csv(file, header = TRUE, sep = ",")

# Aggregate the steps by date and get a sum of those steps after omitting the NA values.
#aggdf <- aggregate(df$steps~df$date, df, sum, na.action = na.omit)

# Aggregate the steps by date and get a sum of those steps
aggdf <- aggregate(df$steps~df$date, df, sum)

#Plot of a hitogram with the total steps per day
#ggplot(aggdf, aes(x=aggdf$`df$date`,y=aggdf$`df$steps`))+geom_bar(stat="identity")+
#  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
#  xlab("Date")+
#  ylab("Total Steps")+
#  ggtitle("Total Steps by Date")+
#  theme(plot.title = element_text(hjust = 0.5))
hist(aggdf$`df$steps`, col = 2, xlab = "Total Steps per day", main = "Histogram of total steps in a day")

#Mean of total steps per day
mean(aggdf$`df$steps`, na.rm = TRUE)

#Median of total steps per day
median(aggdf$`df$steps`, na.rm = TRUE)

aggdf_steps_int <- aggregate(df$steps ~ df$interval, df, mean, na.action = na.omit)

plot(aggdf_steps_int$`df$interval`, aggdf_steps_int$`df$steps`, col = 2, type = "l",
     xlab = "Interval", ylab = "Average Steps", main = "Average steps in a day, averaged across all days")

avg_max_steps_id <- which.max(aggdf_steps_int$`df$steps`)

aggdf_steps_int[avg_max_steps_id, ]

print(paste("The interval with the max number of steps is ", 
            aggdf_steps_int[avg_max_steps_id, ]$`df$interval`,
            " and the number if steps for that interval is ", 
            round(aggdf_steps_int[avg_max_steps_id, ]$`df$steps`)))

missing_values <- df[!complete.cases(df),]
nrow(missing_values)

for(i in 1:nrow(df)) {
  if(is.na(df$steps[i])) {
    steps_value <- aggdf_steps_int$`df$steps`[which(aggdf_steps_int$`df$interval` == df$interval[i])]
    df$steps[i] <- steps_value
  }
}

#Aggregate the steps per day with the imputed values
steps_per_day <- aggregate(steps ~ date, df, sum)

#Draw a histogram of the imputed values
hist(steps_per_day$steps, col = 4, main = "Total number of steps per day Imputed", xlab = "Steps per day")

mean(steps_per_day$steps)

median(steps_per_day$steps)

week_day_check <- function(date_value) {
  weekday <- weekdays(as.Date(date_value, '%Y-%m-%d'))
  if(!(weekday == 'Saturday' || weekday == 'Sunday')) {
    day_type <- 'Weekday'
  } else {
    day_type <- 'Weekend'
  }
  day_type
}

df$day_type <- as.factor(sapply(df$date, week_day_check))

steps_per_day <- aggregate(steps ~ interval+day_type, df, mean)

newplot <- ggplot(steps_per_day, aes(interval, steps)) +
  geom_line(stat = "identity", aes(colour = day_type)) +
  theme_light() +
  facet_grid(day_type ~ ., scales = "fixed", space = "fixed") +
  labs(x = "Interval", y = expression("Number of Steps")) +
  ggtitle("Number of Steps per Interval by Weekday/Weekend")

print(newplot)