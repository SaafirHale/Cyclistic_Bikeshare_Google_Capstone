install.packages('dplyr')
remove.packages('dplyr')
install.packages('lubridate', dependencies = TRUE, INSTALL_opts = '--no-lock')
remove.packages('lubridate')
install.packages('tidyverse')
remove.packages('tidyverse')
install.packages('ggplot2')
remove.packages('ggplot2')


# Installing Data Analysis Libraries
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)

remove.packages("rlang")
install.packages("rlang")
library(rlang)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)


# Uploading Bike Datasets (.csv files)
dec <- read.csv("Dec 2020.csv")
jan <- read.csv("Jan 2021.csv")
feb <- read.csv("Feb 2021.csv")
mar <- read.csv("Mar 2021.csv")
apr <- read.csv("Apr 2021.csv")
may <- read.csv("May 2021.csv")
jun <- read.csv("Jun 2021.csv")
jul <- read.csv("Jul 2021.csv")
aug <- read.csv("Aug 2021.csv")
sep <- read.csv("Sep 2021.csv")
oct <- read.csv("Oct 2021.csv")
nov <- read.csv("Nov 2021.csv")


# Comparing the column names of each file
colnames(dec)
colnames(jan)
colnames(feb)
colnames(mar)
colnames(apr)
colnames(may)
colnames(jun)
colnames(jul)
colnames(aug)
colnames(sep)
colnames(oct)
colnames(nov)

# Checking the files columns and data types to be consistent to the Jan 2021 file
str(dec)
str(jan)
str(feb)
str(mar)
str(apr)
str(may)
str(jun)
str(jul)
str(aug)
str(sep)
str(oct)
str(nov)

# Combining all the months into one data frame
year_trips <- bind_rows(dec,jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov)

# Removing Latitude and Longitude
year_trips <- year_trips %>% 
  select(-c(start_lat, start_lng, end_lat, end_lng))

# Making sure that day_of_week is a integer
year_trips <- mutate(year_trips, day_of_week = as.integer(day_of_week))

# Checking the year_trips table
colnames(year_trips) #List of column names
nrow(year_trips) #Number of rows in data frame
dim(year_trips) #Dimensions in the data frame
head(year_trips) #See the first and last rows of data frame
tail(year_trips) 
str(year_trips) #See list of columns and data types
summary(year_trips) #Statistical summary of data

table(year_trips$member_casual)

# Adding columns date, month, day, and year for ride data frame
year_trips$date <- as.Date(year_trips$started_at) #The default format is yyyy-mm-dd
year_trips$month <- format(as.Date(year_trips$date), "%m")
year_trips$day <- format(as.Date(year_trips$date), "%d")
year_trips$year <- format(as.Date(year_trips$date), "%Y")
year_trips$day_of_week <- format(as.Date(year_trips$date), "%A")

str(year_trips)

# Removing ride_length to recreate it in numeric
year_trips <- subset(year_trips, select = -c(ride_length))

# Creating ride_length
year_trips$ride_length <- difftime(year_trips$ended_at, year_trips$started_at)

# Convert ride_length to numeric
is.character(year_trips$ride_length)
year_trips$ride_length <- as.numeric(year_trips$ride_length)
is.numeric(year_trips$ride_length)

# Remove "bad" data
year_trips_2 <- year_trips[!(year_trips$start_station_name == "HQ QR" | year_trips$ride_length < 0),]

# Analysis on ride_length (in seconds)

mean(year_trips_2$ride_length) # Average
median(year_trips_2$ride_length) # Midpoint
max(year_trips_2$ride_length) #Longest ride
min(year_trips_2$ride_length) #Shortest ride

summary(year_trips_2$ride_length)

# Comparing members and casual users
aggregate(year_trips_2$ride_length ~ year_trips_2$member_casual, FUN = mean)
aggregate(year_trips_2$ride_length ~ year_trips_2$member_casual, FUN = median)
aggregate(year_trips_2$ride_length ~ year_trips_2$member_casual, FUN = max)
aggregate(year_trips_2$ride_length ~ year_trips_2$member_casual, FUN = min)

aggregate(year_trips_2$ride_length ~ year_trips_2$member_casual + year_trips_2$day_of_week,
          FUN = mean)

year_trips_2$day_of_week <- ordered(year_trips_2$day_of_week,
                                    levels = c("Sunday", "Monday", "Tuesday",
                                               "Wednesday", "Thursday", "Friday", "Saturday"))

aggregate(year_trips_2$ride_length ~ year_trips_2$member_casual + year_trips_2$day_of_week,
          FUN = mean)

# Comparing ridership by type and weekday
year_trips_2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% #wday() creates a weekday field
  group_by(member_casual, weekday) %>% #Groups by usertype and weekday
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)


# Visualizing number of rides by rider type
year_trips_2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) + geom_col(position = 'dodge')

# Visualizing average duration
year_trips_2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) + geom_col(position = 'dodge')

# Saving the code into a CSV file for further visualization
counts <- aggregate(year_trips_2$ride_length ~ year_trips_2$member_casual + 
                      year_trips_2$day_of_week, FUN = mean)
write.csv(counts, file = "C:/Users/Saafir Hale/Desktop/cyclistic_rides.csv")
