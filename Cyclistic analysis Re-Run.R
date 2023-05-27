## Install and Load the needed packages
library(tidyverse)
library(janitor)
library(ggmap)
library(geosphere)
library(lubridate)
library(readr)

## import data in R studio

sep_21 <- read.csv("/Users/agadz/Desktop/Google analytics/Cyclistic data/Analysis/202109-divvy-tripdata.csv")
oct_21 <- read.csv("/Users/agadz/Desktop/Google analytics/Cyclistic data/Analysis/202110-divvy-tripdata.csv")
nov_21 <- read.csv("/Users/agadz/Desktop/Google analytics/Cyclistic data/Analysis/202111-divvy-tripdata.csv")
dec_21 <- read.csv("/Users/agadz/Desktop/Google analytics/Cyclistic data/Analysis/202112-divvy-tripdata.csv")
jan_22 <- read.csv("/Users/agadz/Desktop/Google analytics/Cyclistic data/Analysis/202201-divvy-tripdata.csv")
feb_22 <- read.csv("/Users/agadz/Desktop/Google analytics/Cyclistic data/Analysis/202202-divvy-tripdata.csv")
mar_22 <- read.csv("/Users/agadz/Desktop/Google analytics/Cyclistic data/Analysis/202203-divvy-tripdata.csv")
apr_22 <- read.csv("/Users/agadz/Desktop/Google analytics/Cyclistic data/Analysis/202204-divvy-tripdata.csv")
may_22 <- read.csv("/Users/agadz/Desktop/Google analytics/Cyclistic data/Analysis/202205-divvy-tripdata.csv")
jun_22 <- read.csv("/Users/agadz/Desktop/Google analytics/Cyclistic data/Analysis/202206-divvy-tripdata.csv")
jul_22 <- read.csv("/Users/agadz/Desktop/Google analytics/Cyclistic data/Analysis/202207-divvy-tripdata.csv")
aug_22 <- read.csv("/Users/agadz/Desktop/Google analytics/Cyclistic data/Analysis/202208-divvy-tripdata.csv")

## inspecting monthly datasets for uniformity 

colnames(sep_21)
colnames(oct_21)
colnames(nov_21)
colnames(dec_21)
colnames(jan_22)
colnames(feb_22)
colnames(mar_22)
colnames(apr_22)
colnames(may_22)
colnames(jun_22) 
colnames(jul_22)
colnames(aug_22)

## Inspecting the structures of the monthly datasets

str(sep_21)
str(oct_21)
str(nov_21)
str(dec_21)
str(jan_22)
str(feb_22)
str(mar_22)
str(apr_22)
str(may_22)
str(jun_22) 
str(jul_22)
str(aug_22)

## merge monthly data frames into one data frame

all_trips <- bind_rows(sep_21, oct_21, nov_21, dec_21, jan_22, feb_22, mar_22, apr_22, may_22, jun_22, jul_22, aug_22)

## Inspect the new data frame 

colnames(all_trips)
str(all_trips)
nrow(all_trips) 
dim(all_trips)
head(all_trips)
summary(all_trips)

##Add columns that list the date, month, day, and year of each ride
##This will allow us to aggregate ride data for each month, day, or year ... before completing these operations we could only aggregate at the ride level

all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

## Adding ride_length column

all_trips <- all_trips %>%
  mutate(across(c(started_at, ended_at), ymd_hms), 
  ride_length = difftime(ended_at, started_at, units = 'secs'))

# Inspect the structure of the columns

str(all_trips)

## position ride_length after ended_at

all_trips <- all_trips %>%
  relocate(ride_length, .after=ended_at)

# Remove "bad" data

all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<=0),]

#View new data frame

View(all_trips_v2)

#After viewing all_trips_2, it was noticed that some cells were empty, we will have to fill them up with NA

all_trips_v2 <- all_trips_v2 %>% 
  mutate_at(c('start_station_name','start_station_id', 'end_station_name', 'end_station_id'), ~na_if(., ''))

## Descriptive analysis on ride_length 
3
all_trips_v2 %>% 
  summarise(average_ride_length = mean(ride_length), median_length = median(ride_length), 
            max_ride_length = max(ride_length), min_ride_length = min(ride_length))

## Members vs casual riders difference depending on total rides taken

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

## Arrange the days of the week and put them in order

all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

## Check again

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

## Analysis of ridership data by type and weekday

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  
  group_by(member_casual, weekday) %>%  
  summarise(number_of_rides = n()						
      ,average_duration = mean(ride_length)) %>% 		
  arrange(member_casual, weekday)	

# Visualization of the number of rides by rider type on the days of the week 

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
             ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")


# Visualization of the average ride duration and rider type

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

## Visualize total rides data by type and month

all_trips_v2 %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  labs(title ="Total rides by Members and Casual riders Vs. Month", x = "Month", y= "Number Of Rides") +
  theme(axis.text.x = element_text(angle = 45)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

