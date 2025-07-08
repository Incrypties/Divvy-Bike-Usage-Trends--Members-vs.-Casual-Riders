install.packages("tidyverse")
library(tidyverse)

installed.packages("ggplot2")
library(ggplot2)

install.packages("skimr")
library("skimr")

install.packages("janitor")
library("janitor")

install.packages("dplyr")
library("dplyr")

install.packages('readr')
library(readr)

library(lubridate)


read_csv("Divvy_Trips_2019_Q1.csv")
read_csv("Divvy_Trips_2020_Q1.csv")

trips_df_2019 <- read_csv("Divvy_Trips_2019_Q1.csv")
trips_df_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")

## To Look at top 6 rows
head(trips_df_2019)

## Really quick idea of what's in this dataset
glimpse(trips_df_2019)


## Provides a detailed summary of the data
skim_without_charts(trips_df_2019)

## To Look at top 6 rows
head(trips_df_2020)

## Really quick idea of what's in this dataset
glimpse(trips_df_2020)


## Provides a detailed summary of the data
skim_without_charts(trips_df_2020)


# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
## To identify only column names, and compare them
colnames(trips_df_2019)
colnames(trips_df_2020)

## Change the column names to align it with the other data, so the data can be combined
trips_df_2019 %>% 
  rename(ride_id = trip_id
         ,rideable_type = bikeid
         ,started_at = start_time
         ,ended_at = end_time
         ,start_station_name = from_station_name
         ,start_station_id = from_station_id
         ,end_station_name = to_station_name
         ,end_station_id = to_station_id
         ,member_casual = usertype)

trips_df_2019 <-trips_df_2019 %>% 
  rename(ride_id = trip_id
         ,rideable_type = bikeid
         ,started_at = start_time
         ,ended_at = end_time
         ,start_station_name = from_station_name
         ,start_station_id = from_station_id
         ,end_station_name = to_station_name
         ,end_station_id = to_station_id
         ,member_casual = usertype)


## To look at type of data contain in the data set and to look at column names
str(trips_df_2019)       
str(trips_df_2020)

## Convert ride_id and rideable_type to character so that they can stack correctly
trips_df_2019 <- mutate(trips_df_2019, ride_id = as.character(ride_id)
                        ,rideable_type = as.character(rideable_type))

## Stack individual quarter's data frames into one big data frame
every_trip <- bind_rows(trips_df_2019,trips_df_2020)

## Remove lat, long, birthyear, and gender fields 
every_trip <- every_trip %>%
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, "tripduration"))

View(every_trip)

# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#List of column names
colnames(every_trip)

## How many rows are in data frame?
nrow(every_trip)

## Dimensions of the data frame?
dim(every_trip)

## See the first 6 rows of data frame
head(every_trip) 

tail(every_trip)

## See list of columns and data types (numeric, character, etc)
str(every_trip) 

## Statistical summary of data. Mainly for numeric
summary(every_trip) 

## Streamline the membership to two categories as there are four labels in the member_casual column
## Gives total of each subset in the column
table(every_trip$member_casual)

## (1)In the "member_casual" column, "Subscriber" will be replaced by "member" and "Customer" with
"casual"
## Making this change to have consistency throughout the data set
every_trip <- every_trip %>%
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))

## To reconfirm if the change was made and reassignment for properly done
table(every_trip$member_casual)

## (2) Should additional columns such as day, month, year -- that provide additional
## opportunities to aggregate the data.

# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... before completing
## these operations we could only aggregate at the ride level

## Before: We could only answer questions like:
### How long was this one ride?

## After: We can now answer questions like:
### How many rides happened in March?
### What's the average ride length each day?
### Do members ride more on weekends than weekdays?

# The default format for date is yyyy-mm-dd

every_trip$date <- as.Date(every_trip$started_at)

every_trip$month <- format(as.Date(every_trip$date), "%m")

every_trip$day <- format(as.Date(every_trip$date), "%d")

every_trip$year <- format(as.Date(every_trip$date), "%Y")

every_trip$day_of_week <- format(as.Date(every_trip$date), "%A")

# Add a "ride_length" calculation to all_trips (in seconds)
every_trip$ride_length <- difftime(every_trip$ended_at,every_trip$started_at)

View(every_trip)

## Inspect the structure of the columns
str(every_trip)

# Converting "ride_length" to numeric so we can run calculations on the data
## This line checks whether the ride_length column in my every_trip dataframe is a factor data type.
is.factor(every_trip$ride_length)

# Converting ride_length column into numeric
every_trip$ride_length <- as.numeric(as.character(every_trip$ride_length))
is.numeric(every_trip$ride_length)

## To check if there is a negative numbers or not
every_trip %>% 
  summarize(min(ride_length))
## Also can go filter it to look at it as all the negatives are associated with 
## start_station_name "HQ OR".

# Remove "bad" data
## The dataframe includes a few hundred entries when bikes were taken out of docks and
## checked for quality by Divvy or ride_length was negative.
# Create a new version of the dataframe (v2) since data is being removed
every_trip_v2<- every_trip[!(every_trip$start_station_name == "HQ QR" | every_trip$ride_length<0),]


View(every_trip_v2)

# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
## Total members vs Casual rides percentage
casual_weekend_ride <- every_trip_v2 %>%
  filter(member_casual == "casual") %>%
  mutate(day_of_week = wday(started_at, label = TRUE)) %>%
  filter(day_of_week %in% c("Sat", "Sun")) %>%
  count()
print(casual_ride)

casual_rides <- 71643
member_rides <- 720313
total_rides <- 791956

## Calculate percentages
casual_percent <- round(casual_weekend_ride / casual_rides, 2) * 100

member_percent <- round(member_rides / total_rides, 2) * 100
print(casual_percent)

print(member_percent)

## Descriptive analysis on ride_length (all figures in seconds)
## Straight average (total ride length / rides)
mean(every_trip_v2$ride_length)

every_trip_v2 %>% 
  summarize(average_ride_length=mean(ride_length))

## Midpoint number in the ascending array of ride lengths
every_trip_v2 %>% 
  summarize(median_ride_length=median(ride_length))

## Longest Ride
every_trip_v2 %>% 
  summarize(max_ride_length=max(ride_length))

## Shortest ride
every_trip_v2 %>% 
  summarize(min_ride_length=min(ride_length))


## Condensing the top four lines of code into one line
summary(every_trip_v2$ride_length)


# Compare members and casual users
every_trip_v2 %>% 
  group_by(member_casual) %>% 
  drop_na() %>% 
  summarize(average_ride_length = mean(ride_length))


every_trip_v2 %>% 
  group_by(member_casual) %>% 
  drop_na() %>% 
  summarize(median_ride_length = median(ride_length))

every_trip_v2 %>% 
  group_by(member_casual) %>% 
  drop_na() %>% 
  summarize(max_ride_length = max(ride_length))

every_trip_v2 %>% 
  group_by(member_casual) %>% 
  drop_na() %>% 
  summarize(min_ride_length = min(ride_length))


# See the average ride time by each day for members vs casual users
# Explanation of the code:
## ride_length ~ member_casual + day_of_week means: group by member_casual and day_of_week,
## then calculate something (mean) on ride_length.
## data = na.omit(every_trip_v2) removes rows with NA values.
## FUN = mean calculates the mean of each group.
aggregate(ride_length ~ member_casual + day_of_week, 
          data = na.omit(every_trip_v2), 
          FUN = mean)

## Notice that the days of the week are out of order. Put them in Order
every_trip_v2$day_of_week <-ordered(every_trip_v2$day_of_week, levels=c("Sunday", "Monday",
                                                                        "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

## Ran the same code before to check if the days of the week are in order now or not.
aggregate(ride_length ~ member_casual + day_of_week, 
          data = na.omit(every_trip_v2), 
          FUN = mean)   

# Analyze Riders data by type and weekday
every_trip_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% ## creates weekday field using wday()
  group_by(member_casual, weekday) %>% ## groups by user type and weekday
  summarise(number_of_rides = n() ## calculates the number of rides and average duration
            ,average_duration = mean(ride_length)) %>% ## calculates the average duration 
  arrange(member_casual, weekday) ## sorting by first member_casual and then by weekday


# Visualize the number of rides by rider type
every_trip_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Let's create a visualization for average duration
every_trip_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

# To analyze when members ride more during commute hours 
every_trip_v2 %>%
  mutate(hour = hour(started_at)) %>%  # Extract hour from start time
  group_by(member_casual, hour) %>%
  summarise(number_of_rides = n()) %>%
  ggplot(aes(x = hour, y = number_of_rides, color = member_casual)) +
  geom_line(size = 1) +
  labs(
    title = "Ride Frequency by Hour of Day",
    x = "Hour of Day (24-hour format)",
    y = "Number of Rides",
    color = "User Type"
  ) +
  theme_minimal()

#=================================================
