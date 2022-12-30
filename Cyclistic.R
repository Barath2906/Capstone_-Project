### Cyclistic  ###

# This analysis is based on the Divvy case study "'Sophisticated, Clear, and Polished’: Divvy and Data Visualization" written by Kevin Hartman (found here: https://artscience.blog/home/divvy-dataviz-case-study). The purpose of this script is to consolidate downloaded Divvy data into a single dataframe and then conduct simple analysis to help answer the key question: “In what ways do members and casual riders use Divvy bikes differently?”

# # # # # # # # # # # # # # # # # # # # # # # 
# Install required packages
# tidyverse for data import and wrangling
# lubridate for date functions
# ggplot for visualization
# # # # # # # # # # # # # # # # # # # # # # #  

library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
getwd() #displays your working directory
setwd("E:/Study/R/Cyclistic/Dataset") #sets your working directory to simplify calls to data ... make sure to use your OWN username instead of mine ;)

#=====================
# STEP 1: COLLECT DATA
#=====================
# Upload csv files
jan_2021 <- read_csv("202101-divvy-tripdata.csv")
feb_2021 <- read_csv("202102-divvy-tripdata.csv")
mar_2021 <- read_csv("202103-divvy-tripdata.csv")
apr_2021 <- read_csv("202104-divvy-tripdata.csv")
may_2021 <- read_csv("202105-divvy-tripdata.csv")
jun_2021 <- read_csv("202106-divvy-tripdata.csv")
jul_2021 <- read_csv("202107-divvy-tripdata.csv")
aug_2021 <- read_csv("202108-divvy-tripdata.csv")
sep_2021 <- read_csv("202109-divvy-tripdata.csv")
oct_2021 <- read_csv("202110-divvy-tripdata.csv")
nov_2021 <- read_csv("202111-divvy-tripdata.csv")


#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================
# Compare column names each of the files
# The names of all columns dO need to match perfectly before we can use a 
# command to join them into one file
colnames(jan_2021)
colnames(feb_2021)
colnames(mar_2021)
colnames(apr_2021)
colnames(may_2021)
colnames(jun_2021)
colnames(jul_2021)
colnames(aug_2021)
colnames(sep_2021)
colnames(oct_2021)
colnames(nov_2021)


# Inspect the dataframes and look for incongruencies
str(jan_2021)
str(feb_2021)
str(mar_2021)
str(apr_2021)
str(may_2021)
str(jun_2021)
str(jul_2021)
str(aug_2021)
str(sep_2021)
str(oct_2021)
str(nov_2021)

# Stack all data frames into one big data frame
trips_2021 <- bind_rows(jan_2021, feb_2021, mar_2021, apr_2021, may_2021, 
                       jun_2021, jul_2021, aug_2021, sep_2021, oct_2021, 
                       nov_2021)

# Remove unwanted fields from the combined dataframe
trips_2021_req <- trips_2021 %>%  
  select(-c(start_station_id, end_station_id, start_lat, start_lng, 
            end_lat, end_lng))

#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# Inspect the new table that has been created
colnames(trips_2021_req)  #List of column names
nrow(trips_2021_req)  #How many rows are in data frame?
dim(trips_2021_req)  #Dimensions of the data frame?
head(trips_2021_req)  #See the first 6 rows of data frame.
str(trips_2021_req)  #See list of columns and data types (numeric, character, etc)
summary(trips_2021_req)  #Statistical summary of data. Mainly for numerics

# There are a few problems we will need to fix:
# (1) In the "member_casual" column, there are two names for members 
#     ("member" and "Subscriber") and two names for casual riders ("Customer" and "casual"). 
#     We will need to consolidate that from four to two labels.
# (2) The data can only be aggregated at the ride-level, which is too granular. 
#     We will want to add some additional columns of data -- such as day, month, 
#     year -- that provide additional opportunities to aggregate the data.
# (3) We will want to add a calculated field for length of ride since the 2020Q1 
#     data did not have the "tripduration" column. We will add "ride_length" to the 
#     entire dataframe for consistency.
# (4) There are some rides where tripduration shows up as negative, including several
#     hundred rides where Divvy took bikes out of circulation for Quality Control 
#     reasons. We will want to delete these rides.

# In the "member_casual" column, replace "Subscriber" with "member" and "Customer" with "casual"
# Before 2020, Divvy used different labels for these two types of riders ... we will want to make our dataframe consistent with their current nomenclature
# N.B.: "Level" is a special property of a column that is retained even if a subset does not contain any values from a specific level
# Begin by seeing how many observations fall under each usertype
table(trips_2021_req$member_casual)

# Reassign to the desired values 
trips_2021_req <-  trips_2021_req %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))

# Check to make sure the proper number of observations were reassigned
table(trips_2021_req$member_casual)

# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year
# before completing these operations we could only aggregate at the ride level
# https://www.statmethods.net/input/dates.html more on date formats in R found 
# at that link
trips_2021_req$date <- as.Date(trips_2021_req$started_at) #The default format is yyyy-mm-dd
trips_2021_req$month <- format(as.Date(trips_2021_req$date), "%m")
trips_2021_req$day <- format(as.Date(trips_2021_req$date), "%d")
trips_2021_req$year <- format(as.Date(trips_2021_req$date), "%Y")
trips_2021_req$day_of_week <- format(as.Date(trips_2021_req$date), "%A")

# Add a "ride_length" calculation to all_trips (in seconds)
trips_2021_req$ride_length <- difftime(trips_2021_req$ended_at,
                                       trips_2021_req$started_at)
# Inspect the structure of the columns
str(trips_2021_req)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(trips_2021_req$ride_length)
trips_2021_req$ride_length <- as.numeric(as.character(trips_2021_req$ride_length))
is.numeric(trips_2021_req$ride_length)

# Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of docks 
# and checked for quality by Divvy or ride_length was negative
# We will create a new version of the dataframe (v2) since data is being removed
trips_2021_req_v2 <- trips_2021_req[!(trips_2021_req$start_station_name == "HQ QR" | trips_2021_req$ride_length<0),]

str(trips_2021_req_v2)

#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================
# Descriptive analysis on ride_length (all figures in seconds)
mean(trips_2021_req_v2$ride_length, na.rm = TRUE) #straight average (total ride length / rides)
median(trips_2021_req_v2$ride_length, na.rm = TRUE) #midpoint number in the ascending array of ride lengths
max(trips_2021_req_v2$ride_length, na.rm = TRUE) #longest ride
min(trips_2021_req_v2$ride_length, na.rm = TRUE) #shortest ride

# You can condense the four lines above to one line using summary() on the 
# specific attribute
summary(trips_2021_req_v2$ride_length)

# Compare members and casual users
aggregate(trips_2021_req_v2$ride_length ~ trips_2021_req_v2$member_casual, FUN = mean)
aggregate(trips_2021_req_v2$ride_length ~ trips_2021_req_v2$member_casual, FUN = median)
aggregate(trips_2021_req_v2$ride_length ~ trips_2021_req_v2$member_casual, FUN = max)
aggregate(trips_2021_req_v2$ride_length ~ trips_2021_req_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(trips_2021_req_v2$ride_length ~ trips_2021_req_v2$member_casual
          + trips_2021_req_v2$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
trips_2021_req_v2$day_of_week <- ordered(trips_2021_req_v2$day_of_week, 
                              levels=c("Sunday", "Monday", "Tuesday", "Wednesday", 
                                       "Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(trips_2021_req_v2$ride_length ~ trips_2021_req_v2$member_casual + 
            trips_2021_req_v2$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
trips_2021_req_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts

# Let's visualize the number of rides by rider type
trips_2021_req_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Let's create a visualization for average duration
trips_2021_req_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Create a csv file that we will visualize in Excel, Tableau, or my presentation software
# N.B.: This file location is for a Mac. If you are working on a PC, change the file location accordingly (most likely "C:\Users\YOUR_USERNAME\Desktop\...") to export the data. You can read more here: https://datatofish.com/export-dataframe-to-csv-in-r/
counts <- aggregate(trips_2021_req_v2$ride_length ~ trips_2021_req_v2$member_casual + 
                      trips_2021_req_v2$day_of_week, FUN = mean)
write.csv(counts, file = 'E:/Study/R/Cyclistic/Dataset/avg_ride_length.csv')


