# LIBRARIES
library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)

library(scales)
library(cluster)
library(tidyverse)
library(factoextra)

# DATA READING
# - downloaded from https://data.fivethirtyeight.com/
apr_data <- read.csv("dataset/uber-raw-data-apr14.csv")
may_data <- read.csv("dataset/uber-raw-data-may14.csv")
jun_data <- read.csv("dataset/uber-raw-data-jun14.csv")

uber_data <- rbind(apr_data, may_data, jun_data)

# PREPROCESSING
uber_data$Date.Time <- as.POSIXct(uber_data$Date.Time, format = "%m/%d/%Y %H:%M:%S")

uber_data$Time <- format(uber_data$Date.Time, format="%H:%M:%S")

uber_data$Date.Time <- ymd_hms(uber_data$Date.Time)

uber_data$day <- factor(day(uber_data$Date.Time))
uber_data$month <- factor(month(uber_data$Date.Time, label = TRUE))
uber_data$year <- factor(year(uber_data$Date.Time))
uber_data$dayofweek <- factor(wday(uber_data$Date.Time, label = TRUE))

uber_data$hour <- factor(hour(hms(uber_data$Time)))
uber_data$minute <- factor(minute(hms(uber_data$Time)))
uber_data$second <- factor(second(hms(uber_data$Time)))

# CLEANING
sum(is.null(uber_data))

# PLOTTING: trips by hours in a day
hour_data<-uber_data %>%
  group_by(hour) %>%
  dplyr::summarize(Total = n())

datatable(hour_data)

ggplot(hour_data, aes(hour, Total)) + 
  geom_bar(stat = "identity", fill = "steelblue", color = "red") +
  ggtitle("Trips Every Hour") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

month_hour <- uber_data %>%
  group_by(month, hour) %>%
  dplyr::summarize(Total = n())

ggplot(month_hour, aes(hour, Total, fill = month)) + 
  geom_bar(stat = "identity") +
  ggtitle("Trips by Hour and Month") +
  scale_y_continuous(labels = comma)

# PLOTTING: uber_data by trips during every day of the month
colors = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")

day_group <- uber_data %>%
  group_by(day) %>%
  dplyr::summarize(Total = n())

datatable(day_group)

ggplot(day_group, aes(day, Total)) + 
  geom_bar( stat = "identity", fill = "steelblue") +
  ggtitle("Trips Every Day") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

day_month_group <- uber_data %>%
  group_by(month, day) %>%
  dplyr::summarize(Total = n())

ggplot(day_month_group, aes(day, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Day and Month") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)

month_group <- uber_data %>%
  group_by(month) %>%
  dplyr::summarize(Total = n())

datatable(month_group)

ggplot(month_group, aes(month, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Month") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)

month_weekday <- uber_data %>%
  group_by(month, dayofweek) %>%
  dplyr::summarize(Total = n())

ggplot(month_weekday, aes(month, Total, fill = dayofweek)) + 
  geom_bar( stat = "identity", position = "dodge") +
  ggtitle("Trips by Day and Month") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)


# Finding out the number of Trips by bases
ggplot(uber_data, aes(Base)) + 
  geom_bar(fill = "darkred") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases")

ggplot(uber_data, aes(Base, fill = month)) + 
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and Month") +
  scale_fill_manual(values = colors)

ggplot(uber_data, aes(Base, fill = dayofweek)) + 
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and Day of Week") +
  scale_fill_manual(values = colors)


# Creating a Heatmap visualization of day, hour and month
day_and_hour <- uber_data %>%
  group_by(day, hour) %>%
  dplyr::summarize(Total = n())

datatable(day_and_hour)

ggplot(day_and_hour, aes(day, hour, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heatmap by Hour and Day")

ggplot(day_month_group, aes(day, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heatmap by Month and Day")

ggplot(month_weekday, aes(dayofweek, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heatmap by Month and Day of Week")

month_base <-  uber_data %>%
  group_by(Base, month) %>%
  dplyr::summarize(Total = n())

ggplot(month_base, aes(Base, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heatmap by Month and Bases")

dayofweek_bases <-  uber_data %>%
  group_by(Base, dayofweek) %>%
  dplyr::summarize(Total = n())

ggplot(dayofweek_bases, aes(Base, dayofweek, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heatmap by Bases and Day of Week")

# K-MEANS CLUSTERING
# - https://towardsdatascience.com/how-does-uber-use-clustering-43b21e3e6b7d
clus <- uber_data %>%
  group_by(Lat, Lon) %>%
  dplyr::summarize()

# SCALING -> to avoid arbitrary variable unit dependency
# clus <- scale(clus)

# rmd: let's use k=6 for this case
k2 <- kmeans(clus, centers = 2, nstart = 25)
str(k2)
k2

fviz_cluster(k2, data = clus, main = "Clusterfook")