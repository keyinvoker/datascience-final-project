# LIBRARIES
library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
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


# PLOTTING: trips by hours in a day
hour_data<-uber_data %>%
  group_by(hour) %>%
  dplyr::summarize(Total = n())

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


# PLOTTING: trips during every day of the month
colors = c("orange", "red", "blue", "green", "purple", "black", "pink")

day_group <- uber_data %>%
  group_by(day) %>%
  dplyr::summarize(Total = n())

ggplot(day_group, aes(day, Total)) + 
  geom_bar(stat = "identity", fill = "steelblue") +
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


# CALCULATING NUMBER OF TRIPS USING BASES
ggplot(uber_data, aes(Base)) + 
  geom_bar(fill = "darkred") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases")

ggplot(uber_data, aes(Base, fill = month)) + 
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Month and Bases") +
  scale_fill_manual(values = colors)

# HEATMAP
month_base <-  uber_data %>%
  group_by(Base, month) %>%
  dplyr::summarize(Total = n())

ggplot(month_base, aes(Base, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heatmap by Month and Bases")

# MAP - NYC based on Uber Ride Orders
min_lat <- 40.5774
max_lat <- 40.9176
min_long <- -74.15
max_long <- -73.7004

ggplot(uber_data, aes(x=Lon, y=Lat, color = Base)) +
  geom_point(size=1) +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC Map Based On Uber Ride Orders During Apr-Jun 2014 by Base")

# K-MEANS CLUSTERING (?)
# - https://towardsdatascience.com/how-does-uber-use-clustering-43b21e3e6b7d
# - https://uc-r.github.io/kmeans_clustering
clus <- uber_data %>%
  group_by(Lat, Lon) %>%
  dplyr::summarize()

# rmd: let's use k=6 for this case
k2 <- kmeans(clus, centers = 2, nstart = 25)
str(k2)
fviz_cluster(k2, data = clus, main = "Uber Spread by Cluster with 2 Centers")