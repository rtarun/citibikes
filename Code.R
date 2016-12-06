library(ggplot2)
library(dplyr)
library(scale)
library(scales)
library(RPostgreSQL)


df$year = 0
df$date = 0
df$hour = 0
df$month = 0

for (i in 1:nrow(original))
{
  array <- strsplit(x = df$starttime[i], split = "/")
  array2 <- strsplit(x = array[[1]][3], split = ":")
  final <- strsplit(x = array2[[1]][1], split = " ")
  df$year[i] = final[[1]][1]
  df$hour[i] = final[[1]][2]
  df$date[i] = array[[1]][2]
  df$month[i] = array[[1]][1]
  
}

daily3 <- read.csv('daily3.csv')
wdays <- read.csv('wdays.csv')
weekends <- read.csv('weekends.csv')
days <- read.csv('days.csv')

#WEEKDAYS#

p <- ggplot(data = wdays, aes(x = Hours, y = Trips)) +
  geom_bar(stat = "identity", fill = "deepskyblue2") +
  scale_x_discrete("Hour of Day") +
  scale_y_continuous("Average Hourly Trips", labels = comma) 
title_with_subtitle("NYC Citi Bike Trips by Hour of Day - Weekdays")
p + labs(title = "Citi Bike Trips by Hour of Day in New York City - Weekdays",subtitle = "From September to November 2015")+ theme(axis.text.x = element_text(angle=90,hjust = 1))



#WEEKENDS#

q <- ggplot(data = weekends, aes(x = Hours, y = Trips)) +
  geom_bar(stat = "identity", fill = "dodgerblue1") +
  scale_x_discrete("Hour of Day") +
  scale_y_continuous("Average Hourly Trips", labels = comma) +
  title_with_subtitle("Citi Bike Trips by Hour of Day in New York City - Weekends", "From September to November 2015") 
q + labs(title = "Citi Bike Trips by Hour of Day in New York City - Weekends")+ theme(axis.text.x = element_text(angle=90,hjust = 1))



#DAYS#

days$Day
days$Day <- factor(days$Day, levels = days$Day[order(days$Trips)])
r <- ggplot(data = days, aes(x = Day, y = Trips)) +
  geom_bar(stat = "identity", fill = "deepskyblue2") +
  scale_x_discrete("Day of the week") +
  scale_y_continuous("Average Daily Trips\n", labels = comma)
  r + labs(title = "Citi Bike Trips by Day of Week in New York City",subtitle = "From September to November 2015")

#YEAR#
  s <- ggplot(data = daily3.csv, aes(x =  Month, y = Total.Monthly.Trip , group = 1)) +
    geom_line(size = 1, color = "dodgerblue1") +
    scale_x_discrete("Month of the Year") +
    scale_y_continuous("Total Monthly Trips", labels = comma) 
  title_with_subtitle("NYC Monthly Citi Bike Trips")
  s + labs(title = "New York City Monthly Citi Bike Trips , 2014 - 2015")+ theme(axis.text.x = element_text(angle=90,hjust = 1))
