rm(list=ls(all=TRUE))

# Set the local directory!!
setwd("/Users/tarunruchandani/Desktop/Fall2016/IEORE4650_BusinessAnalytics/Project/Citibike-BA 2/")

#install.packages("data.table")
library(data.table)

#parameters
min_interval = 15


bike = data.table()
#dates = c(seq(201307, 201312), seq(201401,201412), seq(201501,201512), seq(201601,201609))
dates = seq(201601,201609)
#dates = c(201609)

time = c(Sys.time())
for (d in dates)
{
    bike = rbind(bike, fread(paste("csv/", d,"-citibike-tripdata.csv", sep = "")))
    time = c(time, Sys.time())
}

colnames(bike)

time = c(time, Sys.time())
bike[,"startdate"] = as.Date(bike[,starttime], "%m/%d/%Y")
bike[,"starthour"] = strptime(bike[,starttime], format="%m/%j/%Y %H:%M:%S")$hour*100 + floor(strptime(bike[,starttime], format="%m/%j/%Y %H:%M:%S")$min/min_interval)*min_interval
time = c(time, Sys.time())
bike[,"stopdate"] = as.Date(bike[,stoptime], "%m/%d/%Y")
bike[,"stophour"] = strptime(bike[,stoptime], format="%m/%j/%Y %H:%M:%S")$hour*100 + floor(strptime(bike[,stoptime], format="%m/%j/%Y %H:%M:%S")$min/min_interval)*min_interval
time = c(time, Sys.time())
bike[,"male"] = bike[,gender==1]
bike[,"female"] = bike[,gender==2]
bike[,"unknown_gender"] = bike[,gender==0]
bike[,"customer"] = bike[,usertype=="Customer"]
bike[,"subscriber"] = bike[,usertype=="Subscriber"]

bike_summary_start = bike[, .(tripstarted=.N, male=sum(male), female=sum(female),unknown_gender=sum(unknown_gender), customer=sum(customer), subscriber=sum(subscriber)),keyby=.(`start station id`,startdate, starthour)]
bike_summary_end = bike[, .(tripended=.N, male=sum(male), female=sum(female),unknown_gender=sum(unknown_gender), customer=sum(customer), subscriber=sum(subscriber)),keyby=.(`end station id`, stopdate, stophour)]

weather = fread("weather/201307-201609-weather-LGA.csv")

weather[,"date"] = as.Date(weather[,valid], "%m/%d/%y")
weather[,"time"] = strptime(weather[,valid], format="%m/%j/%y %H:%M")$hour*100 + floor(strptime(weather[,valid], format="%m/%j/%y %H:%M")$min/min_interval)*min_interval
weather[,"sky"] = weather[,pmax(skyc1,skyc2,skyc3,skyc4, na.rm = TRUE)]

#weather_summary = weather[, .( tmp=mean(tmpf,na.rm = TRUE), wind=mean(sknt, na.rm = TRUE), rain=mean(p01i,na.rm = TRUE), sky=max(sky, na.rm = TRUE) ), keyby=.(date, time)]
weather_summary = weather[, .( tmp=mean(tmpf,na.rm = TRUE), wind=mean(sknt, na.rm = TRUE), rain=max(p01i,na.rm = TRUE), sky=max(sky, na.rm = TRUE) ), keyby=.(date)]
weather_summary[sky==-Inf] = NA
weather_summary$sky=factor(weather_summary$sky)

