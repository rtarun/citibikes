dt = read.csv("C:\\Users\\Afeez\\Documents\\RProject\\bike_day_weather.csv")
head(dt)
library(ggplot2)
library(data.table)
str(dt)
dt = as.data.table(dt)
dt$avgtemp = (dt$TMAX + dt$TMIN)/2
dt$DATE = as.Date(as.character(dt$DATE), "%Y%m%d")
attach(dt)
pl1 = ggplot(dt,aes(x=DATE,y=tripstarted, color=avgtemp)) + geom_point(shape=1) + scale_color_gradientn(colors=c("blue","green","yellow","red"))
pl2 = pl1 + theme_bw() + labs(title = 'Trend of Daily Bike Traffic', x = 'Day of the Year', y = 'Number of Trips Started')
pl2
