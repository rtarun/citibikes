#install.packages('devtools')
#library(devtools)
#install.packages("gmapsdistance")
#Package computes estimates for time and distance between points using the Google Maps Distance Matrix API
#https://cran.r-project.org/web/packages/gmapsdistance/gmapsdistance.pdf
library(gmapsdistance) 

library(data.table)

###Acquiring the data
bike = data.table()
dates = 201601

for (d in dates)
{
  bike = rbind(bike, fread(paste("csv/", d,"-citibike-tripdata.csv", sep = "")))
  time = c(time, Sys.time())
}

###Data Cleaning
bike$`start station id` = as.integer(bike$`start station id`)
bike$`end station id` = as.integer(bike$`end station id`)
bike$starttime = as.POSIXct(bike$starttime, format = "%m/%d/%Y %H:%M:%S")
bike$stoptime = as.POSIXct(bike$stoptime, format = "%m/%d/%Y %H:%M:%S")
bike$starthr = as.numeric(format(bike$starttime, format = "%H"))

###Extracting the rush hour data
rush_hr = bike[starthr %in% c(7,8,9,16,17,18)] #rush hour traffic is assumed as 7-10 am and 4-7 pm
rush_hr$startloc = rush_hr[,as.character(paste(rush_hr$`start station latitude`,rush_hr$`start station longitude`,sep = "+"))]
rush_hr$endloc = rush_hr[,as.character(paste(rush_hr$`end station latitude`,rush_hr$`end station longitude`,sep = "+"))]
rush_hr$subscriber = ifelse(rush_hr$usertype == "Subscriber",1,0)
rush_hr$gend = ifelse(rush_hr$gender == 1,1,0)
rush_hr$age = rush_hr[,as.numeric(format(Sys.Date(), format = "%Y"))-as.numeric(rush_hr$`birth year`)]
rush_hr$tripduration = rush_hr[,as.numeric(rush_hr$tripduration)]

#Randomly selecting 2500 rows. 2500 limit is set by Google's standard API key
set.seed(2)
indx = sample(1:nrow(rush_hr),2500)
rush_hr = rush_hr[indx, .(starttime,starthr,tripduration,startloc,endloc,subscriber,gend,age)]

###Setting the given Google API key
set.api.key("AIzaSyCSCBAi1b66lghnX0tQaITRC0Umkpi0dOY")

#Pinging the Google Maps Distance Matrix API to get trip duration and distance estimates
for (i in 1:nrow(rush_hr)) {
  dist = gmapsdistance(origin = rush_hr$startloc[i], destination = rush_hr$endloc[i], mode = "bicycling")
  rush_hr$estimated_duration[i] = dist$Time
  rush_hr$estimated_distance[i] = dist$Distance
  rush_hr$estimated_diff_duration[i] = rush_hr$tripduration[i] - rush_hr$estimated_duration[i]
}

#clean datatable of NA values in age column
rush_hr = rush_hr[!is.na(rush_hr$age)]
#Removing Google duration estimates equal to 0
rush_hr = rush_hr[!(rush_hr$estimated_duration==0),]
#Excluding tourists
rush_hr = rush_hr[rush_hr$subscriber==1,]

#Isolating and Analyzing the slow and fast riders
slow_riders = rush_hr[rush_hr$estimated_diff_duration > 0,]
summary(slow_riders) #average age is 41; median is 39
proportion_of_males_slow = sum(slow_riders$gend)/nrow(slow_riders) #74% of slow riders are male

fast_riders = rush_hr[data.clean$estimated_diff_duration < 0,]
summary(fast_riders) #average age is 38; median is 37
proportion_of_males_fast = sum(fast_riders$gend)/nrow(fast_riders) #88% of fast riders are male

#More generally, 62% of all riders are slower than expected
nrow(rush_hr[rush_hr$estimated_diff_duration>0,])/nrow(datas)

#Some quick correlation estimates
cor(tripduration,age)
# 0.08 : only statistically significant factor that affects the difference in time estimates; low correlation, though
cor(tripduration,estimated_duration)
# 0.51 : surprisingly low correlation. Apparently, riders are usually way off Google estimates!
cor(tripduration,estimated_distance)
# 0.51 : Also surprisingly low correlations

###Creating Linear Model
#Create 10 equally size folds for 10-Fold Cross Validation
folds <- cut(seq(1,nrow(rush_hr)),breaks=10,labels=FALSE)
errors = rep(0,10)
attach(rush_hr)

#Perform 10 fold-cross validation
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- rush_hr[testIndexes, ]
  trainData <- rush_hr[-testIndexes, ]
  model.cv = lm(estimated_diff_duration~estimated_distance+gend+age, data = trainData)
  errors[i] = mean((testData$estimated_diff_duration - predict(model.cv, testData))^2)
}

least.error = which.min(errors) #index of model with least error
testIndexes <- which(folds==least.error,arr.ind=TRUE)
testData <- rush_hr[testIndexes, ]
trainData <- rush_hr[-testIndexes, ]
#building selected model with the entire dataset
model.final = lm(estimated_diff_duration~estimated_distance+gend+age, data = rush_hr)
summary(model.final) #gender and age are significant!
