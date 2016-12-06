#install.packages("data.table")
#install.packages("GGally")
#install.packages("leaps")
library(data.table) #upgraded version with higher performances of data.frame
library(GGally) #Plotting correlation graphs
library(leaps) # Best subset selection package


#Loads the processed data csv file
bike_day = fread("bike_day.csv")


# Weather processing
weather = fread("weather/central_park_weather.csv")
colnames(weather)
weather$STATION = NULL
weather$STATION_NAME = NULL
weather[,"cdate"] = as.Date(paste(substr(weather$DATE,1,4), substr(weather$DATE,5,6), substr(weather$DATE,7,8),  sep = "-"), "%Y-%m-%d")
weather$AWND = pmax(0,weather$AWND)


# Joining bike_day with weather, matching on date
setkey(weather,cdate)
setkey(bike_day,startdate)
bike_day_weather = weather[bike_day, nomatch=0] #inner join on date


# Here start the analysis of the data

#Correlation graphs
ggpairs(bike_day_weather[,.(tripstarted, TMIN,TMAX, PRCP, AWND, SNOW, SNWD)])

#Adding a weekend indicator - categorical variable
bike_day_weather$weekend = factor(bike_day_weather$weekend)
bike_day_weather$id = seq.int(nrow(bike_day_weather))

#Dividing the data into training/validation/test set, with respect to the chronology
train = bike_day_weather[cdate <= as.Date("12/31/2014", "%m/%d/%Y"), id]
validation = bike_day_weather[cdate <= as.Date("06/30/2015", "%m/%d/%Y") & cdate > as.Date("12/31/2014", "%m/%d/%Y"), id]
test = bike_day_weather[cdate > as.Date("06/30/2015", "%m/%d/%Y"), id]

#Prediction function for the best subset selection
predict.regsubsets=function(regfit.full,newdata,t){
  form=as.formula(regfit.full$call[[2]])
  mat=model.matrix(form,newdata) #mat = model.matrix(Salary~., newdata)
  coefi=coef(regfit.full,id=t) #obtain the coefficients of the model corresponding to t
  xvars=names(coefi)
  pred = mat[,xvars]%*%coefi
  return(pred)
}

#Maximum number of variable to use
p=6


x = bike_day_weather[,.(tripstarted, weekend, PRCP, SNOW, SNWD, TMAX, AWND)] #Preparing the dataset
regfit.full=regsubsets(tripstarted~.,data=x[train,],nvmax=p) #Best subset selection on the training set
reg.summary=summary(regfit.full)
print(reg.summary)
reg.summary$rsq # R-squared

#MSE calculation on the validation set
bestSubsetMSE = array(NA,dim=c(p,1)) 
for (t in 1:p) #Prediction for each best subset result
{
  pred = predict.regsubsets(regfit.full, x[validation,], t)
  actual = x[validation, tripstarted]
  bestSubsetMSE[t,1] = mean((actual-pred)^2) #test set MSE
}
bestSubsetMSE

m = which.min(bestSubsetMSE) #Choosing the number of variables with the lowest MSE

#Retraining the model on the whole training and validation sets
reg = lm(x[c(train,validation),tripstarted]~., data=x[c(train,validation)])
summary(reg)
predTest = predict(reg, x[test,], interval="prediction")
actualTest = x[test, tripstarted]
testMSE = mean((actualTest-predTest)^2) #MSE on test set
testMSE


#Some graphs

#summary(bike_day_weather$TMAX)
temperature = bike_day_weather[,.(trips=mean(tripstarted)), keyby=TMAX]
plot(temperature$TMAX, temperature$trips)

rain = bike_day_weather[,.(trips=mean(tripstarted)), keyby=PRCP]
plot(rain$PRCP, rain$trips)


#Analysis of the influence of weather on the subscribers only, assumed to be New Yorkers

x = bike_day_weather[,.(subscriber=(subscriber/sum(subscriber)*100), weekend, PRCP, SNOW, SNWD, TMAX, AWND)] 
regfit.full=regsubsets(subscriber~.,data=x[train,],nvmax=p)
reg.summary=summary(regfit.full)
print(reg.summary)
reg.summary$rsq

bestSubsetMSE = array(NA,dim=c(p,1)) 
for (t in 1:p) #Prediction for each best subset result
{
  pred = predict.regsubsets(regfit.full, x[validation,], t)
  actual = x[validation, subscriber]
  bestSubsetMSE[t,1] = mean((actual-pred)^2) #test set MSE
}
bestSubsetMSE

m = which.min(bestSubsetMSE)
m

reg = lm(x[c(train,validation),subscriber]~., data=x[c(train,validation)])
summary(reg)
predTest = predict(reg, x[test,], interval="prediction")
actualTest = x[test, subscriber]
testMSE = mean((actualTest-predTest)^2)
testMSE

#Analysis of the influence of weather on the customers only, assumed to be tourists

x = bike_day_weather[,.(customer=(customer/sum(customer)*100), weekend, PRCP, SNOW, SNWD, TMAX, AWND)]
regfit.full=regsubsets(customer~.,data=x[train,],nvmax=p)
reg.summary=summary(regfit.full)
print(reg.summary)
reg.summary$rsq

bestSubsetMSE = array(NA,dim=c(p,1)) 
for (t in 1:p) #Prediction for each best subset result
{
  pred = predict.regsubsets(regfit.full, x[validation,], t)
  actual = x[validation, customer]
  bestSubsetMSE[t,1] = mean((actual-pred)^2) #test set MSE
}
bestSubsetMSE

m = which.min(bestSubsetMSE)
m

reg = lm(x[c(train,validation),customer]~., data=x[c(train,validation)])
summary(reg)
predTest = predict(reg, x[test,], interval="prediction")
actualTest = x[test, customer]
testMSE = mean((actualTest-predTest)^2)
testMSE
