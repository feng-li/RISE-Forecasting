library(forecast)
library(tsfeatures)
library(gratis)
library(imputeTS)
library(ggplot2)
library(readxl)
library(openxlsx)

work_path='D:\\university\\研究生\\tourism_forecasting_competition\\submit\\'  
setwd(work_path)
###the work path must be readjusted before running the code
data=read_excel('data.xlsx')

tslist=list()  ##every element in the list is a time series
for( i in 2:21)
{
  loc=min(which(is.na(data[,i])==FALSE))-1
  zc=loc%/%12
  yu=loc%%12
  r=which(is.na(data[(loc+1):410,i])==TRUE)
  x=data[(loc+1):410,i]
  if (length(r)!=0){
    x=na_kalman(x)
    ###interpolating the missing value with Kalman filtering 
  }
  xts=ts(x,start=c((1989+zc),(1+yu)),frequency=12)
  tslist[[i-1]]=xts
}


###The following code is used the data before 2018 to generate the training set for stacking model

tslist2018=list()  ##the data before 2018
for( i in 1:20)
{
  l=length(tslist[[i]])
  tsl=tslist[[i]][1:(l-62)]
  tsl=ts(tsl,frequency = 12,end = c(2017,12))
  tslist2018[[i]]=tsl
}

arima2018=array(0,c(20,24)) ##arima
for(i in 1:20)
{
  x=tslist2018[[i]]
  m=auto.arima(x)
  fore=forecast(m,h=24)
  arima2018[i,]=fore$mean
}
arima2018=t(arima2018)

ets2018=array(0,c(20,24)) ##ets
for(i in 1:20)
{
  x=tslist2018[[i]]
  m=ets(x)
  fore=forecast(m,h=24)
  ets2018[i,]=fore$mean
}
ets2018=t(ets2018)

holt2018=array(0,c(20,24)) ##holt
for(i in 1:20)
{
  x=tslist2018[[i]]
  m=holt(x,h=24)
  fore=forecast(m,h=24)
  holt2018[i,]=fore$mean
}
holt2018=t(holt2018)

hw2018=array(0,c(20,24)) ##holt-winters
for(i in 1:20)
{
  x=tslist2018[[i]]
  m=hw(x,h=24)
  fore=forecast(m,h=24)
  hw2018[i,]=fore$mean
}
hw2018=t(hw2018)

rwf2018=array(0,c(20,24)) ##random_walk
for(i in 1:20)
{
  x=tslist2018[[i]]
  m=rwf(x,h=24)
  fore=forecast(m,h=24)
  rwf2018[i,]=fore$mean
}
rwf2018=t(rwf2018)

snaive2018=array(0,c(20,24)) ##seasonal naive
for(i in 1:20)
{
  x=tslist2018[[i]]
  m=snaive(x,h=24)
  fore=forecast(m,h=24)
  snaive2018[i,]=fore$mean
}
snaive2018=t(snaive2018)

nnetar2018=array(0,c(20,24)) ##neural network models
for(i in 1:20)
{
  x=tslist2018[[i]]
  m=nnetar(x)
  fore=forecast(m,h=24)
  nnetar2018[i,]=fore$mean
}
nnetar2018=t(nnetar2018)

tbats2018=array(0,c(20,24)) ##tbats
for(i in 1:20)
{
  x=tslist2018[[i]]
  m=tbats(x)
  fore=forecast(m,h=24)
  tbats2018[i,]=fore$mean
}
tbats2018=t(tbats2018)

file_path='D:\\university\\研究生\\tourism_forecasting_competition\\submit\\stacking_train\\'
setwd(file_path)
write.xlsx(data.frame(arima2018),'arima2018.xlsx')
write.xlsx(data.frame(ets2018),'ets2018.xlsx')
write.xlsx(data.frame(holt2018),'holt2018.xlsx')
write.xlsx(data.frame(hw2018),'hw2018.xlsx')
write.xlsx(data.frame(nnetar2018),'nnetar2018.xlsx')
write.xlsx(data.frame(rwf2018),'rwf2018.xlsx')
write.xlsx(data.frame(snaive2018),'snaive2018.xlsx')
write.xlsx(data.frame(tbats2018),'tbats2018.xlsx')