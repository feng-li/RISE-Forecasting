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


###The following code is used the data before 2020 to generate the baseline forecast

tslist2020=list()  ##the data before 2020
for( i in 1:20)
{
  l=length(tslist[[i]])
  tsl=tslist[[i]][1:(l-38)]
  tsl=ts(tsl,frequency = 12,end = c(2019,12))
  tslist2020[[i]]=tsl
}

arima2020=array(0,c(20,24)) ##arima
for(i in 1:20)
{
  x=tslist2020[[i]]
  m=auto.arima(x)
  fore=forecast(m,h=24)
  arima2020[i,]=fore$mean
}
arima2020=t(arima2020)

ets2020=array(0,c(20,24)) ##ets
for(i in 1:20)
{
  x=tslist2020[[i]]
  m=ets(x)
  fore=forecast(m,h=24)
  ets2020[i,]=fore$mean
}
ets2020=t(ets2020)

holt2020=array(0,c(20,24)) ##holt
for(i in 1:20)
{
  x=tslist2020[[i]]
  m=holt(x,h=24)
  fore=forecast(m,h=24)
  holt2020[i,]=fore$mean
}
holt2020=t(holt2020)

hw2020=array(0,c(20,24)) ##holt-winters
for(i in 1:20)
{
  x=tslist2020[[i]]
  m=hw(x,h=24)
  fore=forecast(m,h=24)
  hw2020[i,]=fore$mean
}
hw2020=t(hw2020)

rwf2020=array(0,c(20,24)) ##random_walk
for(i in 1:20)
{
  x=tslist2020[[i]]
  m=rwf(x,h=24)
  fore=forecast(m,h=24)
  rwf2020[i,]=fore$mean
}
rwf2020=t(rwf2020)

snaive2020=array(0,c(20,24)) ##seasonal naive
for(i in 1:20)
{
  x=tslist2020[[i]]
  m=snaive(x,h=24)
  fore=forecast(m,h=24)
  snaive2020[i,]=fore$mean
}
snaive2020=t(snaive2020)

nnetar2020=array(0,c(20,24)) ##neural network models
for(i in 1:20)
{
  x=tslist2020[[i]]
  m=nnetar(x)
  fore=forecast(m,h=24)
  nnetar2020[i,]=fore$mean
}
nnetar2020=t(nnetar2020)

tbats2020=array(0,c(20,24)) ##tbats
for(i in 1:20)
{
  x=tslist2020[[i]]
  m=tbats(x)
  fore=forecast(m,h=24)
  tbats2020[i,]=fore$mean
}
tbats2020=t(tbats2020)

file_path='D:\\university\\研究生\\tourism_forecasting_competition\\submit\\baseline_forecast\\'
setwd(file_path)
write.xlsx(data.frame(arima2020),'arima2020.xlsx')
write.xlsx(data.frame(ets2020),'ets2020.xlsx')
write.xlsx(data.frame(holt2020),'holt2020.xlsx')
write.xlsx(data.frame(hw2020),'hw2020.xlsx')
write.xlsx(data.frame(nnetar2020),'nnetar2020.xlsx')
write.xlsx(data.frame(rwf2020),'rwf2020.xlsx')
write.xlsx(data.frame(snaive2020),'snaive2020.xlsx')
write.xlsx(data.frame(tbats2020),'tbats2020.xlsx')