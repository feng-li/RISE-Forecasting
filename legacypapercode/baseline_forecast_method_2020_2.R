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

tslist2020=list()  ##the data before 2020
for( i in 1:20)
{
  l=length(tslist[[i]])
  tsl=tslist[[i]][1:(l-38)]
  tsl=ts(tsl,frequency = 12,end = c(2019,12))
  tslist2020[[i]]=tsl
}

#Time series stl decomposition
stllist=list()
for(i in 1:20)
{    
  a=mstl(tslist2020[[i]])
  stllist[[i]]=a
}

#Method 1: The trend term uses the base model forecast, the seasonal term 
#and the error term uses the average of the corresponding months over the past 3 years.
arimastlA2020=matrix(0,24,20)
arimastlarray=array(0,c(20,24,3))
for(i in 1:20)
{
  l=dim(stllist[[i]])[1]
  model=auto.arima(stllist[[i]][,2])
  fore=forecast(model,h=24)
  array=matrix(stllist[[i]][(l-35):l,3],c(12,3))
  season=apply(array,1,mean)
  array=matrix(stllist[[i]][(l-35):l,4],c(12,3))
  res=apply(array,1,mean)
  arimastlarray[i,,1]=fore$mean
  arimastlarray[i,,2]=season
  arimastlarray[i,,3]=res
  finfore=apply(arimastlarray[i,,],1,sum)
  arimastlA2020[,i]=finfore
}

#Method 2: The trend term uses the base model forecast, 
#the seasonal term and the error term uses the values of the corresponding month in the past year.
arimastlB2020=matrix(0,24,20)
arimastlarray1=array(0,c(20,24,3))
for(i in 1:20)
{
  l=dim(stllist[[i]])[1]
  model=auto.arima(stllist[[i]][,2])
  fore=forecast(model,h=24)
  array=matrix(stllist[[i]][(l-11):l,3],c(12,1))
  season=apply(array,1,mean)
  array=matrix(stllist[[i]][(l-11):l,4],c(12,1))
  res=apply(array,1,mean)
  arimastlarray1[i,,1]=fore$mean
  arimastlarray1[i,,2]=season
  arimastlarray1[i,,3]=res
  finfore=apply(arimastlarray1[i,,],1,sum)
  arimastlB2020[,i]=finfore
}



#Method 3: All three sequences are predicted using the underlying model.
arimastlC2020=matrix(0,24,20)
arimastlarray2=array(0,c(20,24,3))
for(i in 1:20)
{
  l=dim(stllist[[i]])[1]
  model=auto.arima(stllist[[i]][,2])
  fore=forecast(model,h=24)
  model1=auto.arima(stllist[[i]][,3])
  fore1=forecast(model1,h=24)
  model2=auto.arima(stllist[[i]][,4])
  fore2=forecast(model2,h=24)
  arimastlarray2[i,,1]=fore$mean
  arimastlarray2[i,,2]=fore1$mean
  arimastlarray2[i,,3]=fore2$mean
  finfore=apply(arimastlarray2[i,,],1,sum)
  arimastlC2020[,i]=finfore
}

#Method 1: The trend term uses the base model forecast, the seasonal term 
#and the error term uses the average of the corresponding months over the past 3 years.
etsstlA2020=matrix(0,24,20)
etsstlarray=array(0,c(20,24,3))
for(i in 1:20)
{
  l=dim(stllist[[i]])[1]
  model=ets(stllist[[i]][,2])
  fore=forecast(model,h=24)
  array=matrix(stllist[[i]][(l-35):l,3],c(12,3))
  season=apply(array,1,mean)
  array=matrix(stllist[[i]][(l-35):l,4],c(12,3))
  res=apply(array,1,mean)
  etsstlarray[i,,1]=fore$mean
  etsstlarray[i,,2]=season
  etsstlarray[i,,3]=res
  finfore=apply(etsstlarray[i,,],1,sum)
  etsstlA2020[,i]=finfore
}

#Method 2: The trend term uses the base model forecast, 
#the seasonal term and the error term uses the values of the corresponding month in the past year.
etsstlB2020=matrix(0,24,20)
etsstlarray1=array(0,c(20,24,3))
for(i in 1:20)
{
  l=dim(stllist[[i]])[1]
  model=ets(stllist[[i]][,2])
  fore=forecast(model,h=24)
  array=matrix(stllist[[i]][(l-11):l,3],c(12,1))
  season=apply(array,1,mean)
  array=matrix(stllist[[i]][(l-11):l,4],c(12,1))
  res=apply(array,1,mean)
  etsstlarray1[i,,1]=fore$mean
  etsstlarray1[i,,2]=season
  etsstlarray1[i,,3]=res
  finfore=apply(etsstlarray1[i,,],1,sum)
  etsstlB2020[,i]=finfore
}

#Method 3: All three sequences are predicted using the underlying model.
etsstlC2020=matrix(0,24,20)
etsstlarray2=array(0,c(20,24,3))
for(i in 1:20)
{
  l=dim(stllist[[i]])[1]
  model=ets(stllist[[i]][,2])
  fore=forecast(model,h=24)
  model1=ets(stllist[[i]][,3])
  fore1=forecast(model1,h=24)
  model2=ets(stllist[[i]][,4])
  fore2=forecast(model2,h=24)
  etsstlarray2[i,,1]=fore$mean
  etsstlarray2[i,,2]=fore1$mean
  etsstlarray2[i,,3]=fore2$mean
  finfore=apply(etsstlarray2[i,,],1,sum)
  etsstlC2020[,i]=finfore
}

file_path='D:\\university\\研究生\\tourism_forecasting_competition\\submit\\baseline_forecast\\'
setwd(file_path)
write.xlsx(data.frame(arimastlA2020),'arimastlA2020.xlsx')
write.xlsx(data.frame(arimastlB2020),'arimastlB2020.xlsx')
write.xlsx(data.frame(arimastlC2020),'arimastlC2020.xlsx')
write.xlsx(data.frame(etsstlA2020),'etsstlA2020.xlsx')
write.xlsx(data.frame(etsstlB2020),'etsstlB2020.xlsx')
write.xlsx(data.frame(etsstlC2020),'etsstlC2020.xlsx')
