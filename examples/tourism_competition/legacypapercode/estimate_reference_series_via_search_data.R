library(Metrics)
library(forecast)
require('dejavu')
library(dtw)
library(robustbase)
library(readxl)
library(openxlsx)
library(prophet)
library(dplyr)
library(TSA)


country_name=c("加拿大",'智利','墨西哥','台湾','香港','日本','韩国','澳门','马尔代夫','柬埔寨','印尼','新加坡','新西兰','美国','泰国','土耳其','澳大利亚','夏威夷','奥地利','捷克')


work_path='D:\\university\\研究生\\tourism_forecasting_competition\\submit\\composite_search_index\\' 
file_path='D:\\university\\研究生\\tourism_forecasting_competition\\submit\\reference_series\\' 
setwd(file_path)

###holt_winters
holtwinters_result=data.frame(matrix(nrow=6,ncol=20))
colnames(holtwinters_result)=country_name
for (i in 1:20){
  file_name=paste(work_path,country_name[i],'.xlsx',sep='')
  data=read_excel(file_name)
  data=data[,1:ncol(data)]
  df=data[complete.cases(data),]
  data$ratio=data$tourism_arrival/data$composite_search_index
  temp=data[111:nrow(data),]
  temp=temp[complete.cases(temp),]
  diff_time=data$date[nrow(data)]-temp$date[nrow(temp)]
  h=round(as.numeric(diff_time/30))
  train=ts(data=temp$ratio,frequency = 12)
  fit=HoltWinters(train)
  fc=forecast(fit,level=80)
  pre=nrow(data)-h+1
  nex=149-h
  if (nex==143){
    holtwinters_result[[country_name[i]]]=fc$mean[1:h]*data$composite_search_index[pre:nrow(data)]}
  else{
    holtwinters_result[[country_name[i]]]=append(c(fc$mean[1:h]*data$composite_search_index[pre:nrow(data)]),data$tourism_arrival[144:nex],after=0)}
}
write.xlsx(holtwinters_result,'holtwinters_result.xlsx')


###ets
ets_result=data.frame(matrix(nrow=6,ncol=20))
colnames(ets_result)=country_name
for (i in 1:20){
  file_name=paste(work_path,country_name[i],'.xlsx',sep='')
  data=read_excel(file_name)
  data=data[,1:ncol(data)]
  df=data[complete.cases(data),]
  data$ratio=data$tourism_arrival/data$composite_search_index
  temp=data[110:nrow(data),]
  temp=temp[complete.cases(temp),]
  diff_time=data$date[nrow(data)]-temp$date[nrow(temp)]
  h=round(as.numeric(diff_time/30))
  train=ts(data=temp$ratio,frequency = 12)
  fit=ets(train)
  fc=forecast(fit,level=80)
  pre=nrow(data)-h+1
  nex=149-h
  if (nex==143){
    ets_result[[country_name[i]]]=fc$mean[1:h]*data$composite_search_index[pre:nrow(data)]}
  else{
    ets_result[[country_name[i]]]=append(c(fc$mean[1:h]*data$composite_search_index[pre:nrow(data)]),data$tourism_arrival[144:nex],after=0)}
}
write.xlsx(ets_result,'ets_result.xlsx')


##tbats
tbats_result=data.frame(matrix(nrow=6,ncol=20))
colnames(tbats_result)=country_name
for (i in 1:20){
  file_name=paste(work_path,country_name[i],'.xlsx',sep='')
  data=read_excel(file_name)
  data=data[,1:ncol(data)]
  df=data[complete.cases(data),]
  data$ratio=data$tourism_arrival/data$composite_search_index
  temp=data[110:nrow(data),]
  temp=temp[complete.cases(temp),]
  diff_time=data$date[nrow(data)]-temp$date[nrow(temp)]
  h=round(as.numeric(diff_time/30))
  train=ts(data=temp$ratio,frequency = 12)
  fit=tbats(train)
  fc=forecast(fit,level=80)
  pre=nrow(data)-h+1
  nex=149-h
  if (nex==143){
    tbats_result[[country_name[i]]]=fc$mean[1:h]*data$composite_search_index[pre:nrow(data)]}
  else{
    tbats_result[[country_name[i]]]=append(c(fc$mean[1:h]*data$composite_search_index[pre:nrow(data)]),data$tourism_arrival[144:nex],after=0)}
}
write.xlsx(tbats_result,'tbats_result.xlsx')


###facebook prophet(all data is contained as training set)

prophet_full_result=data.frame(matrix(nrow=6,ncol=20))
colnames(prophet_full_result)=country_name

for (i in 1:20){
  file_name=file_name=paste(work_path,country_name[i],'.xlsx',sep='')
  data=read_excel(file_name)
  data=data[,2:ncol(data)]
  df=data[complete.cases(data),]
  diff_time=data$date[nrow(data)]-df$date[nrow(df)]
  h=round(as.numeric(diff_time/30))
  pre=nrow(data)-h+1
  nex=149-h
  df_prophet=df[c('date','tourism_arrival','composite_search_index')]
  df_prophet=df_prophet |> rename('ds'='date','y'='tourism_arrival','index'='composite_search_index')
  max_value=max(df_prophet$y)
  min_value=min(df_prophet$y)
  df_prophet$cap=max_value
  df_prophet$floor=min_value
  model_prophet=prophet(growth='logistic',n.changepoints = 5)
  model_prophet=add_regressor(model_prophet,'index')
  model_prophet=fit.prophet(model_prophet,df_prophet)
  future=data.frame(ds=data$date[pre:149],index=data$composite_search_index[pre:149])
  future$cap=max_value
  future$floor=min_value
  forecast_prophet=predict(model_prophet,future)
  if (nex==143){
    prophet_full_result[[country_name[i]]]=c(forecast_prophet$yhat)}
  else{
    prophet_full_result[[country_name[i]]]=append(c(forecast_prophet$yhat),data$tourism_arrival[144:nex],after=0)}
}
write.xlsx(prophet_full_result,'prophet_full_result.xlsx')


###facebook prophet(only data in the period of covid-19 is contained as training set)
prophet_part_result=data.frame(matrix(nrow=6,ncol=20))
colnames(prophet_part_result)=country_name

for (i in 1:20){
  file_name=file_name=paste(work_path,country_name[i],'.xlsx',sep='')
  data=read_excel(file_name)
  data=data[,2:ncol(data)]
  df=data[complete.cases(data),]
  df=df[111:nrow(df),]
  diff_time=data$date[nrow(data)]-df$date[nrow(df)]
  h=round(as.numeric(diff_time/30))
  pre=nrow(data)-h+1
  nex=149-h
  df_prophet=df[c('date','tourism_arrival','composite_search_index')]
  df_prophet=df_prophet |> rename('ds'='date','y'='tourism_arrival','index'='composite_search_index')
  max_value=max(df_prophet$y)
  min_value=min(df_prophet$y)
  df_prophet$cap=max_value
  df_prophet$floor=min_value
  model_prophet=prophet(growth='logistic',n.changepoints = 5)
  model_prophet=add_regressor(model_prophet,'index')
  model_prophet=fit.prophet(model_prophet,df_prophet)
  future=data.frame(ds=data$date[pre:149],index=data$composite_search_index[pre:149])
  future$cap=max_value
  future$floor=min_value
  forecast_prophet=predict(model_prophet,future)
  if (nex==143){
    prophet_part_result[[country_name[i]]]=c(forecast_prophet$yhat)}
  else{
    prophet_part_result[[country_name[i]]]=append(c(forecast_prophet$yhat),data$tourism_arrival[144:nex],after=0)}
}
write.xlsx(prophet_part_result,'prophet_part_result.xlsx')


###ARIMAX(all data is contained as training set)

arimax_full_result=data.frame(matrix(nrow=6,ncol=20))
colnames(arimax_full_result)=country_name

for (i in 1:20){
  file_name=paste('D:\\university\\研究生\\tourism_forecasting_competition\\baidu_index\\',country_name[i],'_月度.xlsx',sep='')
  data=read_excel(file_name)
  data=data[,2:ncol(data)]
  df=data[complete.cases(data),]
 # df=df[111:nrow(df),]
  diff_time=data$date[nrow(data)]-df$date[nrow(df)]
  h=round(as.numeric(diff_time/30))
  pre=nrow(data)-h+1
  nex=149-h
  model_arimax=arimax(df$旅游人数,xreg=df$综合指数)
  forecast_arimax=predict(model_arimax,newxreg=data$综合指数[pre:nrow(data)],n.ahead=h)
  if (nex==143){
    arimax_full_result[[country_name[i]]]=c(forecast_arimax$pred)}
  else{
    arimax_full_result[[country_name[i]]]=append(c(forecast_arimax$pred),data$旅游人数[144:nex],after=0)}
}
write.xlsx(arimax_full_result,'arimax_full_result.xlsx')


###ARIMAX(only data in the period of covid-19 is contained as training set)

arimax_part_result=data.frame(matrix(nrow=6,ncol=20))
colnames(arimax_part_result)=country_name

for (i in 1:20){
  file_name=paste('D:\\university\\研究生\\tourism_forecasting_competition\\baidu_index\\',country_name[i],'_月度.xlsx',sep='')
  data=read_excel(file_name)
  data=data[,2:ncol(data)]
  df=data[complete.cases(data),]
  df=df[111:nrow(df),]
  diff_time=data$date[nrow(data)]-df$date[nrow(df)]
  h=round(as.numeric(diff_time/30))
  pre=nrow(data)-h+1
  nex=149-h
  model_arimax=arimax(df$旅游人数,xreg=df$综合指数)
  forecast_arimax=predict(model_arimax,newxreg=data$综合指数[pre:nrow(data)],n.ahead=h)
  if (nex==143){
    arimax_part_result[[country_name[i]]]=c(forecast_arimax$pred)}
  else{
    arimax_part_result[[country_name[i]]]=append(c(forecast_arimax$pred),data$旅游人数[144:nex],after=0)}
}
write.xlsx(arimax_part_result,'arimax_part_result.xlsx')



