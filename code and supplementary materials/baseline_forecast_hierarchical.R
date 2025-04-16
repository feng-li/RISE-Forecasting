#import packages
library(openxlsx)
library(forecast)
library(imputeTS)
library(readxl)
library(tseries)
library(hts)

#1.Import original data-------
work_path='D:\\university\\研究生\\tourism_forecasting_competition\\submit\\'  
setwd(work_path)
###the work path must be readjusted before running the code
data=read_excel('data.xlsx')

#2.Fill in missing values (Chile\Maldives\Turkey)-------
#(1)Chile--------
Chile = data["Chile"]
# find the first non-na index
first_non_na1 = min(which(!is.na(Chile)))
Chile = Chile[first_non_na1:nrow(data),]
# impute and see the result 
ggplot_na_distribution(Chile)
Chile_imp = na_kalman(Chile, model = "StructTS", smooth = TRUE)
ggplot_na_imputations(Chile, Chile_imp)
data["Chile"][first_non_na1:nrow(data),] = Chile_imp
#(2)Maldives----
Maldives = data["Maldives"]
# find the first non-na index
first_non_na2 = min(which(!is.na(Maldives)))
Maldives = Maldives[first_non_na2:nrow(data),]
# impute and see the result 
ggplot_na_distribution(Maldives)
Maldives_imp = na_kalman(Maldives, model = "StructTS", smooth = TRUE)
ggplot_na_imputations(Maldives, Maldives_imp)
data["Maldives"][first_non_na2:nrow(data),] = Maldives_imp
#(3)Turkey-------
Turkey = data["Turkey"]
# find the first non-na index
first_non_na3 = min(which(!is.na(Turkey)))
Turkey = Turkey[first_non_na3:nrow(data),]
# impute and see the result 
ggplot_na_distribution(Turkey)
Turkey_imp = na_kalman(Turkey, model = "StructTS", smooth = TRUE)
ggplot_na_imputations(Turkey, Turkey_imp)
data["Turkey"][first_non_na3:nrow(data),] = Turkey_imp

#3.Set the hierarchical data-----
first_non_na_vec = vector()
for(i in 2:ncol(data))
{
  first_non_na_vec[i-1] <- min(which(!is.na(data[,i])))
}
#Select part of the orginal dataset(with data of 20 countries all complete)
#2013.1-2019.12
dt = data[max(first_non_na_vec):nrow(data),]#ensure all 20 series are complete with no na-values
row.names(dt) = 1:nrow(dt)
dt=dt[1:84,-1]
#Reset the column name in the hierarchical version
names(dt) = c("AMEcanada","AMEchilee","AMEmexico","NOAtaipei","NOAhongko",
              "NOAjapann","NOAkoreaa","NOAmacaoo","SOAmaldiv","SEAcambod",
              "SEAindone","SEAsingap","PACnewzea","AMEameric","NOAthaila",
              "WEAturkey","PACaustra","PAChawaii","EURaustri","EURczechh")

#turn into time series version
dt_ts2021 <- ts(dt, frequency = 12, start = c(2013, 1),end = c(2019,12))#Dataset used to forecast 2020 and 2021
h_ts2021 <- hts(dt_ts2021, characters = c(3, 6))
dt_ts1819 <- ts(dt, frequency = 12, start = c(2013, 1),end = c(2017,12))#Dataset used to forecast 2018 and 2019
h_ts1819 <- hts(dt_ts1819, characters = c(3, 6))

#4.Forecast---------
#(1)Top Down method(TD)-----
#A.breakdown by historical proportions
#Aa.Base model:ETS
TDhistory_ets2021=forecast(h_ts2021,h=24, method="tdgsa", fmethod="ets") 
TDhistory_ets1819=forecast(h_ts1819,h=24, method="tdgsa", fmethod="ets") 
#save results
TDhe2021=TDhistory_ets2021$bts
TDhe2021=TDhe2021[,c(2:4,11,7:10,19,16:18,15,1,12,20,13,14,5,6)]
#TDhe2021=as.data.frame(TDhe2021)
#names(TDhe_2021)=c("Canada","Chile","Mexico","ChineseTaipei","HongKongSAR","Japan",
#                   "Korea","Macao.China","Maldives","Cambodia","Indonesia","Singapore",
#                   "NewZealand","USA","Thailand","Turkey","Australia","Hawaii",
#                  "Austria","Czech")
TDhe_2021=array(t(TDhe2021),c(20,24,1))
save(TDhe_2021,file="TD_history_ets_2020.RData")


TDhe1819=TDhistory_ets1819$bts
TDhe1819=TDhe1819[,c(2:4,11,7:10,19,16:18,15,1,12,20,13,14,5,6)]
TDhe_1819=array(t(TDhe1819),c(20,24,1))
save(TDhe_1819,file="TD_history_ets_1819.RData")

#Ab.Base model:ARIMA
TDhistory_arima2021=forecast(h_ts2021, h=24,method="tdgsa", fmethod="arima")
TDhistory_arima1819=forecast(h_ts1819, h=24,method="tdgsa", fmethod="arima")
#save results
TDha2021=TDhistory_arima2021$bts
TDha2021=TDha2021[,c(2:4,11,7:10,19,16:18,15,1,12,20,13,14,5,6)]
TDha_2021=array(t(TDha2021),c(20,24,1))
save(TDha_2021,file="TD_history_arima_2021.RData")

TDha1819=TDhistory_arima1819$bts
TDha1819=TDha1819[,c(2:4,11,7:10,19,16:18,15,1,12,20,13,14,5,6)]
TDha_1819=array(t(TDha1819),c(20,24,1))
save(TDha_1819,file="TD_history_arima_1819.RData")


#B.breakdown by proportions of predicted values
#Ba.base model:ETS
TDfore_ets2021=forecast(h_ts2021, h=24,method="tdfp", fmethod="ets") 
TDfore_ets1819=forecast(h_ts1819, h=24,method="tdfp", fmethod="ets") 
#save results
TDfe2021=TDfore_ets2021$bts
TDfe2021=TDfe2021[,c(2:4,11,7:10,19,16:18,15,1,12,20,13,14,5,6)]
TDfe_2021=array(t(TDfe2021),c(20,24,1))
save(TDfe_2021,file="TD_fore_ets_2021.RData")

TDfe1819=TDfore_ets1819$bts
TDfe1819=TDfe1819[,c(2:4,11,7:10,19,16:18,15,1,12,20,13,14,5,6)]
TDfe_1819=array(t(TDfe1819),c(20,24,1))
save(TDfe_1819,file="TD_fore_ets_1819.RData")

#Bb.base model:ARIMA
TDfore_arima2021=forecast(h_ts2021, h=24,method="tdfp", fmethod="arima")
TDfore_arima1819=forecast(h_ts1819, h=24,method="tdfp", fmethod="arima")
#save results
TDfa2021=TDfore_arima2021$bts
TDfa2021=TDfa2021[,c(2:4,11,7:10,19,16:18,15,1,12,20,13,14,5,6)]
TDfa_2021=array(t(TDfa2021),c(20,24,1))
save(TDfa_2021,file="TD_fore_arima_2021.RData")

TDfa1819=TDfore_arima1819$bts
TDfa1819=TDfa1819[,c(2:4,11,7:10,19,16:18,15,1,12,20,13,14,5,6)]
TDfa_1819=array(t(TDfa1819),c(20,24,1))
save(TDfa_1819,file="TD_fore_arima_1819.RData")

#(2)Optimal forecast reconciliation for hierarchical time series----
#A.mint
mint2021=forecast(h_ts2021, h=24,method = "comb" ,weights = "mint")
mint1819=forecast(h_ts1819, h=24,method = "comb" ,weights = "mint")
#save results
Mint2021=mint2021$bts
Mint2021=Mint2021[,c(2:4,11,7:10,19,16:18,15,1,12,20,13,14,5,6)]
mint_2021=array(t(Mint2021),c(20,24,1))
save(mint_2021,file="mint_2021.RData")

Mint1819=mint1819$bts
Mint1819=Mint1819[,c(2:4,11,7:10,19,16:18,15,1,12,20,13,14,5,6)]
mint_1819=array(t(Mint1819),c(20,24,1))
save(mint_1819,file="mint_1819.RData")

#B.wls
wls2021=forecast(h_ts2021,h=24,method = "comb" ,weights = "wls")
wls1819=forecast(h_ts1819,h=24,method = "comb" ,weights = "wls")
#save results
Wls2021=wls2021$bts
Wls2021=Wls2021[,c(2:4,11,7:10,19,16:18,15,1,12,20,13,14,5,6)]
wls_2021=array(t(Wls2021),c(20,24,1))
save(wls_2021,file="wls_2021.RData")

Wls1819=wls1819$bts
Wls1819=Wls1819[,c(2:4,11,7:10,19,16:18,15,1,12,20,13,14,5,6)]
wls_1819=array(t(Wls1819),c(20,24,1))
write.xlsx(wls_1819,file="wls_1819.xlsx")
save(wls_1819,file="wls_1819.RData")

file_path='D:\\university\\研究生\\tourism_forecasting_competition\\submit\\stacking_train\\'
setwd(file_path)
write.xlsx(Mint1819,'mint2018.xlsx')
write.xlsx(Wls1819,'wls2018.xlsx')
write.xlsx(TDfa1819,'TDfa2018.xlsx')
write.xlsx(TDfe1819,'TDfe2018.xlsx')
write.xlsx(TDha1819,'TDha2018.xlsx')
write.xlsx(TDhe1819,'TDhe2018.xlsx')

file_path='D:\\university\\研究生\\tourism_forecasting_competition\\submit\\baseline_forecast\\'
setwd(file_path)
write.xlsx(Mint2021,'mint2020.xlsx')
write.xlsx(Wls2021,'wls2020.xlsx')
write.xlsx(TDfa2021,'TDfa2020.xlsx')
write.xlsx(TDfe2021,'TDfe2020.xlsx')
write.xlsx(TDha2021,'TDha2020.xlsx')
write.xlsx(TDhe2021,'TDhe2020.xlsx')

