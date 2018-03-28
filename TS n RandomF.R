demographics<-read.csv('demographics.csv')
event<-read.csv('event_calendar.csv')
historical<-read.csv('historical_volume.csv')
soda<-read.csv('industry_soda_sales.csv')
volume<-read.csv('industry_volume.csv')
price<-read.csv('price_sales_promotion.csv')
weather<-read.csv('weather.csv')


######################################################################
#        Approach 1:Time Series (ETS) (Score- 0.77)                  #
######################################################################
k<-1
df3<-data.frame(Agency='',SKU='',count=0,index=1:1450)
df3$Agency<-as.character(df3$Agency)
df3$SKU<-as.character(df3$SKU)
for (i in 1:length(agencies)){
  for (j in 1:length(sku)){
    df3$Agency[k]<-as.character(agencies[i])
    df3$SKU[k]<-as.character(sku[j])
    df3$count[k]<-length(historical$Volume[historical$Agency==agencies[i] & historical$SKU==sku[j]]>0)
    k<-k+1
  }
}
k<-1
for (i in 1:350){
  hist<-historical[historical$Agency==df4$Agency[i] & historical$SKU==df4$SKU[i],]
  hist<-hist[order(hist$Year,hist$Month),]
  t<-ts(hist$Volume,start = c(0,13),frequency = 12)
  if(any(t==0)){
    model<-auto.arima(t)
  }
  else{
    model<-ets(t,model = 'MAM')
  }
    df4$Volume[i]<-as.numeric(forecast(model,1)$mean)
    plot(forecast(model,12))
    k<-k+1
}

#Writing File
vforecast<-read.csv('volume_forecast.csv')
test2$Volume[test2$Volume<1]<-1
for (i in 1:350){
  vforecast$Volume[vforecast$Agency==test2$Agency[i] & vforecast$SKU==test2$SKU[i]]<-test2$Volume[i]
}

write.csv(vforecast,'volume_forecast.csv',row.names = FALSE)

######################################################################
#       Approach 2:Regression Random Forest(score-0.865)             #
######################################################################
#Merging Files
volume<-merge(weather,volume,by='YearMonth')
volume<-merge(volume,soda,by='YearMonth')
volume<-merge(volume,demographics,by='Agency')
volume2<-merge(volume2,price,by=c('YearMonth','Agency'))
volume2<-merge(volume2,event2,by='YearMonth')
volume2<-merge(volume2,historical,by=c('YearMonth','Agency',"SKU"))
volume2$month<-as.numeric(substr(volume2$YearMonth,5,6))
volume2$year<-as.numeric(substr(volume2$YearMonth,1,4))

#Preparing Test Set
test<-read.csv('volume_forecast.csv')
test<-test[test$Volume>0,]
test$month<-01
test$year<-2018
test$YearMonth<-201801
test<-merge(test,event,by='YearMonth')

#Soda Estimation for 201801 with no trend
t<-ts(soda$Soda_Volume,start = c(0,13),frequency=12)
model<-ets(t,model ='MNM' )
test$Soda_Volume<-forecast(model,1)$mean
#Soda Estimation for 201801 with M trend
t<-ts(soda$Soda_Volume,start = c(0,13),frequency=12)
model<-ets(t,model ='MMM' )
test2$Soda_Volume<-forecast(model,1)$mean

#Industry Volume Estimation for 201801
t<-ts(volume$Industry_Volume,start = c(0,13),frequency = 12)
model<-ets(t,model ='MMM' )
test2$Industry_Volume<-forecast(model,1)$mean

#Estimation of Price,Sales and Promotions
test2$Price<-0
test2$Sales<-0
test2$Promotions<-0
for(i in 1:350){
  hist<-volume2[volume2$Agency==test2$Agency[i] & volume2$SKU==test2$SKU[i],]
  hist<-hist[order(hist$year,hist$month),]
  tPrice<-ts(hist$Price,start = c(0,13),frequency = 12)
  tSales<-ts(hist$Sales,start = c(0,13),frequency = 12)
  tPromotions<-ts(hist$Promotions,start = c(0,13),frequency = 12)
  model<-auto.arima(tPrice)
  test2$Price[i]<-forecast(model,1)$mean
  model<-auto.arima(tSales)
  test2$Sales[i]<-forecast(model,1)$mean
  model<-auto.arima(tPromotions)
  test2$Promotions[i]<-forecast(model,1)$mean
}

for(a in 1:8){
  test2$Price[ind[a]]<-IQR(volume2$Price[volume2$Agency==test2$Agency[ind[a]] & volume2$SKU==test2$SKU[ind[a]]])
}

test2<-merge(test2,demographics,by='Agency')
weather2<-split(weather,weather$Agency)
for(i in 1:length(weather2)){
  hist<-weather2[[i]]
  hist<-hist[order(hist$year,hist$month),]
  t<-ts(hist$Avg_Max_Temp,start=c(0,13),frequency = 12)
  model<-ets(t,model = 'MNA')
  test2$Avg_Max_Temp[test2$Agency==unique(hist$Agency)]<-forecast(model,1)$mean
}


#Creating Model and Predicting Answer
for (i in 1:350){
model<-randomForest(Volume~Avg_Max_Temp+Industry_Volume+Soda_Volume+Avg_Population_2017+Avg_Yearly_Household_Income_2017+Price+Sales+Promotions+Easter.Day+Good.Friday+New.Year+Christmas+Labor.Day+Independence.Day+Revolution.Day.Memorial+Regional.Games+FIFA.U.17.World.Cup+Football.Gold.Cup+Beer.Capital+Music.Fest+month+active_months+competitors,data=volume2[volume2$Agency==test2$Agency[i] & volume2$SKU==test2$SKU[i],],ntree=2000)
test2$Volume[i]<-predict(model,newdata=test2[i,])
}

######################################################################
#                 Recommendation For Agencies                        #
######################################################################
df5<-as.data.frame(table(df4$SKU))
df5$Ratio<-0
for(i in 1:length(df5$Var1)){
  df5$Ratio[i]<-sum(df4$Volume[df4$SKU==df5$Var1[i]])/df5$Freq[i]
}
test2$Avg_Max_temp<-0
