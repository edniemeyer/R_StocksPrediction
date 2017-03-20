library(TSPred)
library(zoo)

# Compute the daily returns
#dolar.close = csv_dolar$Close

# Use only the last two years of returns
#dolar.tail = as.ts( tail( dolar.close, 500 ) )




# currentIndex is the index of the day we are making a forcast for
# xx is the return series
# history is look-back period to consider at each point

xx <- total_normalizado$Close;
history <- 1003;
currentIndex<-length(train$Close);
lags<-1;
len = NROW( xx );
forecasts_arima = test$Close;
repeat
{
  nextIndex = currentIndex + 1
  
  # lags is how many days behind is the data, the default is 1,
  # meaning use data up to yesterdays close
  forecastLength = nextIndex - currentIndex + lags - 1
  
  # Get the series
  yy = xx[index(xx)[(currentIndex-history-lags+1):(currentIndex-lags)]]
  
  
  # Save the forecast
  forecasts_arima[currentIndex-length(train$Close)+1] = arimapred(yy,n.ahead=forecastLength)[1]
  
  
  if( nextIndex > len -1) break
  
  currentIndex = nextIndex

}

den_forecasts_arima <- mapply(minMaxDenormalize,forecasts_arima,min(total$Close),max(total$Close))
#den_forecasts_arima <- den_forecasts_arima[-c(length(den_forecasts_arima))] # removendo ultimo registro

#MSE.arima <- sum((ordered_test_puro$Close - den_forecasts_arima)^2)/nrow(ordered_test_puro)
#RMSE.arima <- sqrt(MSE.arima)

whitenoise.arima <- (den_forecasts_arima - ordered_test_2003$Close);
accuracy.arima <- sum(whitenoise.arima^2)/nrow(ordered_test_2003); #MSE
#RMSE.lm <- sqrt(MSE.lm)

precision.arima <-  qt(0.975,df=length(whitenoise.arima)-1)*sd(whitenoise.arima)/sqrt(length(whitenoise.arima)); #confidence interval

error.arima <- precision.arima + accuracy.arima;

