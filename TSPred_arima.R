library(TSPred)

ordered_train <- csv_dolar[1:round(0.8*nrow(csv_dolar)),];
ordered_test <- csv_dolar[round(0.8*nrow(csv_dolar)):length(csv_dolar$Close),];
# Compute the daily returns
#dolar.close = csv_dolar$Close

# Use only the last two years of returns
#dolar.tail = as.ts( tail( dolar.close, 500 ) )




# currentIndex is the index of the day we are making a forcast for
# xx is the return series
# history is look-back period to consider at each point

xx <- csv_dolar$Close;
history <- 500;
currentIndex<-length(ordered_train$Close);
lags<-1;
len = NROW( xx );
forecasts_arima = ordered_test$Close;
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
  
  
  if( nextIndex > len ) break
  
  currentIndex = nextIndex

}


MSE.arima <- sum((ordered_test$Close - forecasts_arima)^2)/nrow(ordered_test)
RMSE.arima <- sqrt(MSE.arima)
