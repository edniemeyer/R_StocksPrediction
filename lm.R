library(zoo)


xx <- total_normalizado$Close;
history <- 1003;
currentIndex<-length(train$Close);
lags<-1;
len = NROW( xx );
forecasts_lm = test$Close;
repeat
{
  nextIndex = currentIndex + 1
  
  # lags is how many days behind is the data, the default is 1,
  # meaning use data up to yesterdays close
  forecastLength = nextIndex - currentIndex + lags - 1
  
  # Get the series
  yy = xx[index(xx)[(currentIndex-history-lags+1):(currentIndex-lags)]]
  lm.fit <- glm(Close~., data=yy)
  
  
  # Save the forecast
  forecasts_lm[currentIndex-length(train$Close)+1] = predict(lm.fit,n.ahead=forecastLength)[1]
  
  
  if( nextIndex > len-1 ) break
  
  currentIndex = nextIndex
  
}

den_forecasts_lm <- mapply(minMaxDenormalize,forecasts_lm,min(total$Close),max(total$Close))
#den_forecasts_lm <- den_forecasts_lm[-c(length(den_forecasts_lm))] # removendo ultimo registro
