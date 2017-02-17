library(quantmod)
library(fArma)

# Compute the daily returns
#dolar.close = csv_dolar$Close

# Use only the last two years of returns
#dolar.tail = as.ts( tail( dolar.close, 500 ) )

arSearch = function(
  xx,  
  minOrder=c(0,0),
  maxOrder=c(5,0),
  trace=FALSE )
{
  bestAic = 1e9
  len = NROW( xx ) 
  for( p in minOrder[1]:maxOrder[1] ) for( q in minOrder[2]:maxOrder[2] )
  {
    if( p == 0 && q == 0 )
    {    
      next
    }    
    
    formula = as.formula( paste( sep="", "xx ~ arma(", p, ",", q, ")" ) )
    
    fit = tryCatch( armaFit( formula, data=xx ),
                    error=function( err ) FALSE,
                    warning=function( warn ) FALSE )
    if( !is.logical( fit ) )
    {    
      fitAic = fit@fit$aic
      if( fitAic < bestAic )
      {    
        bestAic = fitAic
        bestFit = fit
        bestModel = c( p, q )
      }    
      
      if( trace )
      {    
        ss = paste( sep="", "(", p, ",", q, "): AIC = ", fitAic )
        print( ss ) 
      }    
    }    
    else
    {    
      if( trace )
      {    
        ss = paste( sep="", "(", p, ",", q, "): None" )
        print( ss ) 
      }    
    }    
  }
  
  if( bestAic < 1e9 )
  {
    return( list( aic=bestAic, fit=bestFit, model=bestModel ) )
  }
  
  return( FALSE )
}





# currentIndex is the index of the day we are making a forcast for
# xx is the return series
# history is look-back period to consider at each point

xx <- csv_dolar$Close;
history <- 500;
currentIndex<-length(ordered_train$Close);
lags<-1;
len = NROW( xx );
forecasts_ar = ordered_test$Close;
repeat
{
  nextIndex = currentIndex + 1
  
  # lags is how many days behind is the data, the default is 1,
  # meaning use data up to yesterdays close
  forecastLength = nextIndex - currentIndex + lags - 1
  
  # Get the series
  yy = xx[index(xx)[(currentIndex-history-lags+1):(currentIndex-lags)]]
  
  # Find the best fit
  bestFit = arSearch( yy)  
  
  if( !is.null( bestFit ) )
  {
    # Forecast
    fore = tryCatch( predict( bestFit$fit, n.ahead=forecastLength, doplot=FALSE ),
                     error=function( err ) FALSE,
                     warning=function( warn ) FALSE )
    if( !is.logical( fore ) )
    {    
      # Save the forecast
      forecasts_ar[currentIndex-length(train$Close)+1] = tail( fore$pred, 1 )
      
      # Save the model order
      #ars[currentIndex] = bestFit$model[1]
      #mas[currentIndex] = bestFit$model[2]
      
      #forecasts[currentIndex-length(train$Close)+1] = 0
    }
    
    if( nextIndex > len ) break
    currentIndex = nextIndex
  }
}

den_forecasts_ar <- mapply(minMaxDenormalize,forecasts_ar,dolarminvec,dolarmaxvec)

MSE.ar <- sum((ordered_test_puro$Close - den_forecasts_ar)^2)/nrow(ordered_test_puro)
RMSE.ar <- sqrt(MSE.ar)


