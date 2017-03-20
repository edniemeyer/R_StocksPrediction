rwalk <- function(p0, days, mu, sigma) {
  
  
  mu.daily <- mu/days
  sigma.daily <- sigma * 1/sqrt(days)
  
  r <- rnorm(days, mu.daily, sigma.daily)  # 100 random normals 
  logPrice <- log(p0) + cumsum(r)         # Returns a vector whose elements are the
  # cumulative sums of the argument's elements
  Prices <- exp(logPrice)
  
  # display
  #png(file="random_walk.png")
  #plot(Prices, type="l")
  #dev.off()
  return(Prices);
}

mu <- diff(train$Close)/train$Close[-length(train$Close)]
mu <- mean(mu[2:length(mu)])

ret <- log(lag(timeSeries(train$Close))) - log(train$Close)
vol <- sd(ret[3:length(ret)]) * sqrt(250)

pr.rw <- rwalk(train$Close[length(train$Close)], length(test$Close), mu,vol)


den_forecasts_rw <- mapply(minMaxDenormalize,pr.rw,min(total$Close),max(total$Close));

whitenoise.rw <- (den_forecasts_rw - ordered_test_2003$Close);
accuracy.rw <- sum(whitenoise.rw^2)/nrow(ordered_test_2003); #MSE
#RMSE.lm <- sqrt(MSE.lm)

precision.rw <-  qt(0.975,df=length(whitenoise.rw)-1)*sd(whitenoise.rw)/sqrt(length(whitenoise.rw)); #confidence interval

error.rw <- precision.rw + accuracy.rw;
