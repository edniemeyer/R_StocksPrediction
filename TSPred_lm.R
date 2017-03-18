library(TSPred)

#fitting linear regression
fittest <- fittestLM(train,test, maxorder=1)
#fittest model information
fittest$rank[1,]
#predictions of the fittest model
fittest$ranked.results[[1]]$pred

lm.fit <- glm(Close~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)

den_forecasts_lm <- mapply(minMaxDenormalize,pr.lm,min(total$Close),max(total$Close));

whitenoise.lm <- (den_forecasts_lm - ordered_test_2003$Close);
accuracy.lm <- sum(whitenoise.lm^2)/nrow(ordered_test_2003); #MSE
#RMSE.lm <- sqrt(MSE.lm)

precision.lm <-  qt(0.975,df=length(whitenoise.lm)-1)*sd(whitenoise.lm)/sqrt(length(whitenoise.lm)); #confidence interval

error.lm <- precision.lm + accuracy.lm;