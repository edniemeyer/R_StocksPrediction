#lendo csv
#csv_dolar<-read.csv('normalizados-Dolar.csv')
csv_ibov<-read.csv('normalizado-Ibov.csv')
csv_dolar<-read.csv2('DOLAR.csv')

csv_dolar <- subset(csv_dolar, select = -Date )

csv_dolar_normalizado <- as.data.frame(lapply(csv_dolar,minMaxNormalize))

csv_dolar_normalizado$seq<-1:nrow(csv_dolar_normalizado)
csv_dolar$seq<-1:nrow(csv_dolar)

#ordered_train <- csv_dolar[1:round(0.8*nrow(csv_dolar)),];
#ordered_test <- csv_dolar[round(0.8*nrow(csv_dolar)):length(csv_dolar$Close),];
ordered_train <- csv_dolar[1:1003,];
ordered_test_2003 <- csv_dolar[1004:1256,];
total <- rbind(ordered_train,ordered_test_2003);
ordered_train_normalizado <- csv_dolar_normalizado[1:1003,];
ordered_test_2003_normalizado <- csv_dolar_normalizado[1004:1256,];
#ordered_train_puro <- csv_dolar_puro[1:round(0.8*nrow(csv_dolar)),];
#ordered_test_puro <- csv_dolar_puro[round(0.8*nrow(csv_dolar)):length(csv_dolar$Close),];

#separando treino e teste
#index <- sample(1:nrow(csv_dolar),round(0.8*nrow(csv_dolar)))
#train <- csv_dolar[index,]
#test <- csv_dolar[-index,]
train <-ordered_train_normalizado;
test <- ordered_test_2003_normalizado;
total_normalizado <- rbind(train,test)

#Verificando outliers
boxplot(total_normalizado$Close,outline=TRUE)





#fitting linear regression
lm.fit <- glm(Close~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)

den_forecasts_lm <- mapply(minMaxDenormalize,pr.lm,min(total$Close),max(total$Close));

whitenoise.lm <- (den_forecasts_lm - ordered_test_2003$Close);
accuracy.lm <- sum(whitenoise.lm^2)/nrow(ordered_test_2003); #MSE
#RMSE.lm <- sqrt(MSE.lm)

precision.lm <-  qt(0.975,df=length(whitenoise.lm)-1)*sd(whitenoise.lm)/sqrt(length(whitenoise.lm)); #confidence interval

error.lm <- precision.lm + accuracy.lm;




#neuralnet
library('timeSeries')

csv_dolar_ts<-timeSeries(csv_dolar_normalizado$Close)
csv_dolar_normalizado$Close1 <- lag(csv_dolar_ts, k=1)
csv_dolar_normalizado$Close2 <- lag(csv_dolar_ts, k=2)
csv_dolar_normalizado$Close3 <- lag(csv_dolar_ts, k=3)
csv_dolar_normalizado$Close4 <- lag(csv_dolar_ts, k=4)
csv_dolar_normalizado$Close5 <- lag(csv_dolar_ts, k=5)
csv_dolar_normalizado$Close6 <- lag(csv_dolar_ts, k=6)

csv_dolar_normalizado <- subset(csv_dolar_normalizado, select = -seq )
csv_dolar_normalizado <- na.omit(csv_dolar_normalizado)

ordered_train.nn <- csv_dolar_normalizado[1:1003,];
ordered_test.nn <- csv_dolar_normalizado[1004:1256,];

train.nn <-ordered_train.nn;
test.nn <- ordered_test.nn;

library(neuralnet)
n <- names(train.nn)
f <- as.formula(paste("Close ~", paste(n[!n %in% "Close"], collapse = " + ")))
nn <- neuralnet(f,data=train.nn,hidden=c(4,4),linear.output=T)

pr.nn <- compute(nn,test.nn[,2:7])

den_forecasts_nn <- mapply(minMaxDenormalize,pr.nn$net.result,dolarminvec,dolarmaxvec)

whitenoise.nn <- (den_forecasts_nn - ordered_test_2003$Close)
accuracy.nn <- sum(whitenoise.nn^2)/nrow(ordered_test_2003) #MSE
#RMSE.nn <- sqrt(MSE.nn)

precision.nn <-  qt(0.975,df=length(whitenoise.nn)-1)*sd(whitenoise.nn)/sqrt(length(whitenoise.nn)); #confidence interval

error.nn <- precision.nn + accuracy.nn;



#cross validation
#linear regression
library(boot)
set.seed(200)
lm.fit <- glm(Close~.,data=csv_dolar)
cv.mse.lm <- cv.glm(csv_dolar,lm.fit,K=10)$delta[1]
cv.rmse.lm <-sqrt(cv.mse.lm)


#neural net
set.seed(450)
cv.mse.nn <- NULL
cv.rmse.nn <- NULL
k <- 10

library(plyr) 
pbar <- create_progress_bar('text')
pbar$init(k)

data <- csv_dolar
for(i in 1:k){
  index <- sample(1:nrow(data),round(0.8*nrow(data)))
  train.cv <- data[index,]
  test.cv <- data[-index,]
  
  nn <- neuralnet(f,data=train.cv,hidden=c(3),linear.output=T)
  
  pr.nn <- compute(nn,test.cv[,2:4])
  pr.nn <- pr.nn$net.result*(max(data$Close)-min(data$Close))+min(data$Close)
  
  test.cv.r <- (test.cv$Close)*(max(data$Close)-min(data$Close))+min(data$Close)
  
  cv.mse.nn[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)
  cv.rmse.nn[i] <- sqrt(cv.mse.nn[i])
  
  pbar$step()
}


boxplot(cv.rmse.nn,xlab='RMSE CV',col='cyan',
        border='blue',names='CV error (RMSE)',
        main='CV error (RMSE) for NN',horizontal=TRUE)
