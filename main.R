#lendo csv
csv_dolar<-read.csv('normalizados-Dolar.csv')
csv_ibov<-read.csv('normalizado-Ibov.csv')

#separando treino e teste
index <- sample(1:nrow(csv_dolar),round(0.8*nrow(csv_dolar)))
train <- csv_dolar[index,]
test <- csv_dolar[-index,]


#fitting linear regression
lm.fit <- glm(Close~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$Close)^2)/nrow(test)
RMSE.lm <- sqrt(MSE.lm)



#neuralnet
library(neuralnet)
n <- names(train)
f <- as.formula(paste("Close ~", paste(n[!n %in% "Close"], collapse = " + ")))
nn <- neuralnet(f,data=train,hidden=c(3),linear.output=T)

pr.nn <- compute(nn,test[,2:4])

MSE.nn <- sum((test$Close - pr.nn$net.result)^2)/nrow(test)
RMSE.nn <- sqrt(MSE.nn)


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
