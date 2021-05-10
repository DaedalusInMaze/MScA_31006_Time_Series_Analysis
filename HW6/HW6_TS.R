library(tseries)
library(fpp)

### 1.

load('visitors_monthly.rda')

visitors <- visitors[, -1]

# names(visitors)[names(visitors) == 'X'] <- 'Y'

## plot without transformation
visitors_ts <- ts(visitors$x, start = c(1985, 5), frequency = 12)

plot(visitors_ts)

tsdisplay(visitors_ts,ylab='Visitors',main='Monthly Austrailian visitors')

# We see that transformation is needed


## plot with transformation

### Get lambda
lambda <- BoxCox.lambda(visitors_ts)
lambda

# do transformation based on the best lambda 
visitors_tran <- BoxCox(visitors_ts, lambda = lambda)
tsdisplay(visitors_tran)


#### 2.

## Model 1
model_1 <- auto.arima(visitors_ts, lambda = 'auto', seasonal = TRUE, trace = TRUE)
summary(model_1)

## Model 2
model_2 <- ets(visitors_tran, model = 'MAM', lambda = 'auto')
summary(model_2)




### 3.
k <- 160 # minimum data length for fitting a model
n <- length(visitors_ts) # Number of data points

p <- 12 ### Period
H <- 12 # Forecast Horiz



# Predict with Arima
defaultW <- getOption("warn") 
options(warn = -1)

st <- tsp(visitors_ts)[1]+(k-2)/p #  gives the start time in time units,

mae_1 <- matrix(NA,n-k,H)
mae_2 <- matrix(NA,n-k,H)
mae_3 <- matrix(NA,n-k,H)
mae_4 <- matrix(NA,n-k,H)

AICc_1 <- matrix(NA,n-k,H)
AICc_2 <- matrix(NA,n-k,H)
AICc_3 <- matrix(NA,n-k,H)
AICc_4 <- matrix(NA,n-k,H)




for(i in 1:(n-k)){
  ### One Month rolling forecasting
  # Expanding Window 
  train_1 <- window(visitors_ts, end=st + i/p)  ## Window Length: k+i
  
  # Sliding Window - keep the training window of fixed length. 
  # The training set always consists of k observations.
  train_2 <- window(visitors_ts, start=st+(i-k+1)/p, end=st+i/p) ## Window Length: k
  
  test <- window(visitors_ts, start=st + (i+1)/p, end=st + (i+H)/p) ## Window Length: H
  
  if (i<10) {
    print(i)
    cat(c("*** CV", i,":","len(Expanding Window):",length(train_1), "len(Sliding Window):",length(train_2), "len(Test):",length(test),'\n'  ))
    cat(c("*** TRAIN -  Expanding WIndow:",tsp(train_1)[1],'-',tsp(train_1)[2],'\n'))
    cat(c("*** TRAIN - Sliding WIndow:",tsp(train_2)[1],'-',tsp(train_2)[2],'\n'))
    cat(c("*** TEST:",tsp(test)[1],'-',tsp(test)[2],'\n'))
    cat("*************************** \n \n")
  }
  
  
  fit_1 <- Arima(train_1, order=c(1,0,1), seasonal=list(order=c(0,1,2), period=p),
                 include.drift=TRUE, lambda='auto', method="ML")
  fcast_1 <- forecast(fit_1, h=H)
  
  
  fit_2 <- Arima(train_2, order=c(1,0,1), seasonal=list(order=c(0,1,2), period=p),
                 include.drift=TRUE, lambda='auto', method="ML")
  fcast_2 <- forecast(fit_2, h=H)
  
  fit_3 <- ets(train_1, model = 'MAM')
  fcast_3 <- forecast(fit_3, h=H)
  
  fit_4 <- ets(train_2, model = 'MAM')
  fcast_4 <- forecast(fit_4, h=H)
  
  AICc_1[i, 1:length(test)] <- fit_1$aicc
  AICc_2[i, 1:length(test)] <- fit_2$aicc
  AICc_3[i, 1:length(test)] <- fit_3$aicc
  AICc_4[i, 1:length(test)] <- fit_4$aicc
  
  
  mae_1[i,1:length(test)] <- abs(fcast_1[['mean']]-test)
  mae_2[i,1:length(test)] <- abs(fcast_2[['mean']]-test)
  mae_3[i,1:length(test)] <- abs(fcast_3[['mean']]-test)
  mae_4[i,1:length(test)] <- abs(fcast_4[['mean']]-test)
  
}

# plot MAE
plot(1:12, colMeans(mae_1,na.rm=TRUE), type="l",col=1,xlab="horizon", ylab="MAE",
     ylim=c(15,40))
lines(1:12, colMeans(mae_2,na.rm=TRUE), type="l",col=2)
lines(1:12, colMeans(mae_3,na.rm=TRUE), type="l",col=3)
lines(1:12, colMeans(mae_4,na.rm=TRUE), type="l",col=4)
legend("topleft",legend=c("ARIMA - Expanding Window","ARIMA - Sliding Window", 'ETS - Expanding Window', 'ETS - Sliding Window'),col=1:4,lty=1)



# plot rmse
plot(1:12, sqrt(colMeans(mae_1^2,na.rm=TRUE)), type="l",col=1,xlab="horizon", ylab="RMSE",
     ylim=c(20,50))
lines(1:12, sqrt(colMeans(mae_2^2,na.rm=TRUE)), type="l",col=2)
lines(1:12, sqrt(colMeans(mae_3^2,na.rm=TRUE)), type="l",col=3)
lines(1:12, sqrt(colMeans(mae_4^2,na.rm=TRUE)), type="l",col=4)
legend("topleft",legend=c("ARIMA - Expanding Window","ARIMA - Sliding Window", 'ETS - Expanding Window', 'ETS - Sliding Window'),col=1:4,lty=1)


# plot aicc
plot(1:80, rowMeans(AICc_1,na.rm=TRUE), type="l",col=1,xlab="iteration", ylab="AICc",
     ylim=c(-600,3000))
lines(1:80, rowMeans(AICc_2,na.rm=TRUE), type="l",col=2)
lines(1:80, rowMeans(AICc_3,na.rm=TRUE), type="l",col=3)
lines(1:80, rowMeans(AICc_4,na.rm=TRUE), type="l",col=4)
legend("topleft",legend=c("ARIMA - Expanding Window","ARIMA - Sliding Window", 'ETS - Expanding Window', 'ETS - Sliding Window'),col=1:4,lty=1)



### 4.
