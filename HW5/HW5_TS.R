library(tseries)
library(fpp)
library(TSA)
library(forecast)



load('condmilk.rda')

## 1.
train <- condmilk[1:108]
test <- condmilk[109:120]



train_milk <- ts(train, start = c(1971, 1), frequency = 12)

test_milk <- ts(test, start = c(1980, 1), frequency = 12)



## 2.
autoplot(train_milk)
tsdisplay(train_milk)

### From the graph we see that variance change overtime so we do need transformations
lambda <- BoxCox.lambda(train_milk)
lambda

# do transformation based on the best lambda 
tran_milk <- BoxCox(train_milk, lambda = lambda)

autoplot(tran_milk)
tsdisplay(tran_milk)


## 3. ????

## 1st difference with lag 12
plot(diff(tran_milk, lag = 12))


## KPSS test
kpss.test(diff(tran_milk), null = c("Level", "Trend"), lshort = TRUE)
kpss.test(diff(diff(tran_milk), null = c("Level", "Trend"), lshort = TRUE))



## 4.
model_1 <- auto.arima(train_milk, lambda = 'auto', seasonal = TRUE, trace = TRUE)

model_2 <- auto.arima(train_milk, lambda = 'auto', seasonal = TRUE, trace = TRUE, D = 1, d = 1)


## 5.
checkresiduals(model_1)

checkresiduals(model_2)



## 6.
h <- 12

### Model 1

model_forecast_1 <-forecast(model_1, h)

model_forecast_1

plot(model_forecast_1)

### Model 2

model_forecast_2 <-forecast(model_2, h)

model_forecast_2

plot(model_forecast_2)

## 7.

# Model 1
MSE_1 <- sum((model_forecast_1$mean - test_milk)^2) / 12

MAEP_1 <- sum(abs((test_milk - model_forecast_1$mean) / model_forecast_1$mean))/12

accuracy(model_forecast_1, test_milk)

# Model 2

MSE_2 <- sum((model_forecast_2$mean - test_milk)^2) / 12

MAEP_2 <- sum(abs((test_milk - model_forecast_2$mean) / model_forecast_2$mean))/12

accuracy(model_forecast_2, test_milk)

## 8.
model_SNaive <- snaive(train_milk, h)

MSE_S <- sum((model_SNaive$mean - test)^2) / 12

MAEP_S <- sum(abs((test - model_SNaive$mean) / model_SNaive$mean))/12

accuracy(model_SNaive, test_milk)

