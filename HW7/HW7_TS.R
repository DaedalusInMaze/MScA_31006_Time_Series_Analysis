library(fpp)
library(tseries)
library(forecast)
library(TSA)
library(vars)

load('motel.rda')

### 1.
RN <- motel[,1]
TAKING <- motel[,2]
CPI <- motel[,3]

# Derived
cost <- TAKING*1000/RN # Unit Thousand Australian dollars
# cost <- ts(cost, start = c(1980,1), frequency = 12)
## a.

# ts for cost
tsdisplay(cost)
plot(cost)

# ts for cpi
tsdisplay(CPI)
plot(CPI)

## b.

# cost against cpi
plot.ts(cbind(cost, CPI))
plot(CPI, cost, type = 'p')
cor(CPI, cost)

# correlation
cor(log(cost), log(CPI))

## We take log because ARMAX need stationary of data but both CPI and cost are not stationary

# log
plot(log(CPI), log(cost), type = 'p')



## 2.
model1 <- tslm(log(cost) ~ log(CPI))
model1

plot(log(CPI), log(cost))
abline(-1.682, 1.303)

summary(model1)
checkresiduals(model1)



## 3.
model2 <- auto.arima(cost, lambda = 0, seasonal = TRUE, trace = TRUE, xreg = CPI, D = 1)
summary(model2)
checkresiduals(model2)


## 4.

# Using Naive
naive_pred <- naive(CPI, h = 18)
naive_pred
plot(naive_pred)

# Using ARIMA
pred <- forecast(model2, xreg = CPI_pred$mean, h = 18)
pred
plot(pred)


## 5.
#### a.
variables <- cbind(log(cost), log(CPI))
# VARselect(variables, lag.max = 8, type = 'const')$selection
var1 <- VAR(variables, p = 10, type = 'both', season = 12)
serial.test(var1, lags.pt = 10, type = 'PT.asymptotic')

var_pred <- forecast(var1, h = 18)
var_pred
autoplot(var_pred)


#### c.
summary(var1)

acf(residuals(var1), main = 'Residuals for the model')
#cost_res <- residuals(var1)[,1]
#cpi_res <- residuals(var1)[,2]
