library(tseries)
library(forecast)
library(fpp)
library(TSA)
library(dplyr)



### 1.
load('usgdp.rda')

usgdp

train <- subset(usgdp, usgdp$Year < 2013)

test <- subset(usgdp, usgdp $ Year > 2012)


### 2.
train_gdp <- ts(train$GDP, start= 1960, frequency = 1)
test_gdp <- ts(test$GDP, start= 2013, frequency = 1)

autoplot(train_gdp)

lambda <- BoxCox.lambda(train_gdp)
lambda

train_gdp %>% BoxCox(lambda = lambda) %>% autoplot()


trans_gdp <- BoxCox(train_gdp, lambda = lambda)

# It does not need any transformation or sqrt is preferred.


### 3. Plot the 1st and 2nd order difference of the data. 
### Apply KPSS Test for Stationarity to determine which difference order results in a stationary dataset.

## 1st difference
plot(diff(trans_gdp))

## 2nd difference
plot(diff(diff(trans_gdp)))

## kpss
kpss.test(diff(trans_gdp), null = c("Level", "Trend"), lshort = TRUE)
kpss.test(diff(diff(trans_gdp), null = c("Level", "Trend"), lshort = TRUE))

# We want choose second order as the null hypothesis is stationary so we do not want to reject null hypothesis.


### 4. Fit a suitable ARIMA model to the training dataset using the auto.arima() function. 
### Remember to transform the data first if necessary. Report the resulting 𝑝,𝑑,𝑞 and the coefficients values.
fit <- auto.arima(train_gdp, trace = TRUE, lambda = 'auto')
fit

Arima(train_gdp, order = c(1,1,0), lambda = lambda, include.drift = T)


### 5. Compute the sample Extended ACF (EACF) and use the Arima() function to try some other 
### plausible models by experimenting with the orders chosen. Limit your models to 𝑞,𝑝 ≤2 and 𝑑≤2. 
### Use the model summary() function to compare the Corrected Akaike information criterion (i.e., AICc) 
### values (Note: Smaller values indicated better models).
?eacf

eacf(trans_gdp)

# p = 1, d = 1, q = 1
a111 <- Arima(train_gdp, order = c(1,1,1), lambda = lambda, include.drift = T)
summary(a111)

# p = 1, d = 1, q = 2
a112 <- Arima(train_gdp, order = c(1,1,2), lambda = lambda, include.drift = T)
summary(a112)

# p = 2, d = 1, q = 1
a211 <- Arima(train_gdp, order = c(2,1,1), lambda = lambda, include.drift = T)
summary(a211)

# p = 1, d = 2, q = 1
a121 <- Arima(train_gdp, order = c(1,2,1), lambda = lambda, seasonal = F)
summary(a121)

# p = 1, d = 2, q = 2
a122 <- Arima(train_gdp, order = c(1,2,2), lambda = lambda)
summary(a122)

# p = 2, d = 2, q = 1
a221 <- Arima(train_gdp, order = c(2,2,1), lambda = lambda)
summary(a221)

# p = 2, d = 2, q = 2
a222 <- Arima(train_gdp, order = c(2,2,2), lambda = lambda)
summary(a222)


### The best model from EACF is q = 1, d = 2, q = 1 and the AICc is 442.5


### 6.Use the model chosen in Question 4 to forecast and plot the GDP forecasts with 80 and 95 % 
### confidence levels for 2013 - 2017 (Test Period).

test.fc <- forecast(fit, h = 5, level = c(80, 95))
test.fc

plot(forecast(fit,h=5),include=100)  


### 7. Compare your forecasts with the actual values using error = actual - estimate
### and plot the errors. (Note: Use the forecast $mean element for the forecast estimate)
error_gdp <- test_gdp - test.fc$mean

plot(error_gdp)


### 8. Calculate the sum of squared errors.
SSE <- sum(error_gdp^2)
