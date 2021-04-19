library(tseries)
library(forecast)
library(fpp)
library(lubridate)
library(ggplot2)


load('visitors.rda', envir = parent.frame())

## Q1

y_quarterly <-  ts(visitors$Arrivals, start=c(1981, 1), frequency=4)

plot(y_quarterly, main = 'Visitors in Australia from US', xlab = 'Quater')


## Q2

# Additive
fit_add <- decompose(y_quarterly, type="additive")

plot(fit_add)

# Multiplicative
fit_mult <- decompose(y_quarterly, type="multiplicative")

plot(fit_mult)

### Based on the two plots, I think a multiplicative time series fits better



## Q3

#• Linear trend with additive seasonality

fit_hw_add_1 <- hw(y_quarterly, h=16, seasonal="add",damped=FALSE)
summary(fit_hw_add_1)


#• Linear trend with multiplicative seasonality

fit_hw_mul_1 <- hw(y_quarterly, h=16, seasonal="mult",damped=FALSE)
summary(fit_hw_mul_1)


#• Linear trend with additive seasonality and damping

fit_hw_add_2 <- hw(y_quarterly, h=16, seasonal="add",damped=TRUE)
summary(fit_hw_add_2)


#• Linear trend with multiplicative seasonality and damping
fit_hw_mul_2 <- hw(y_quarterly, h=16, seasonal="mult",damped=TRUE)
summary(fit_hw_mul_2)


#• Seasonal Naive
fit_sn <- snaive(y_quarterly, h = 16)


## Q4
### Use the accuracy() function to compare the Root-Mean-Square-Error (RMSE) 
### values of the forecasts from the various methods. Which do you prefer and why?

rmse_hw_add_no_damp <- accuracy(fit_hw_add_1)[2]
rmse_hw_mul_no_damp <- accuracy(fit_hw_mul_1)[2]
rmse_hw_add_damp <- accuracy(fit_hw_add_2)[2]
rmse_hw_mul_damp <- accuracy(fit_hw_mul_2)[2]
rmse_sn <- accuracy(fit_sn)[2]


## Q5
# Use the checkresiduals() function to check that the residuals from the best model look like white noise 
# and provide a summary of the model’s smoothing parameters using the summary() function.
checkresiduals(fit_hw_add_1)

checkresiduals(fit_hw_mul_1)

checkresiduals(fit_hw_add_2)

checkresiduals(fit_hw_mul_2)

checkresiduals(fit_sn)

?checkresiduals
