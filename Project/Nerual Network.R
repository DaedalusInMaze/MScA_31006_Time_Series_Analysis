# library
library(forecast)
library(TSA)
library(tseries)
library(ggplot2)
library(vars)
library(fpp)
library(corrplot)
library(RColorBrewer)
library(Metrics)


# load data
elec <- read.csv('electricity_breakdown.csv')

##### NNETAR


## Time series overview
elec_ts <- ts(elec, start = c(1973, 1), frequency = 12)

electricity <- elec_ts[, -c(1,3)]
consump <- elec_ts[, 2]
temperature <- elec_ts[, 3]
coal <- elec_ts[, 4]
petro <- elec_ts[, 5]
gas <- elec_ts[, 6]
nucl <- elec_ts[, 7]
hydro <- elec_ts[, 8]
biom <- elec_ts[, 9]
geoth <- elec_ts[, 10]
solar <- elec_ts[, 11] + 0.001
wind <- elec_ts[, 12] + 0.001


## 
var_com <- cbind(temperature, coal, petro, gas, nucl, hydro, biom, geoth, solar, wind)
v_train <- var_com[1:552,]
v_test <- var_com[553:576,]
con_train <- consump[1:552]
con_test <- consump[553:576]

v_train <- ts(v_train, start = c(1973,1), frequency = 12)
con_train <- ts(con_train, start = c(1973,1), frequency = 12)
v_test <- ts(v_test, start = c(2019,1), frequency = 12)
con_test <- ts(con_test, start = c(2019,1), frequency = 12)



nn <- nnetar(con_train, repeats = 30, xreg = v_train)
summary(nn)

autoplot(forecast(nn, xreg = v_test, h = 24))
test_pred <- forecast(nn, xreg = v_test, h = 24)

test_pred
con_test

rmse(test_pred$mean, con_test)


test_pred

##### RNN





