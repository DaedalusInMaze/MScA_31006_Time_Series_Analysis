library("rio")


# 1. Convert xls files to csv files and combine 
#dat_path <- paste0(getwd(), '/Traffic Flow Data')
#xls <- dir(path = dat_path, pattern = "xls")
#created <- mapply(convert, xls, gsub("xls", "csv", xls))
#unlink(xls) # delete xlsx files

library(forecast)
library(fpp)
library(TSA)
library("readr")
library('data.table')

### read all csv and combine
filenames <- gsub("\\.csv$","", list.files(pattern="\\.csv$"))

for(i in c(1:16)){
  assign(paste0('dat', i), fread(paste(filenames[i], ".csv", sep=""),  select = c(3, 5), blank.lines.skip = TRUE))}

### Combine
dat = rbind(dat2[5:28], dat3[5:28], dat4[5:28], dat5[5:28], dat6[5:28], dat7[5:28], dat8[5:28], 
            dat9[5:28], dat10[5:28], dat11[5:28], dat12[5:28], dat13[5:28], dat14[5:28], 
            dat15[5:28], dat16[5:28], dat16[5:28], dat1[5:28])

### Rename
names(dat)[names(dat) == "...5"] <- "Traffic"
names(dat)[names(dat) == "...3"] <- "Time"


dat$Traffic <- as.numeric(dat$Traffic)

# 2. Split the dataset into a training dataset which includes 6/16/2013 - 6/30/2013 
# samples and a test dataset which includes 7/1/2013 samples and plot the ACF and PACF.
train <- dat[1: 384]

test <- dat[385:408]

### plot
#firstHour <- 24*(as.Date("2013-06-16 01:00:00")-as.Date("2013-01-01 00:00:00"))
#secondHour <- 24*(as.Date("2013-07-01 01:00:00")-as.Date("2013-01-01 00:00:00"))

#train_dt <- ts(train$Traffic, start = c(2013, 6, 16), frequency=24*365)
#test_dt <- ts(train$Traffic, start = c(2013, secondHour), frequency=24*365)

win.graph(width=6, height=6,pointsize=12)
tsdisplay(ts(dat$Traffic))



# 3. 

### Auto-arima
train_dat <- ts(train$Traffic)
fit<-auto.arima(train_dat, trace = TRUE, lambda = 'auto')
print(fit)

# Find other p, q
res = data.frame(model=1, AICc=1, BIC=1)
ind = 1


## Based on PACF graph, we will keep p = 2 constant
for (q in seq(0,3)){
  for (d in seq(0,2)){
    m.trial = Arima(train_dat, order=c(2,d,q))
    res[ind, "model"] = paste0(2,d,q, collapse = ",")
    res[ind, "AICc"] = m.trial$aicc
    res[ind, "BIC"] = m.trial$bic
    ind = ind + 1
  }
}

res

### Based on AICc, we choose 203 which gives smallest AICc


# 4. 
train_tssea <- ts(train$Traffic, frequency = 7*24)
season_fit<-auto.arima(train_tssea, trace = TRUE)

season_fit


# 5. 
prediction_wk <- forecast(season_fit, h = 24)
prediction_wk
plot(prediction_wk)


# 6. 
train_hr <- ts(train$Traffic, frequency = 24)
hr_fit <- auto.arima(train_hr, trace = TRUE, lambda = 'auto')
hr_fit

# 7. 
prediction_day <- forecast(hr_fit, h = 24)
prediction_day
plot(prediction_day)


#8. 
Jul <- data.frame(Time = test$Time[c(8, 9, 17, 18)],
                  Week = prediction_wk$mean[c(8, 9, 17, 18)], 
                  Day = prediction_day$mean[c(8, 9, 17, 18)],
                  Actual = test$Traffic[c(8, 9, 17, 18)])

Jul

