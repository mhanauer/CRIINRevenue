---
  title: "Times Series with Revenue"
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Review revenue.  Get date in mdy format.  Need to change Jul to correct number and add day (1)
```{r}
## Revenue data
setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/SustainWorkshop/RevenueAnalysis")
CIN_revenue = read.csv("CH17-37_20190731_123328.csv", header = TRUE)

head(CIN_revenue)
library(lubridate)
library(forecast)
library(ggplot2)
library(stringr)
library(fpp2)
library(urca)
head(CIN_revenue)
colnames(CIN_revenue)[2] = "location"
head(CIN_revenue)
CIN_revenue_dat = CIN_revenue
head(CIN_revenue_dat)
CIN_revenue_dat$Year.Month = str_replace_all(CIN_revenue_dat$Year.Month, c("Jan" = "1", "Feb"="2", "Mar"="3", "Apr"="4", "May"="5", "Jun"="6", "Jul"="7", "Aug"="8", "Sep" = "9", "Oct"="10", "Nov"="11", "Dec"="12"))
head(CIN_revenue_dat)
CIN_revenue_dat$Year.Month = paste("1-",CIN_revenue_dat$Year.Month, sep = "")
head(CIN_revenue_dat)
CIN_revenue_dat$Year.Month = dmy(CIN_revenue_dat$Year.Month)
head(CIN_revenue_dat)

CIN_revenue_dat$location = NULL
###

#### Need to remove the $ we are removing the .00 so divide by 100 to get original payment
#### Then divide by 100 to get estimates in the hundreds of thousands
CIN_revenue_dat$Payments = gsub("\\D", "", CIN_revenue_dat$Payments)
CIN_revenue_dat$Payments = as.numeric(CIN_revenue_dat$Payments) 
CIN_revenue_dat$Payments = CIN_revenue_dat$Payments / 100
### Get rid of anything before May
CIN_revenue_dat = subset(CIN_revenue_dat, Year.Month < "2019-06-01")

### Get rid of non-recoverable
CIN_revenue_dat = subset(CIN_revenue_dat, Financial.Class.Value != "Non-Recoverable")

#### Get rid of any revenue that is zero
CIN_revenue_dat = subset(CIN_revenue_dat, Payments > 0)
CIN_revenue_dat
```
Now aggregate data by month
```{r}
head(CIN_revenue_dat)
library(prettyR)
describe.factor(CIN_revenue_dat$Year.Month)
CIN_revenue_dat_month = data.frame(date =  CIN_revenue_dat$Year.Month, revenue = CIN_revenue_dat$Payments)
CIN_revenue_dat_month$revenue = as.numeric(CIN_revenue_dat_month$revenue)
head(CIN_revenue_dat_month)

CIN_revenue_dat_month = aggregate(.~date, data = CIN_revenue_dat_month, sum)
head(CIN_revenue_dat_month)
dim(CIN_revenue_dat_month)
CIN_revenue_dat_month$revenue = round((CIN_revenue_dat_month$revenue))
CIN_revenue_dat_month
```
Regularly spaced time and get rid of June and July those numbers are not ready yet
```{r}
## Get rid of date
CIN_revenue_dat_month$date = NULL
dim(CIN_revenue_dat_month)
head(CIN_revenue_dat_month)
CIN_revenue_dat_month_ts = ts(CIN_revenue_dat_month, start = c(2017, 7), end = c(2019,5), frequency = 12)
head(CIN_revenue_dat_month_ts)
CIN_revenue_dat_month_ts
```
Plot the data over time
Trend: long term increase or increase; Season = short term predictable with some patteron; cyclic: 1 to 2 year trend correlated with some economic event
```{r}
ggseasonplot(CIN_revenue_dat_month_ts) +
  ylab("$ Millions")+
  ggtitle("Figure 1: Revenue per year")

## Lag plot 
#gglagplot(CIN_revenue_dat_month)

```
Two time series assumptions
Mean of residuals is zero if not then add the mean of the residuals to all forcasts
Residuals are uncorrelated
Important but not necessary
Residuals have constant var
Residuals normal

Hetero: the variance of y should be the same as the variance of the errors

bptest is heterosked where is regresses the residuals on the variables and evaluates if those variables explain too much of the variance in the residuals 

Use box test to see if the times series is independence
Acf to see if there is significant auto-correlation at different lags (so lag 1 means the relationship between each time point and the one before it) can also be used for residuals

Partial is the relationship between the the time point y and 1 excluding the all the lower order lags

https://people.duke.edu/~rnau/411arim3.htm

Box.test: https://stat.ethz.ch/R-manual/R-devel/library/stats/html/box.test.html

ur.kpss: Whether there some consent trend or a constent with a linear component that can be explained  
```{r}
ggAcf(CIN_revenue_dat_month_ts, type = "correlation")
ggAcf(CIN_revenue_dat_month_ts, type = "partial")

### More tests for autocorrelation

Box.test(CIN_revenue_dat_month_ts, type = "Ljung-Box")
summary(ur.kpss(CIN_revenue_dat_month_ts))

## If you want to adjust for the days of the year
#CIN_revenue_dat_month_ts / monthdays(CIN_revenue_dat_month_ts)
```
For regression include other variables not sure how to plot with multiple variables in time series 
```{r}
CIN_revenue_dat_month = data.frame(date =  CIN_revenue_dat$Year.Month, revenue = CIN_revenue_dat$Payments, number_pay = CIN_revenue_dat$Number.of.Payments.Received)
CIN_revenue_dat_month$revenue = as.numeric(CIN_revenue_dat_month$revenue)

CIN_revenue_dat_month = aggregate(.~date, data = CIN_revenue_dat_month, sum)
head(CIN_revenue_dat_month)
colnames(CIN_revenue_dat_month)[1:3] = c("date", "revenue", "number_pay")

typeof(CIN_revenue_dat_month$date)
CIN_revenue_dat_month$date
### Create a time variable that is the trend
CIN_revenue_dat_month$time = 1:dim(CIN_revenue_dat_month)[1]
### ### Create a quarter variable
CIN_revenue_dat_month$quarter = quarter(CIN_revenue_dat_month$date)
CIN_revenue_dat_month
CIN_revenue_dat_month_ts = ts(CIN_revenue_dat_month, start = c(2017, 7), end = c(2019,5), frequency = 12)
head(CIN_revenue_dat_month_ts)
```




Autoregressive model is like a lagged version of a linear model with the previous time point values as predictors.  So the value at time point 2, 3,..n predicting time point one?
P is the autoregressive parameter, which is the number of time points we go back to so we include each time point minus the previous as an average parameter if AR2 then each time point and first and second difference.

C = difference plus error

Moving average model uses the errors to predict future values

p = number of lags or previous y data points to predict
q = "moving average" is about the residuals as predicts of different time points
d = degree of difference in y

Order = (p,d,q)

Develop univariate model, because you do not have to select what values of other covariates will be 

Maybe phi is the average correlation between all time points and the difference according p (all t minus 1's).  For example, p = 1 is the all t minus 1's between. Phi is the average correlation between t and minus 1's.

Testing versus training: One shot testing with small amounts of data: https://otexts.com/fpp2/forecasting-on-training-and-test-sets.html


```{r}
CIN_revenue_dat_unit =  CIN_revenue_dat_month_ts[,2]
head(CIN_revenue_dat_unit)

arima_model =  auto.arima(CIN_revenue_dat_unit, seasonal = FALSE)
summary(arima_model)

### Test assumptions
ggAcf(residuals(arima_model))
ggPacf(residuals(arima_model))
Box.test(residuals(arima_model), type = "Ljung-Box")
summary(ur.kpss(residuals(arima_model)))

### Training versus testing
CIN_revenue_dat_unit_train = CIN_revenue_dat_unit[1:17]
1-(17/23)
CIN_revenue_dat_unit_test = CIN_revenue_dat_unit[18:23]
#### Model for both
arima_model_train =  auto.arima(CIN_revenue_dat_unit_train, seasonal = FALSE)
accuracy(arima_model_train)
arima_model_test = Arima(CIN_revenue_dat_unit_test, model = arima_model_train)
accuracy(arima_model_test)

### Forcast model
forecast_model = forecast(arima_model)
summary(forecast_model)
autoplot(forecast_model)
```

Try neural network feed forward model
Inputs: Values for the covariates in the model
Box-Cox set to auto to find a transformation of the data that makes it normal if no change is needed defaults to data.

Inputs: number of covariates
Hidden layers: unknown
Output: usually just one the predicted values
Little p = number of lags to include
Big p = number of lags for season series
k = number of nodes or functions in the one hidden layer

I think NNAR(1,1,2) mean one non-lagged value and maybe 1 seasonal lagged value, and two nodes
2-2-1 mean two inputs (one non-seasonal lag and the original data), and two nodes, 9 weights means 9 regression coeffients going to the hidden nodes
Feed forward model I am assuming a sigmoid function for hidden layers.
```{r}

CIN_revenue_dat_unit =  CIN_revenue_dat_month_ts[,2]
head(CIN_revenue_dat_unit)
nn_auto = nnetar(CIN_revenue_dat_unit)
summary(nn_auto)
nn_auto
### evaluate accuracy
residuals_nn_auto = data.frame(resid_nn_auto= residuals(nn_auto))
residuals_nn_auto = apply(residuals_nn_auto, 1, mean)
residuals_nn_auto = data.frame(residuals_nn_auto)
ggAcf(residuals(residuals_nn_auto))
ggPacf(residuals(residuals_nn_auto))
Box.test(residuals_nn_auto, type = "Ljung-Box")
summary(ur.kpss(residuals_nn_auto$residuals_nn_auto))
library(lmtest)
hist(round(residuals_nn_auto),3)
accuracy(nn_auto)
#### Compare training versus testing
#### Model for both
nn_model_train =  nnetar(CIN_revenue_dat_unit_train)
accuracy(nn_model_train)
nn_model_test = nnetar(CIN_revenue_dat_unit_test, model = nn_model_train)
accuracy(nn_model_test)

### compare accuracy
accuracy(nn_auto)
accuracy(arima_model)
dm.test(residuals_nn_auto$residuals_nn_auto, residuals(arima_model))

### Forecast
forecast_nn_auto = forecast(nn_auto, PI = TRUE)
forecast_nn_auto
autoplot(forecast_nn_auto)+
  labs(title = "Figure 2: Forecasts for CIN Bloomington July 2019 to May 2021", y = "$ Millions in revenue per month", x = "Time")

```
Try to get a longitduinal data set with the number of payments per class
Average revenue per agency per month
```{r}
head(CIN_revenue_dat)
describe.factor(CIN_revenue_dat$Financial.Class.Value)

library(prettyR)
describe.factor(CIN_revenue_dat$Year.Month)

CIN_revenue_sim = data.frame(date =  CIN_revenue_dat$Year.Month, revenue = CIN_revenue_dat$Payments, class = CIN_revenue_dat$Financial.Class.Value)
CIN_revenue_sim$revenue = as.numeric(CIN_revenue_sim$revenue)
head(CIN_revenue_sim)
CIN_revenue_sim
library(psych)
dummy_class = dummy.code(CIN_revenue_sim$class)
dummy_class = data.frame(dummy_class)
apply(dummy_class,2, sum)
### Need to combine into other category Voc Rehab and Client Assistance may delete later
dummy_class$Voc.Rehab = NULL
dummy_class$Client.Assistance = NULL
CIN_revenue_sim$Non.Recoverable = NULL
CIN_revenue_sim$class = NULL
CIN_revenue_sim = data.frame(CIN_revenue_sim, dummy_class)
head(CIN_revenue_sim)
### Combine Medicaid and HIP for later model
CIN_revenue_sim$Medicaid_HIP = (CIN_revenue_sim$Medicaid+ CIN_revenue_sim$HIP)/2

CIN_revenue_sim = aggregate(.~date, data = CIN_revenue_sim, sum)
CIN_revenue_sim$Non.Recoverable = NULL
head(CIN_revenue_sim)
```
Get descriptives
```{r}
round(apply(CIN_revenue_sim[2:12], 2, mean),0)
round(apply(CIN_revenue_desc[2:12], 2, sd),0)


```


Try dynamic simulations
```{r}
library(dynsim)
library(DataCombine)
CIN_revenue_dat_month_dy = slide(CIN_revenue_sim, Var = "revenue")
head(CIN_revenue_dat_month_dy)
colnames(CIN_revenue_dat_month_dy)[14] = c("revuneMinus1")
CIN_revenue_dat_month_dy$Agency  = ifelse(CIN_revenue_dat_month_dy$Agency == "Inf", 0, CIN_revenue_dat_month_dy$Agency)
head(CIN_revenue_dat_month_dy)


model_9 = lm(revenue ~revuneMinus1+Commercial  +Medicaid_HIP, data = CIN_revenue_dat_month_dy)
summary(model_9)
checkresiduals(model_9)
library(car)
vif(model_9)
library(lmtest)
bptest(model_9)


com_1 <- data.frame(Commercial = quantile(CIN_revenue_dat_month_dy$Commercial, .75),revuneMinus1 =  mean(CIN_revenue_dat_month_dy$revuneMinus1, na.rm = TRUE), DCS = mean(CIN_revenue_dat_month_dy$DCS), Medicaid_HIP = mean(CIN_revenue_dat_month_dy$Medicaid_HIP), MRO = mean(CIN_revenue_dat_month_dy$MRO))

com_2 <- data.frame(Commercial =  quantile(CIN_revenue_dat_month_dy$Commercial, .50),revuneMinus1 =  mean(CIN_revenue_dat_month_dy$revuneMinus1, na.rm = TRUE), DCS = mean(CIN_revenue_dat_month_dy$DCS), Medicaid_HIP = mean(CIN_revenue_dat_month_dy$Medicaid_HIP), MRO = mean(CIN_revenue_dat_month_dy$MRO))

com_3 <- data.frame(Commercial = quantile(CIN_revenue_dat_month_dy$Commercial, .25),revuneMinus1 =  mean(CIN_revenue_dat_month_dy$revuneMinus1, na.rm = TRUE), DCS = mean(CIN_revenue_dat_month_dy$DCS), Medicaid_HIP = mean(CIN_revenue_dat_month_dy$Medicaid_HIP), MRO = mean(CIN_revenue_dat_month_dy$MRO))


medhip_1 <- data.frame(Medicaid_HIP = quantile(CIN_revenue_dat_month_dy$Medicaid_HIP, .75),revuneMinus1 =  mean(CIN_revenue_dat_month_dy$revuneMinus1, na.rm = TRUE), DCS = mean(CIN_revenue_dat_month_dy$DCS), Commercial = mean(CIN_revenue_dat_month_dy$Commercial), MRO = mean(CIN_revenue_dat_month_dy$MRO))

medhip_2 <- data.frame(Commercial =  quantile(CIN_revenue_dat_month_dy$Commercial, .50),revuneMinus1 =  mean(CIN_revenue_dat_month_dy$revuneMinus1, na.rm = TRUE), DCS = mean(CIN_revenue_dat_month_dy$DCS), Medicaid_HIP = mean(CIN_revenue_dat_month_dy$Medicaid_HIP), MRO = mean(CIN_revenue_dat_month_dy$MRO))

medhip_3 <- data.frame(Commercial = quantile(CIN_revenue_dat_month_dy$Commercial, .25),revuneMinus1 =  mean(CIN_revenue_dat_month_dy$revuneMinus1, na.rm = TRUE), DCS = mean(CIN_revenue_dat_month_dy$DCS), Medicaid_HIP = mean(CIN_revenue_dat_month_dy$Medicaid_HIP), MRO = mean(CIN_revenue_dat_month_dy$MRO))


plot_com =  dynsimGG(sim_com, leg.labels = c("25%", "15%", "5%")) +
  labs(title = "Figure 3: Increase in Commercial Payments June 2019 to June 2020", y = "Predicted revenue in $ Millions", x = "Months")+
  theme(plot.title = element_text(size= 12))+
  theme(axis.title.x = element_text(size= 12))+
  theme(axis.title.y = element_text(size= 12))
plot_com

plot_medhip = dynsimGG(sim_medhip, leg.labels = c("25%", "15%", "5%")) +
  labs(title = "Increase in Medicaid and HIP Payments June 2019 to June 2020", y = "Predicted revenue in $ Millions", x = "Months", size = 12)+
  theme(plot.title = element_text(size= 12))+
  theme(axis.title.x = element_text(size= 12))+
  theme(axis.title.y = element_text(size= 12))
plot_medhip

library(ggpubr)
#ggarrange(plot_com, plot_medhip, ncol = 2)
6.1-4.7
5.5-4.7
```
################
Model building for dynsim
```{r}
model_1 = lm(revenue ~Agency +Commercial, data = CIN_revenue_dat_month_dy)
summary(model_1)
checkresiduals(model_1)

CIN_revenue_dat_month_dy$DCS
model_2 = lm(revenue ~ revuneMinus1+ Commercial +DCS, data = CIN_revenue_dat_month_dy)
summary(model_2)
checkresiduals(model_2)


CIN_revenue_dat_month_dy$EAP
model_3 = lm(revenue ~revuneMinus1+Commercial +DCS + EAP, data = CIN_revenue_dat_month_dy)
summary(model_3)
checkresiduals(model_3)


CIN_revenue_dat_month_dy$Grant
model_4 = lm(revenue ~revuneMinus1+ Commercial +DCS + Grant, data = CIN_revenue_dat_month_dy)
summary(model_4)
checkresiduals(model_4)


CIN_revenue_dat_month_dy$HIP
model_5 = lm(revenue ~revuneMinus1+ Commercial +DCS + HIP, data = CIN_revenue_dat_month_dy)
summary(model_5)
checkresiduals(model_5)


CIN_revenue_dat_month_dy$Medicaid
model_6 = lm(revenue ~revuneMinus1+Commercial +DCS + HIP + Medicaid , data = CIN_revenue_dat_month_dy)
summary(model_6)
checkresiduals(model_6)
library(car)
vif(model_6)
### Try combining 
CIN_revenue_dat_month_dy$Medicaid
model_6a = lm(revenue ~revuneMinus1+Commercial +DCS  + Medicaid , data = CIN_revenue_dat_month_dy)
summary(model_6a)
checkresiduals(model_6a)
CIN_revenue_dat_month_dy$Medicaid_HIP = (CIN_revenue_dat_month_dy$Medicaid+CIN_revenue_dat_month_dy$HIP)/2

model_6b = lm(revenue ~revuneMinus1+Commercial + DCS +Medicaid_HIP, data = CIN_revenue_dat_month_dy)
summary(model_6b)
checkresiduals(model_6a)

CIN_revenue_dat_month_dy$Medicare.B
model_7 = lm(revenue ~revuneMinus1+Commercial + DCS +Medicaid_HIP + Medicare.B, data = CIN_revenue_dat_month_dy)
summary(model_7)
checkresiduals(model_7)

CIN_revenue_dat_month_dy$MRO
model_8 = lm(revenue ~revuneMinus1+Commercial + DCS +Medicaid_HIP + MRO, data = CIN_revenue_dat_month_dy)
summary(model_8)
checkresiduals(model_8)

CIN_revenue_dat_month_dy$Self.Pay
model_9 = lm(revenue ~revuneMinus1+Commercial + DCS +Medicaid_HIP + MRO +Self.Pay, data = CIN_revenue_dat_month_dy)
summary(model_9)
checkresiduals(model_9)
```



##############################
Try nnetear doesn't work when series is two times as long as freq for some odd reason
```{r}
#### Clean the June data
head(CIN_revenue_dat)
dim(CIN_revenue_dat)
dim(CIN_revenue_8_30)
CIN_revenue_8_30 = read.csv("CH16-37_20190830_123423.csv", header = TRUE)
CIN_revenue_8_30
CIN_revenue_8_30$Year.Month = str_replace_all(CIN_revenue_8_30$Year.Month, c("Jan" = "1", "Feb"="2", "Mar"="3", "Apr"="4", "May"="5", "Jun"="6", "Jul"="7", "Aug"="8", "Sep" = "9", "Oct"="10", "Nov"="11", "Dec"="12"))
head(CIN_revenue_8_30)
CIN_revenue_8_30$Year.Month = paste("1-",CIN_revenue_8_30$Year.Month, sep = "")
head(CIN_revenue_8_30)
CIN_revenue_8_30$Year.Month = dmy(CIN_revenue_8_30$Year.Month)
describe.factor(CIN_revenue_8_30$Year.Month)

#### Get rid of beyond June
CIN_revenue_dat = subset(CIN_revenue_dat, Year.Month < "2019-06-01")
describe.factor(CIN_revenue_dat$Year.Month)
### Only grab June 
CIN_revenue_8_30 = subset(CIN_revenue_8_30, Year.Month == "2019-06-01")
describe.factor(CIN_revenue_8_30$Year.Month)
### Combine the data sets
CIN_revenue_dat = rbind(CIN_revenue_dat, CIN_revenue_8_30)
```
Dymsim can only do stable increases over time not dynamic increases over time
```{r}
test_quant = list()
quants = seq(from = .5, to = .5+(.02*11), by =.02)
#Did not work
#quants = as.list(quants)
#quants
## Not the function tried with rnorm only the last value worked
for(i in 1:length(quants)){
  test_quant[[i]] = quantile(CIN_revenue_dat_month_dy$Medicaid_HIP, quants[[i]])
}
test_quant = data.frame(test_quant)
test_quant =  t(test_quant)
colnames(test_quant) = "quants"
test_quant = data.frame(test_quant)
rownames(test_quant) = c() 
test_quant
model_9

medhip_2_increase = data.frame(Medicaid_HIP = test_quant$quants,revuneMinus1 =  rep(mean(CIN_revenue_dat_month_dy$revuneMinus1, na.rm = TRUE), 12), DCS = rep(mean(CIN_revenue_dat_month_dy$DCS), 12), Commercial =rep(mean(CIN_revenue_dat_month_dy$Commercial), 12), MRO = rep(mean(CIN_revenue_dat_month_dy$MRO)), 12)
medhip_2_increase$X12 = NULL

scen_com <- list(com_1, com_2, com_3)
scen_medhip = list(medhip_1, medhip_2, medhip_3)
sim_com <- dynsim(obj = model_9, ldv = "revuneMinus1", scen = scen_com, n = 12)
sim_medhip <- dynsim(obj = model_9, ldv = "revuneMinus1", scen = scen_medhip, n = 12)

### Test medhip 2% increase 
sim_medhip_2_increase = dynsim(obj = model_9, ldv = "revuneMinus1", scen = medhip_2_increase, n = 12)
dynsimGG(sim_medhip_2_increase) +
  labs(title = "Scenario Monthly Revenue CIN Bloomington June 2019 to June 2020", y = "Predicted revenue in $ Millions", x = "Months")+
  theme_grey(base_size = 12)

```
Test sample dat set to if the forecasts are similar yes nnetar is much better
```{r}
n_net = nnetar(sunspotarea)
n_net_plot = autoplot(forecast(n_net, PI = TRUE))
arima_model =  auto.arima(sunspotarea, seasonal = FALSE)
arima_plot = autoplot(forecast(arima_model))
accuracy(arima_model)
accuracy(n_net)
```

