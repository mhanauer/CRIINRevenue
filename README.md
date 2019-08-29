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
#### Need to remove the $ we are removing the .00 so divide by 100 to get original payment
#### Then divide by 100 to get estimates in the hundreds of thousands
CIN_revenue_dat$Payments = gsub("\\D", "", CIN_revenue_dat$Payments)
CIN_revenue_dat$Payments = as.numeric(CIN_revenue_dat$Payments) 
CIN_revenue_dat$Payments = CIN_revenue_dat$Payments / 100
head(CIN_revenue_dat)
```
Now aggregate data by month
```{r}
head(CIN_revenue_dat)
CIN_revenue_dat_month = data.frame(date =  CIN_revenue_dat$Year.Month, revenue = CIN_revenue_dat$Payments)
CIN_revenue_dat_month$revenue = as.numeric(CIN_revenue_dat_month$revenue)
head(CIN_revenue_dat_month)

CIN_revenue_dat_month = aggregate(.~date, data = CIN_revenue_dat_month, sum)
head(CIN_revenue_dat_month)
dim(CIN_revenue_dat_month)
```
Regularly spaced time and get rid of June and July those numbers are not ready yet
```{r}
### Get rid of June and July
CIN_revenue_dat_month[24:25,] = NA
CIN_revenue_dat_month = na.omit(CIN_revenue_dat_month)
## Get rid of date
CIN_revenue_dat_month$date = NULL
dim(CIN_revenue_dat_month)
CIN_revenue_dat_month_ts = ts(CIN_revenue_dat_month, start = c(2017, 7), end = c(2019,5), frequency = 12)
head(CIN_revenue_dat_month_ts)
CIN_revenue_dat_month_ts
```
Plot the data over time
Trend: long term increase or increase; Season = short term predictable with some patteron; cyclic: 1 to 2 year trend correlated with some economic event
```{r}
ggseasonplot(CIN_revenue_dat_month_ts) +
  ylab("$ Millions")+
  ggtitle("Revenue per year")

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

CIN_revenue_dat_month$revenue = CIN_revenue_dat_month$revenue 
typeof(CIN_revenue_dat_month$date)

### Create a time variable that is the trend
CIN_revenue_dat_month$time = 1:dim(CIN_revenue_dat_month)[1]

### ### Create a quarter variable
CIN_revenue_dat_month$quarter = quarter(CIN_revenue_dat_month$date)

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

### Forcast model
forecast_model = forecast(arima_model)
summary(forecast_model)
autoplot(forecast_model)
```
Prove differening
```{r}
CIN_revenue_dat_month_ts[,2]
32361367 - 31926558
diff(CIN_revenue_dat_month_ts[,2], lag = 2)
```
Use the model not just to predict, but to predict based on different scenerios
This would be where we use the test data set, which is five percent so 5 values.

Need to keep everything constant, but one value.  
```{r}
summary(CIN_revenue_dat_month_ts[,3:5])
describe(CIN_revenue_dat_month_ts[,3:5])
number_pay = seq(from= 5680, to= (5680+1200), by = 100)
```

Try autoregressive model with predictors
```{r}
CIN_revenue_dat_month_ts[,2]
CIN_revenue_dat_month_ts[,5]
CIN_revenue_dat_month_ts

arima_model_dy =  auto.arima(CIN_revenue_dat_month_ts[,2], xreg = CIN_revenue_dat_month_ts[,3], seasonal = FALSE)
summary(arima_model_dy)
summary(arima_model)
#ggAcf()
forecast_model_dy = forecast(arima_model_dy, xreg = number_pay)
summary(forecast_model_dy)
autoplot(forecast_model_dy)
forecast_model_dy
```
Example from book
```{r}
### Try example from book
head(elecdaily)

xreg <- cbind(MaxTemp = elecdaily[, "Temperature"],MaxTempSq = elecdaily[, "Temperature"]^2,Workday = elecdaily[, "WorkDay"])

new_dat_test = cbind(MaxTemp=rep(26,14), MaxTempSq=rep(26^2,14),Workday=c(0,1,0,0,1,1,1,1,1,0,0,1,1,1))


test_auto = auto.arima(elecdaily[,1], xreg = xreg)
checkresiduals(test_auto)

fcast <- forecast(test_auto, xreg = new_dat_test)
summary(fcast)
autoplot(fcast) + ylab("Electricity demand (GW)")
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

I think NNAR(1,1,2) mean one non-lagged valuem and maybe 1 seasonal lagged value, and two nodes
2-2-1 mean two inputs (one non-seasonal lag and the original data), and two nodes, 9 weights means 9 regression coeffients going to the hidden nodes
Feed forward model I am assuming a sigmoid function for hidden layers.
```{r}
nn_auto = nnetar(CIN_revenue_dat_unit, lambda = "auto")
summary(nn_auto)
nn_auto
### evaluate accuracy
residuals_nn_auto = data.frame(residuals(nn_auto))
residuals_nn_auto = apply(residuals_nn_auto, 1, mean)
residuals_nn_auto = data.frame(residuals_nn_auto)
ggAcf(residuals(residuals_nn_auto))
ggPacf(residuals(residuals_nn_auto))
Box.test(residuals_nn_auto, type = "Ljung-Box")
summary(ur.kpss(residuals_nn_auto$residuals_nn_auto))

### compare accuracy
accuracy(nn_auto)
accuracy(arima_model)
dm.test(residuals_nn_auto$residuals_nn_auto, residuals(arima_model))


### Forecast
forecast_nn_auto = forecast(nn_auto, PI = TRUE)
forecast_nn_auto
autoplot(forecast_nn_auto)
```

Try neural network with multiple variables
```{r}
nn_auto_dy = nnetar(CIN_revenue_dat_month_ts[,2], xreg = CIN_revenue_dat_month_ts[,3], lambda = "auto")
accuracy(nn_auto_dy)
accuracy(nn_auto)

results_test = list()
n = 1:3
for(i in 1:length(n)){
results_test[[i]] = dm.test(residuals(nn_auto_dy), residuals(nn_auto), h = n[[i]])
}
results_test

```
Trying predicting them using new data
```{r}
summary(CIN_revenue_dat_month_ts[,3:5])
describe(CIN_revenue_dat_month_ts[,3:5])
number_pay = seq(from= 5680, to= (5680+1200), by = 100)
number_pay_1 = rep(5680, 12)

forecast_model_nn_dy = forecast(nn_auto_dy, xreg = number_pay, PI = TRUE)
summary(forecast_model_nn_dy)
autoplot(forecast_model_nn_dy)



forecast_model_nn_dy_1 = forecast(nn_auto_dy, xreg = number_pay_1, PI = TRUE)
summary(forecast_model_nn_dy_1)
autoplot(forecast_model_nn_dy_1)

```
Try dynamic simulations
```{r}
library(dynsim)
data(grunfeld, package = "dynsim")
grunfeld
library(DataCombine)
grunfeld <- slide(grunfeld, Var = "invest", GroupVar = "company", TimeVar = "year", NewVar = "InvestLag")
grunfeld

CIN_revenue_dat_month_dy = slide(CIN_revenue_dat_month, Var = "revenue")
colnames(CIN_revenue_dat_month_dy)[6] = c("revuneMinus1")
model_1 = lm(revenue ~ revuneMinus1 + number_pay, data = CIN_revenue_dat_month_dy)
summary(model_1)

Scen1 <- data.frame(number_pay = quantile(CIN_revenue_dat_month_dy$number_pay, .95),revuneMinus1 =  mean(CIN_revenue_dat_month_dy$revuneMinus1, na.rm = TRUE))

Scen2 <- data.frame(number_pay = quantile(CIN_revenue_dat_month_dy$number_pay, .5),revuneMinus1 =  mean(CIN_revenue_dat_month_dy$revuneMinus1, na.rm = TRUE))

Scen3 <- data.frame(number_pay = quantile(CIN_revenue_dat_month_dy$number_pay, .05),revuneMinus1 =  mean(CIN_revenue_dat_month_dy$revuneMinus1, na.rm = TRUE))

ScenComb <- list(Scen1, Scen2, Scen3)

Sim1 <- dynsim(obj = model_1, ldv = "revuneMinus1", scen = ScenComb, n = 12)

dynsimGG(Sim1)
```




Need to go back and use test and validation and figure that out.




##################################################################################
Extra
##################################################################################
Try CARET model
Not enough data
```{r}
library(caret)

inTrain = createDataPartition(y = CIN_revenue_dat_month$time, p = .75, list = FALSE)
training = CIN_revenue_dat_month[inTrain,]
testing = CIN_revenue_dat_month[-inTrain,] 

fitControl <- trainControl(
  method = "repeatedcv",
  number = 1,
  repeats = 10)

gbmFit1 <- train(revenue ~ ., data = training, 
                 method = "gbm", 
                 trControl = fitControl,
                 verbose = FALSE)


```

Try and plot nnetar
Doesn't seem to work.
```{r}
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

plot.nnet(nn_auto)

nn_auto

```


