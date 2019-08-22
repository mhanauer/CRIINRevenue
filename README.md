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

Use box test to see if the times series is independence
Acf to see if there is significant auto-correlation at different lags (so lag 1 means the relationship between each time point and the one before it) can also be used for residuals
Identifies if the time series is stationary tests whether the trend is not constant or not a linear trend.
```{r}
ggAcf(CIN_revenue_dat_month_ts, type = "correlation")
ggAcf(CIN_revenue_dat_month_ts, type = "partial")

dim(CIN_revenue_dat_month)
2/sqrt(23)

### More tests for autocorrelation
library(urca)
Box.test(CIN_revenue_dat_month$revenue, type = "Ljung-Box")
summary(ur.kpss(CIN_revenue_dat_month$revenue))

## If you want to adjust for the days of the year
#CIN_revenue_dat_month_ts / monthdays(CIN_revenue_dat_month_ts)
```
########################
Running regression with multiple variables
########################

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
CIN_revenue_dat_month_ts


```
Linear regression
If there is an outlier we know what the date is and we think it is something werid with that date, then create a dummy variable for that date.  So we may not have good information on July yet.

Hetero: the variance of y should be the same as the variance of the errors

bptest is heterosked where is regresses the residuals on the variables and evaluates if those variables explain too much of the variance in the residuals 
```{r}
lm_ts = tslm(revenue ~ number_pay + time + quarter, data = CIN_revenue_dat_month_ts)
summary(lm_ts)
checkresiduals(lm_ts)
library(lmtest)
library(car)
outlierTest(lm_ts)
bptest(lm_ts)
CV(lm_ts)
```
Use the model not just to predict, but to predict based on different scenerios
This would be where we use the test data set, which is five percent so 5 values.

Use third quatile of the number of payments to see what would have happen if we can increase the number of payments.
58293
```{r}
summary(CIN_revenue_dat_month_ts[,3:5])

new_dat = data.frame(quarter = rep(1:4, 3), number_pay = rnorm(n = 12, mean = 58293, sd = 5000), time = 25:36)

predict_lm_ts = forecast(lm_ts, newdata = new_dat)
predict_lm_ts
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
checkresiduals(arima_model)
#ggAcf()

forecast_model = forecast(arima_model)
summary(forecast_model)
plot(forecast_model)


```
Prove differening
```{r}
CIN_revenue_dat_month_ts[,2]
32361367 - 31926558
diff(CIN_revenue_dat_month_ts[,2], lag = 2)
```


Try autoregressive model with predictors
```{r}
CIN_revenue_dat_month_ts[,2]
CIN_revenue_dat_month_ts[,5]

arima_model_dy =  auto.arima(CIN_revenue_dat_month_ts[,2], xreg = CIN_revenue_dat_month_ts[,3:5], seasonal = FALSE)
summary(arima_model_dy)
checkresiduals(arima_model_dy)

#ggAcf()
forecast_model_dy = forecast(arima_model_dy, xreg = new_dat)

summary(forecast_model_dy)
plot(forecast_model_dy)
autoplot(forecast_model_dy)

```
Compare dynamic and non-dynamic model
So the extra variables are not related to revenue so changing them won't really matter
```{r}
accuracy(arima_model_dy)
accuracy(arima_model)
dm.test(residuals(forecast_model_dy), residuals(forecast_model), h =1, alternative = "less")
dm.test(arima_model_dy, arima_model)

```
Try neural network feed forward model
nnetar
dm.test
So the predictions are the exact same for each month
```{r}
nn_auto = nnetar(CIN_revenue_dat_unit)
summary(nn_auto)
nn_auto
forecast_nn_auto = forecast(nn_auto, h = 12)
summary(forecast_nn_auto)
autoplot(forecast_nn_auto)

accuracy(nn_auto)
accuracy(arima_model)
dm.test(residuals(nn_auto), residuals(arima_model), h = 1, alternative = "less")

### Dynamic
new_dat_ex = data.frame(quarter = rep(1:4, 3), number_pay = rnorm(n = 12, mean = 10000, sd = 1000), time = 25:36)
nn_auto_dy = nnetar(CIN_revenue_dat_month_ts[,2], xreg = CIN_revenue_dat_month_ts[,3:5])
summary(nn_auto_dy)
nn_auto_dy
accuracy(nn_auto_dy)
accuracy(nn_auto)
dm.test(residuals(nn_auto_dy), residuals(nn_auto), h = 1, alternative = "less")

forecast_nn_auto_dy = forecast(nn_auto_dy, xreg = new_dat, h = 12)
forecast_nn_auto_dy
autoplot(forecast_nn_auto_dy)

```

No predict


#######################################
Next steps
########################################
Cross validation
Maybe...
Different number of days in each month
Population adjustments
Inflation adjustments


Need to use some type of cross validation if possible
tsCV is cross validation moving forward in time.  So attempts to forecast with less and less data.
If not normal use bootstrap, which takes the prediction but add an unknown error that is based on the distribution of the actual residuals.
```{r}
attach(goog)
e = tsCV(goog200, naive, h = 8)
tail(e, 50)
test_CV = tsCV(CIN_revenue_dat_month_ts, rwf, drift = TRUE, h = 8)
head(test_CV)
naive(CIN_revenue_dat_month_ts)
autoplot(naive(CIN_revenue_dat_month_ts))
forecast(CIN_revenue_dat_month_ts, h = 12)

(18500- 13030)/13030
```
##########################################################################################################
Extra code that might be useful
##########################################################################################################

```{r}

CIN_revenue_dat_month_ts_decomp = decompose(CIN_revenue_dat_month_ts, type = "additive")
CIN_revenue_dat_month_ts_decomp
trendcycle(CIN_revenue_dat_month_ts_decomp)

autoplot(CIN_revenue_dat_month_ts_decomp, series = "Data")+
  autolayer(trendcycle(CIN_revenue_dat_month_ts_decomp))

#Try SEATS method not enough data for a seasonal adjustment
library(seasonal)
CIN_revenue_dat_month_ts %>% seas() %>%
  autoplot()

## STL method cannot have covariates in this method
fit = stl(CIN_revenue_dat_month_ts, t.window = 4, s.window = "periodic", robust = TRUE)

```
If you are interested in sesaon, then do not get rid of it. 
To estimate the trend component, moving averages were popular.
```{r}
ma(CIN_revenue_dat_month_ts, 5)
```
Plot the data, the trend, and the seasonal adjusted
Season adjustment means figuring how of the season (maybe a year) what is the ratio between the average sales or mean centered sales around a month to the actual month.  For example, if the average sales is .85 of the average for the year in a particular month, then you divide by .85 for the month each year increasing the sales.

