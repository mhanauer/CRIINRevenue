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

dim(CIN_revenue_dat_month)
2/sqrt(23)

### More tests for autocorrelation

Box.test(CIN_revenue_dat_month$revenue, type = "Ljung-Box")
summary(ur.kpss(CIN_revenue_dat_month$revenue))

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
CIN_revenue_dat_month_ts
```



Use the model not just to predict, but to predict based on different scenerios
This would be where we use the test data set, which is five percent so 5 values.

Use third quatile of the number of payments to see what would have happen if we can increase the number 
of payments.
```{r}
summary(CIN_revenue_dat_month_ts[,3:5])

new_dat = data.frame(quarter = rep(1:4, 3), number_pay = rnorm(n = 12, mean = 58293, sd = 5000), time = 25:36)
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


Try autoregressive model with predictors
```{r}
CIN_revenue_dat_month_ts[,2]
CIN_revenue_dat_month_ts[,5]

arima_model_dy =  auto.arima(CIN_revenue_dat_month_ts[,2], xreg = CIN_revenue_dat_month_ts[,3:5], seasonal = FALSE)
summary(arima_model_dy)
summary(arima_model)
#ggAcf()
forecast_model_dy = forecast(arima_model_dy, xreg = new_dat)

summary(forecast_model_dy)
plot(forecast_model_dy)
autoplot(forecast_model_dy)

```
Try neural network feed forward model

nnetar
dm.test
```{r}
nn_auto = nnetar(CIN_revenue_dat_unit)
summary(nn_auto)
nn_auto
### evaluate accuracy
ggAcf(residuals(nn_auto))
ggPacf(residuals(nn_auto))
Box.test(residuals(nn_auto), type = "Ljung-Box")
summary(ur.kpss(residuals(nn_auto)))

### compare accuracy
accuracy(nn_auto)
accuracy(arima_model)
dm.test(residuals(nn_auto), residuals(arima_model))


### Forcast
forecast_nn_auto = forecast(nn_auto, PI = TRUE)
forecast_nn_auto
summary(forecast_nn_auto)
```






