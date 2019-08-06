---
  title: "Test"
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

library(lubridate)
library(forecast)
library(ggplot2)
library(stringr)
library(fpp2)
head(CIN_revenue)
colnames(CIN_revenue)[2] = "location"
head(CIN_revenue)
CIN_revenue_dat = CIN_revenue
CIN_revenue_dat$Year.Month = str_replace_all(CIN_revenue_dat$Year.Month, c("Jan" = "1", "Feb"="2", "Mar"="3", "Apr"="4", "May"="5", "Jun"="6", "Jul"="7", "Aug"="8", "Sep" = "9", "Oct"="10", "Nov"="11", "Dec"="12"))
CIN_revenue_dat$Year.Month = paste("1-",CIN_revenue_dat$Year.Month, sep = "")
CIN_revenue_dat$Year.Month = dmy(CIN_revenue_dat$Year.Month)
```
Now aggregate data by month
```{r}
head(CIN_revenue_dat)
CIN_revenue_dat_month = data.frame(date =  CIN_revenue_dat$Year.Month, revenue = CIN_revenue_dat$Payments)
CIN_revenue_dat_month$revenue = as.numeric(CIN_revenue_dat_month$revenue)


CIN_revenue_dat_month = aggregate(.~date, data = CIN_revenue_dat_month, sum)
head(CIN_revenue_dat_month)
colnames(CIN_revenue_dat_month)[1:2] = c("date", "revenue")

head(CIN_revenue_dat_month)
dim(CIN_revenue_dat_month)
```
Regularly spaced time series.  Which interval should we forecast and what that will help us plan for? Mixed model in time has covariates.
Convert to TS object
Money in thousands 
$31,000 means 31 million
```{r}
CIN_revenue_dat_month$revenue = CIN_revenue_dat_month$revenue 
typeof(CIN_revenue_dat_month$date)


CIN_revenue_dat_month$date = NULL

CIN_revenue_dat_month_ts = ts(CIN_revenue_dat_month, start = c(2017, 7), end = c(2019,6), frequency = 12)
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
gglagplot(CIN_revenue_dat_month)

```
Autcorrelation not correlation with the data points
Why do you want to get rid of the trend?  
  Need to account for the days in each month.  So you get the daily average, which accounts for the number of das

Could adjust for three things
Different number of days in each month
Population adjustments
Inflation adjustments

Two time series assumptions
Mean of residuals is zero if not then add the mean of the residuals to all forcasts
Residuals are uncorrelated
Important but not necessary
Residuals have constant var
Residuals normal
```{r}
ggAcf(CIN_revenue_dat_month_ts)
dim(CIN_revenue_dat_month)
2/sqrt(25)

## If you want to adjust for the days of the year
#CIN_revenue_dat_month_ts / monthdays(CIN_revenue_dat_month_ts)
```
Need to subset the data for training and 20% for testing
Forecast errors are for testing and residuals are for training
Can add a quarter variable to help prediction 

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

CIN_revenue_dat_month_ts = ts(CIN_revenue_dat_month, start = c(2017, 7), end = c(2019,6), frequency = 12)
head(CIN_revenue_dat_month_ts)
CIN_revenue_dat_month_ts
```
Linear regression
If there is an outlier we know what the date is and we think it is something werid with that date, then create a dummy variable for that date.  So we may not have good information on July yet.
```{r}
lm_ts = tslm(revenue ~ number_pay + time + quarter, data = CIN_revenue_dat_month_ts)
summary(lm_ts)
checkresiduals(lm_ts)
library(lmtest)
library(car)
outlierTest(lm_ts)
bptest(lm_ts)
```
Use the model not just to predict, but to predict based on different scenerios
This would be where we use the test data set, which is five percent so 5 values.
```{r}
new_dat = data.frame(revenue = rnorm(n = 12, mean = 32000000, sd = 100000), quarter = rep(1:4, 3), number_pay = rnorm(n = 12, mean = 45368, sd = 100000), time = 25:36)

predict_lm_ts = forecast(lm_ts, newdata = new_dat)
predict_lm_ts
```
If you are interested in sesaon, then do not get rid of it. 
To estimate the trend component, moving averages were popular.
```{r}
ma(CIN_revenue_dat_month_ts, 5)
```
Plot the data, the trend, and the seasonal adjusted
Season adjustment means figuring how of the season (maybe a year) what is the ratio between the average sales or mean centered sales around a month to the actual month.  For example, if the average sales is .85 of the average for the year in a particular month, then you divide by .85 and increase the sales.

Trend is the overall long term pattern while season is a predictable cycle
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
Stationary means no trends and no seasons data points should be related to each other, no predictable patterns in the data
Test station
A differenced series is the change between consecutive time points.  So the second observation of revenue is the difference between the second observation and the first observation.
```{r}
library(urca)
Box.test(CIN_revenue_dat_month$revenue, lag= 4, type = "Ljung-Box")
summary(ur.kpss(CIN_revenue_dat_month$revenue))
```
Autoregressive model is like a lagged version of a linear model with the previous time point values as predictors.  So the value at time point 2, 3,..n predicting time point one?
P is the autoregressive parameter, which is the number of time points we go back to so we include each time point minus the previous as an average parameter if AR2 then each time point and first and second difference.

C = difference plus error

Moving average model uses the errors to predict future values
```{r}
CIN_revenue_dat_unit
auto.arima(CIN_revenue_dat_month_ts)
```




