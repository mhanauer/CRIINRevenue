---
title: "Test"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Review revenue.  Get date in mdy format.  Need to change Jul to correct number and add day (1)
```{r}
library(lubridate)
library(forecast)
library(ggplot2)
head(CIN_revenue)
colnames(CIN_revenue)[2] = "location"
head(CIN_revenue)
CIN_revenue_dat = CIN_revenue
CIN_revenue_dat$Year.Month = str_replace_all(CIN_revenue_dat$Year.Month, c("Jan" = "1", "Feb"="2", "Mar"="3", "Apr"="4", "May"="5", "Jun"="6", "Jul"="7", "Aug"="8", "Sep" = "9", "Oct"="10", "Nov"="11", "Dec"="12"))
CIN_revenue_dat$Year.Month = paste("1-",CIN_revenue_dat$Year.Month, sep = "")
test
CIN_revenue_dat$Year.Month = dmy(CIN_revenue_dat$Year.Month)
```
Now aggregate data by month
```{r}
head(CIN_revenue_dat)
CIN_revenue_dat_month = data.frame(date =  CIN_revenue_dat$Year.Month, revenue = CIN_revenue_dat$Payments)
CIN_revenue_dat_month$revenue = as.numeric(CIN_revenue_dat_month$revenue)
CIN_revenue_dat_month = aggregate(x = CIN_revenue_dat_month$revenue, by = list(CIN_revenue_dat_month$date), FUN = sum)
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


gg = ggplot(CIN_revenue_dat_month, aes(x = date, y = revenue))+
  geom_line(aes())+
  ggtitle("CIN Revenue")
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
library(fpp2)
## If you want to adjust for the days of the year
CIN_revenue_dat_month_ts / monthdays(CIN_revenue_dat_month_ts)
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



