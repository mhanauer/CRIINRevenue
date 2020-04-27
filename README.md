---
  title: "Times Series with Revenue"
output: html_document
---
######################
Cleaning the data 
######################
July 2017 through March 2020
```{r}
## Revenue data
setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/SustainWorkshop/RevenueAnalysis")
CIN_revenue = read.csv("cin_revenue_4_26_20.csv", header = TRUE)

head(CIN_revenue)
library(lubridate)
library(forecast)
library(ggplot2)
library(stringr)
library(fpp2)
library(urca)
library(psych)
library(prettyR)
library(tidyr)
library(dplyr)
library(forecast)
head(CIN_revenue)
head(CIN_revenue)
CIN_revenue_dat = CIN_revenue
head(CIN_revenue_dat)
#Replace the month with numbers
CIN_revenue_dat$Year.Month = str_replace_all(CIN_revenue_dat$Service.Month, c("Jan" = "1", "Feb"="2", "Mar"="3", "Apr"="4", "May"="5", "Jun"="6", "Jul"="7", "Aug"="8", "Sep" = "9", "Oct"="10", "Nov"="11", "Dec"="12"))
head(CIN_revenue_dat)
CIN_revenue_dat$Service.Month = NULL
#We do not have a complete date.  Need to add the day just assuming that day is the 1st for each data point.
CIN_revenue_dat$Year.Month = paste("1-",CIN_revenue_dat$Year.Month, sep = "")
head(CIN_revenue_dat)
CIN_revenue_dat$Year.Month = dmy(CIN_revenue_dat$Year.Month)
head(CIN_revenue_dat)

CIN_revenue_dat$Payments = gsub("\\D", "", CIN_revenue_dat$Payments)

## Need to divide by 100 to get rid of cents also it rounds the amount
CIN_revenue_dat$Payments = as.numeric(CIN_revenue_dat$Payments)/100
## Subset just the data you want payments and Year.Month
CIN_revenue_dat = CIN_revenue_dat[c("Payments", "Year.Month")]
CIN_revenue_dat

####Get the quarter variable
CIN_revenue_dat$Quarter = ifelse(CIN_revenue_dat$Year.Month < "2017-10-01", "Q3_2017", ifelse(CIN_revenue_dat$Year.Month < "2018-01-01", "Q4_2017", ifelse(CIN_revenue_dat$Year.Month < "2018-04-01","Q1_2018", ifelse(CIN_revenue_dat$Year.Month < "2018-07-01", "Q2_2018", ifelse(CIN_revenue_dat$Year.Month < "2018-10-01", "Q3_2018", ifelse(CIN_revenue_dat$Year.Month < "2019-01-01", "Q4_2018", ifelse(CIN_revenue_dat$Year.Month < "2019-04-01", "Q1_2019", ifelse(CIN_revenue_dat$Year.Month < "2019-07-01", "Q2_2019", ifelse(CIN_revenue_dat$Year.Month < "2019-10-01", "Q3_2019", ifelse(CIN_revenue_dat$Year.Month < "2020-01-01", "Q4_2019", ifelse(CIN_revenue_dat$Year.Month < "2020-04-01", "Q1_2020")))))))))))
describe.factor(CIN_revenue_dat$Quarter)
head(CIN_revenue_dat)
### Now generate the and create inflation based revenue
#110.697  Inflation rates found in same folder were data is kept
CIN_revenue_dat$inflation = ifelse(CIN_revenue_dat$Quarter == "Q3_2017", (110.697/105.938), ifelse(CIN_revenue_dat$Quarter == "Q4_2017", (110.697/106.646), ifelse(CIN_revenue_dat$Quarter == "Q1_2018", (110.697/107.03), ifelse(CIN_revenue_dat$Quarter == "Q2_2018", (110.697/107.672), ifelse(CIN_revenue_dat$Quarter == "Q3_2018", (110.697/108.045), ifelse(CIN_revenue_dat$Quarter == "Q4_2018", (110.697/108.61), ifelse(CIN_revenue_dat$Quarter == "Q1_2019", (110.697/108.949), ifelse(CIN_revenue_dat$Quarter == "Q2_2019", (110.697/109.51),ifelse(CIN_revenue_dat$Quarter == "Q3_2019", (110.697/110.11), 1)))))))))
describe.factor(CIN_revenue_dat$inflation)
CIN_revenue_dat$inflation = as.numeric(CIN_revenue_dat$inflation)
CIN_revenue_dat$Payments = as.numeric(CIN_revenue_dat$Payments)
CIN_revenue_dat$Payments = CIN_revenue_dat$Payments*CIN_revenue_dat$inflation

head(CIN_revenue_dat)
CIN_revenue_dat$Quarter = NULL
CIN_revenue_dat$inflation = NULL
```
############################ 
Plotting revenue by month
Data cleaning 
############################
```{r}
head(CIN_revenue_dat)
describe.factor(CIN_revenue_dat$Year.Month)
CIN_revenue_dat_month = data.frame(date =  CIN_revenue_dat$Year.Month, revenue = CIN_revenue_dat$Payments)
CIN_revenue_dat_month$revenue = as.numeric(CIN_revenue_dat_month$revenue)
head(CIN_revenue_dat_month)

## Get rid of date
CIN_revenue_dat_month$date = NULL
dim(CIN_revenue_dat_month)
head(CIN_revenue_dat_month)
CIN_revenue_dat_month$time = 1:length(CIN_revenue_dat_month$revenue)
CIN_revenue_dat_month_ts = ts(CIN_revenue_dat_month, start = c(2017, 7), end = c(2020,3), frequency = 12)
head(CIN_revenue_dat_month_ts)

### take care of different days in different months
CIN_revenue_daily_average_ts =  CIN_revenue_dat_month_ts /  monthdays(CIN_revenue_dat_month_ts)
CIN_revenue_daily_average_ts
```
Regression
```{r}
fit_revenue = tslm(revenue ~ time, data = CIN_revenue_dat_month_ts)
summary(fit_revenue)
hist(fit_revenue$residuals)
newdata = data.frame(time = c(34:37))
fcast_revenue = forecast(fit_revenue, newdata = newdata)
autoplot(fcast_revenue)
```



################
Plotting revenue by month
Analysis
################

##############
Notes
##############
Plot the data over time
Trend: long term increase or increase; Season = short term predictable with some patteron; cyclic: 1 to 2 year trend correlated with some economic event
```{r}
library(ggplot2)
ggseasonplot(CIN_revenue_dat_month_ts) +
  ylab("$ Millions")+
  ggtitle("Figure 1: Net revenue per year")+labs(color='Year')+
   scale_y_continuous(labels = scales::dollar)
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
Partial autocorrelation can help identify number of lags (p) needed.

https://people.duke.edu/~rnau/411arim3.htm

Box.test: https://stat.ethz.ch/R-manual/R-devel/library/stats/html/box.test.html

ur.kpss: Whether there some consent trend or a constent with a linear component that can be explained  
```{r}
auto_cor=  ggAcf(CIN_revenue_dat_month_ts[,1], type = "correlation")
auto_cor$data
part_auto_cor =  ggAcf(CIN_revenue_dat_month_ts[,1], type = "partial")
part_auto_cor
part_auto_cor$data
### More tests for autocorrelation

Box.test(CIN_revenue_dat_month_ts, type = "Ljung-Box")
summary(ur.kpss(CIN_revenue_dat_month_ts))
```
#############################
Notes
#############################
Autoregressive model is like a lagged version of a linear model with the previous time point values as predictors.  So the value at time point 2, 3,..n predicting time point one?
P is the autoregressive parameter, which is the number of time points we go back to so we include each time point minus the previous as an average parameter if AR2 then each time point and first and second difference.

C = difference plus error

Moving average model uses the errors to predict future values

p = number of lags or previous y data points to predict
q = "moving average" is about the residuals as predictions of different time points
d = degree of difference in y

Order = (p,d,q)

Maybe phi is the average correlation between all time points and the difference according p (all t minus 1's).  For example, p = 1 is the all t minus 1's between. Phi is the average correlation between t and minus 1's.

Testing versus training: One shot testing with small amounts of data: https://otexts.com/fpp2/forecasting-on-training-and-test-sets.html


Box-Cox set to auto to find a transformation of the data that makes it normal if no change is needed defaults to data.

```{r}

head(CIN_revenue_dat_month_ts)

CIN_revenue_dat_month_ts_unit= CIN_revenue_dat_month_ts[,1]

### Training versus testing about 20% in testing
CIN_revenue_dat_month_ts_unit_training = CIN_revenue_dat_month_ts_unit[1:18]
CIN_revenue_dat_month_ts_unit_test =  CIN_revenue_dat_month_ts_unit[19:33]
#### Model for both
arima_model_train =  nnetar(CIN_revenue_dat_month_ts_unit_training)

arima_model_test = nnetar(CIN_revenue_dat_month_ts_unit_test, model = arima_model_train)
accuracy(arima_model_test) 

f.model = forecast(arima_model_test, h = 4, PI = TRUE)
autoplot(f.model)
### Test assumptions
ggAcf(residuals(arima_model_train))
ggPacf(residuals(arima_model_train))
Box.test(residuals(arima_model_train), type = "Ljung-Box")
predict_residuals = residuals(arima_model_train)
hist(predict_residuals)

library(urca)
lag_n_short = c(2:10)
mean_station_short = list()
for(i in 1:length(lag_n_short)){
mean_station_short[[i]]  =  ur.kpss(predict_residuals, type="tau", use.lag
 = lag_n_short[[i]])
mean_station_short[[i]] = summary(mean_station_short[[i]])
}
mean_station_short


lag_n_short = c(2:10)
trend_station_short = list()
for(i in 1:length(lag_n_short)){
trend_station_short[[i]]  =  ur.kpss(predict_residuals, type="mu", use.lag
 = lag_n_short[[i]])
trend_station_short[[i]] = summary(trend_station_short[[i]])
}
trend_station_short

```
Now review test model
```{r}
data("auscafe")
auscafe
training = subset(auscafe, end = length(auscafe)-61)

### Forcast model
forecast_model = forecast(arima_model_train, CIN_revenue_dat_month_ts_unit_test, h = 4)
summary(forecast_model)
autoplot(forecast_model)

autoplot(forecast_model)+
  labs(title = "Figure 2: Forecasts for CIN Bloomington June 2019 to June 2020", y = "$ Millions in revenue per month", x = "Year")+
 #scale_x_date(breaks= as.Date(c("2017-05-01", "2018-01-01", "2018-06-01", "2019-01-01", "2019-05-01", "2020-01-01")), labels = date_format("%m/%Y"))+
  labs(color='level')+
   scale_y_continuous(labels = scales::dollar)

```


Establishing accuracy
```{r}
CIN_revenue_dat_unit =  CIN_revenue_dat_month_ts
head(CIN_revenue_dat_unit)


### Training versus testing
CIN_revenue_dat_unit_train = CIN_revenue_dat_unit[1:17]
1-(17/23)
CIN_revenue_dat_unit_test = CIN_revenue_dat_unit[18:23]
#### Model for both
nnetar_model_train =  nnetar(CIN_revenue_dat_unit_train)
accuracy(nnetar_model_train)
nnetar_model_test = nnetar(CIN_revenue_dat_unit_test, model = nnetar_model_train)
accuracy(nnetar_model_test)

### Forcast model
forecast_model = forecast(nnetar_model,  h = 12)
summary(forecast_model)
autoplot(forecast_model)

```



