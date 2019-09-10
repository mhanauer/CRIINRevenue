---
  title: "Times Series with Revenue"
output: html_document
---

Review revenue.  Get date in mdy format.  Need to change Jul to correct number and add day (1)

See PCE inflation rate for rates
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
head(CIN_revenue_dat)

####Get the quarter variable
CIN_revenue_dat$Quarter = ifelse(CIN_revenue_dat$Year.Month < "2017-12-01", "Q4_2017", ifelse(CIN_revenue_dat$Year.Month < "2018-03-01","Q1_2018", ifelse(CIN_revenue_dat$Year.Month < "2018-6-01", "Q2_2018", ifelse(CIN_revenue_dat$Year.Month < "2018-9-01", "Q3_2018", ifelse(CIN_revenue_dat$Year.Month < "2018-12-01", "Q4_2018", ifelse(CIN_revenue_dat$Year.Month < "2019-03-01", "Q1_2019", ifelse(CIN_revenue_dat$Year.Month < "2019-06-01", "Q2_2019", ifelse(CIN_revenue_dat$Year.Month < "2019-09-01", "Q3_2019", ifelse(CIN_revenue_dat$Year.Month < "2019-12-01", "Q4_2019", "Wrong"))))))))) 
describe.factor(CIN_revenue_dat$Quarter)

### Now generate the and create inflation based revenue
#109.472
CIN_revenue_dat$inflation = ifelse(CIN_revenue_dat$Quarter == "Q4_2017", (109.472/106.646), ifelse(CIN_revenue_dat$Quarter == "Q1_2018", (109.472/107.03), ifelse(CIN_revenue_dat$Quarter == "Q2_2018", (109.472/107.672), ifelse(CIN_revenue_dat$Quarter == "Q3_2018", (109.472/108.045), ifelse(CIN_revenue_dat$Quarter == "Q4_2018", (109.472/108.61), ifelse(CIN_revenue_dat$Quarter == "Q1_2019", (109.472/108.949), ifelse(CIN_revenue_dat$Quarter == "Q2_2019", 1,"Wrong")))))))
describe.factor(CIN_revenue_dat$inflation)
CIN_revenue_dat$inflation = as.numeric(CIN_revenue_dat$inflation)

CIN_revenue_dat$Payments = CIN_revenue_dat$Payments*CIN_revenue_dat$inflation

head(CIN_revenue_dat)
CIN_revenue_dat$Quarter = NULL
CIN_revenue_dat$inflation = NULL
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
CIN_revenue_dat_unit =  CIN_revenue_dat_month_ts
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

CIN_revenue_dat_unit =  CIN_revenue_dat_month_ts
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
#dm.test(residuals_nn_auto$residuals_nn_auto, residuals(arima_model))

### Forecast
forecast_nn_auto = forecast(nn_auto, PI = TRUE, h = 12)
forecast_nn_auto

autoplot(forecast_nn_auto)+
  labs(title = "Figure 2: Forecasts for CIN Bloomington June 2019 to June 2020", y = "$ Millions in revenue per month", x = "Year")

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
Get the total amount per revenue finance class
```{r}
head(CIN_revenue_dat)
CIN_revenue_desc = CIN_revenue_dat[,c(2,4:5)]
head(CIN_revenue_desc)
CIN_revenue_desc_total = aggregate(.~Financial.Class.Value
, data = CIN_revenue_desc, sum)
CIN_revenue_desc_mean = aggregate(.~Financial.Class.Value
, data = CIN_revenue_desc, mean)
CIN_revenue_desc_sd = aggregate(.~Financial.Class.Value
, data = CIN_revenue_desc, sd)

CIN_revenue_desc_all = data.frame(finacial_class = CIN_revenue_desc_total$Financial.Class.Value, mean_rev = CIN_revenue_desc_mean$Payments, total_rev = CIN_revenue_desc_total$Payments, sd_rev = CIN_revenue_desc_sd$Payments)
CIN_revenue_desc_all = CIN_revenue_desc_all[order(-CIN_revenue_desc_all$mean_rev),]
#CIN_revenue_desc_all[is.na(CIN_revenue_desc_all)] = 0
CIN_revenue_desc_all = mutate_if(CIN_revenue_desc_all, is.numeric, round)
CIN_revenue_desc_all
```

Just present a regression model and demonstrate that only Mediciad and HIP and Commerical are actually significantly predicting revenue
```{r}
model_9 = lm(revenue ~Commercial +Medicaid_HIP+MRO, data = CIN_revenue_dat_month_dy)
checkresiduals(model_9)
hist(residuals(model_9))
bptest(model_9)
qqnorm(residuals(model_9))
library(car)
vif(model_9)
sum_model_9 = summary(model_9)
sum_model_9
confint(model_9)
### Run a Bayesian model
library(rstanarm)
bayes_model =  stan_glm(revenue ~Commercial +Medicaid_HIP+MRO, data = CIN_revenue_dat_month_dy, family = gaussian(link = "identity"))
median(bayes_R2(bayes_model))
#launch_shinystan(bayes_model)
summary(bayes_model)
```



##############################################################
Extra code
##############################################################
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
model_9_rev = lm(revenue ~revuneMinus1+Commercial +Medicaid_HIP, data = CIN_revenue_dat_month_dy)
model_9_mro = lm(revenue ~Commercial +Medicaid_HIP+MRO, data = CIN_revenue_dat_month_dy)
AIC(model_9_rev)
AIC(model_9_mro)
BIC(model_9_rev)
BIC(model_9_mro)
summary(model_9_mro)
summary(model_9_rev)
```
Looking into guarentor, but lot's variation in the top not sure if we need to get that specific
```{r}
### Do this by guarnter
CIN_revenue_guar = CIN_revenue_dat[,c(3:5)]
head(CIN_revenue_guar)
CIN_revenue_desc_sum_guar = aggregate(.~Guarantor, data = CIN_revenue_guar, sum)
CIN_revenue_desc_sd_guar = aggregate(.~Guarantor, data = CIN_revenue_guar, sd)
CIN_revenue_desc_mean_guar = aggregate(.~Guarantor, data = CIN_revenue_guar, mean)
CIN_revenue_desc_agg_guar = data.frame(Guarantor = CIN_revenue_desc_mean_guar$Guarantor,mean_rev = CIN_revenue_desc_mean_guar$Payments, total_rev = CIN_revenue_desc_sum_guar$Payments, sd_rev = CIN_revenue_desc_sd_guar$Payments)
CIN_revenue_desc_agg_guar = CIN_revenue_desc_agg_guar[order(-CIN_revenue_desc_agg_guar$mean_rev),]
CIN_revenue_desc_agg_guar = mutate_if(CIN_revenue_desc_agg_guar,is.numeric, ~round(., 3))
CIN_revenue_desc_agg_guar
```



##############################

Test sample dat set to if the forecasts are similar yes nnetar is much better
```{r}
n_net = nnetar(sunspotarea)
n_net_plot = autoplot(forecast(n_net, PI = TRUE))
arima_model =  auto.arima(sunspotarea, seasonal = FALSE)
arima_plot = autoplot(forecast(arima_model))
accuracy(arima_model)
accuracy(n_net)
```

