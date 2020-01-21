---
  title: "Times Series with Revenue"
output: html_document
---
######################
Cleaning the data 
######################
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
library(psych)
library(prettyR)
library(tidyr)
library(dplyr)
head(CIN_revenue)
head(CIN_revenue)
CIN_revenue_dat = CIN_revenue
head(CIN_revenue_dat)
#Replace the month with numbers
CIN_revenue_dat$Year.Month = str_replace_all(CIN_revenue_dat$Year.Month, c("Jan" = "1", "Feb"="2", "Mar"="3", "Apr"="4", "May"="5", "Jun"="6", "Jul"="7", "Aug"="8", "Sep" = "9", "Oct"="10", "Nov"="11", "Dec"="12"))
head(CIN_revenue_dat)
#We do not have a complete date.  Need to add the day just assuming that day is the 1st for each data point.
CIN_revenue_dat$Year.Month = paste("1-",CIN_revenue_dat$Year.Month, sep = "")
head(CIN_revenue_dat)
CIN_revenue_dat$Year.Month = dmy(CIN_revenue_dat$Year.Month)
head(CIN_revenue_dat)
### Remove the location we will not have this later on
CIN_revenue_dat$billing_tx_history.program_value = NULL
head(CIN_revenue_dat)
#### Need to remove the $ we are removing the .00 so divide by 100 to get original payment
#### Then divide by 100 to get estimates in the hundreds of thousands
CIN_revenue_dat$Payments = gsub("\\D", "", CIN_revenue_dat$Payments)

CIN_revenue_dat$Payments = as.numeric(CIN_revenue_dat$Payments) 
CIN_revenue_dat$Payments = CIN_revenue_dat$Payments / 100
### Get rid of anything before May
CIN_revenue_dat = subset(CIN_revenue_dat, Year.Month < "2019-06-01")
sum(CIN_revenue_dat$Year.Month == "2019-07-01")
### Get rid of non-recoverable
CIN_revenue_dat = subset(CIN_revenue_dat, Financial.Class.Value != "Non-Recoverable")
describe.factor(CIN_revenue_dat$Financial.Class.Value)
#### Get rid of any revenue that is zero
CIN_revenue_dat = subset(CIN_revenue_dat, Payments > 0)
sum(CIN_revenue_dat$Payments <= 0)
head(CIN_revenue_dat)
####Get the quarter variable
CIN_revenue_dat$Quarter = ifelse(CIN_revenue_dat$Year.Month < "2017-12-01", "Q4_2017", ifelse(CIN_revenue_dat$Year.Month < "2018-03-01","Q1_2018", ifelse(CIN_revenue_dat$Year.Month < "2018-6-01", "Q2_2018", ifelse(CIN_revenue_dat$Year.Month < "2018-9-01", "Q3_2018", ifelse(CIN_revenue_dat$Year.Month < "2018-12-01", "Q4_2018", ifelse(CIN_revenue_dat$Year.Month < "2019-03-01", "Q1_2019", ifelse(CIN_revenue_dat$Year.Month < "2019-06-01", "Q2_2019", ifelse(CIN_revenue_dat$Year.Month < "2019-09-01", "Q3_2019", ifelse(CIN_revenue_dat$Year.Month < "2019-12-01", "Q4_2019", "Wrong"))))))))) 
describe.factor(CIN_revenue_dat$Quarter)
head(CIN_revenue_dat)
### Now generate the and create inflation based revenue
#109.472  Inflation rates found in same folder were data is kept
CIN_revenue_dat$inflation = ifelse(CIN_revenue_dat$Quarter == "Q4_2017", (109.472/106.646), ifelse(CIN_revenue_dat$Quarter == "Q1_2018", (109.472/107.03), ifelse(CIN_revenue_dat$Quarter == "Q2_2018", (109.472/107.672), ifelse(CIN_revenue_dat$Quarter == "Q3_2018", (109.472/108.045), ifelse(CIN_revenue_dat$Quarter == "Q4_2018", (109.472/108.61), ifelse(CIN_revenue_dat$Quarter == "Q1_2019", (109.472/108.949), ifelse(CIN_revenue_dat$Quarter == "Q2_2019", 1,"Wrong")))))))
describe.factor(CIN_revenue_dat$inflation)
CIN_revenue_dat$inflation = as.numeric(CIN_revenue_dat$inflation)

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

CIN_revenue_dat_month = aggregate(.~date, data = CIN_revenue_dat_month, sum)
head(CIN_revenue_dat_month)
dim(CIN_revenue_dat_month)
CIN_revenue_dat_month$revenue = round((CIN_revenue_dat_month$revenue))


## Get rid of date
CIN_revenue_dat_month$date = NULL
dim(CIN_revenue_dat_month)
head(CIN_revenue_dat_month)
CIN_revenue_dat_month_ts = ts(CIN_revenue_dat_month, start = c(2017, 7), end = c(2019,5), frequency = 12)
head(CIN_revenue_dat_month_ts)

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
ggseasonplot(CIN_revenue_dat_month_ts) +
  ylab("$ Millions")+
  ggtitle("Figure 1: Revenue per year")

```
####################################################
Neural Network prediction model for total revenue by month
Testing assumptions before model
#################################################



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
####################################################
Neural Network prediction model for total revenue by month
Analysis
Assumptions testing
#################################################

#############################
Notes
#############################
Autoregressive model is like a lagged version of a linear model with the previous time point values as predictors.  So the value at time point 2, 3,..n predicting time point one?
P is the autoregressive parameter, which is the number of time points we go back to so we include each time point minus the previous as an average parameter if AR2 then each time point and first and second difference.

C = difference plus error

Moving average model uses the errors to predict future values

p = number of lags or previous y data points to predict
q = "moving average" is about the residuals as predicts of different time points
d = degree of difference in y

Order = (p,d,q)

Maybe phi is the average correlation between all time points and the difference according p (all t minus 1's).  For example, p = 1 is the all t minus 1's between. Phi is the average correlation between t and minus 1's.

Testing versus training: One shot testing with small amounts of data: https://otexts.com/fpp2/forecasting-on-training-and-test-sets.html

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

### Forecast
forecast_nn_auto = forecast(nn_auto, PI = TRUE, h = 12)
forecast_nn_auto

autoplot(forecast_nn_auto)+
  labs(title = "Figure 2: Forecasts for CIN Bloomington June 2019 to June 2020", y = "$ Millions in revenue per month", x = "Year")+
 #scale_x_date(breaks= as.Date(c("2017-06-01", "2018-01-01", "2018-06-01", "2019-01-01", "2019-05-01", "2020-01-01")), labels = date_format("%m/%Y"))+
  labs(color='Financial class')+
   scale_y_continuous(labels = dollar)
forecast_nn_auto
##Drop in revenue in Decemeber
mean(forecast_nn_auto$mean)-4584399

```
#######################
Descriptives for all data total revenue, number of payments, average payment, and sd for average payment by class
###########################
```{r}
head(CIN_revenue_dat)

CIN_revenue_desc = CIN_revenue_dat[,c(2,4,5)]
head(CIN_revenue_desc)

### CIN_revenue_desc needs a mean payment
CIN_revenue_desc$mean_payment = CIN_revenue_desc$Payments/CIN_revenue_desc$Number.of.Payments.Received
describe(CIN_revenue_desc)
CIN_revenue_desc

CIN_revenue_desc_total = aggregate(.~Financial.Class.Value
, data = CIN_revenue_desc, sum)
CIN_revenue_desc_total

CIN_revenue_desc_mean = aggregate(.~Financial.Class.Value
, data = CIN_revenue_desc, mean)

CIN_revenue_desc_mean

CIN_revenue_desc_sd = aggregate(.~Financial.Class.Value
, data = CIN_revenue_desc, sd)
CIN_revenue_desc_sd
CIN_revenue_desc_all = data.frame(finacial_class = CIN_revenue_desc_total$Financial.Class.Value, mean_payment_rev = CIN_revenue_desc_mean$mean_payment, sd_mean_payment = CIN_revenue_desc_sd$mean_payment, total_rev = CIN_revenue_desc_total$Payments, total_num_payments = CIN_revenue_desc_total$Number.of.Payments.Received)
CIN_revenue_desc_all = CIN_revenue_desc_all[order(-CIN_revenue_desc_all$mean_payment_rev),]
CIN_revenue_desc_all = mutate_if(CIN_revenue_desc_all, is.numeric, round)
CIN_revenue_desc_all = subset(CIN_revenue_desc_all, total_num_payments > 2000)
CIN_revenue_desc_all
write.csv(CIN_revenue_desc_all, "CIN_revenue_desc_all.csv", row.names = FALSE)


#### MRO percentage of total revenue
total_rev = sum(CIN_revenue_desc_all$total_rev)
mro_rev = CIN_revenue_desc_all[2,4]

mro_p_rev = round(mro_rev / total_rev,2)
mro_p_rev

```
####################
Plots
Data cleaning
#####################

###############
Notes
###############

Plot top six over time

DCS,MRO, HIP, Grant, Medicaid, Commercial
```{r}
CIN_revenue_stats_plot = CIN_revenue_dat[,c(1,2,4,5)]
CIN_revenue_stats_plot$mean_payment = CIN_revenue_stats_plot$Payments/CIN_revenue_stats_plot$Number.of.Payments.Received

CIN_revenue_stats_plot = 
CIN_revenue_stats_plot = subset(CIN_revenue_stats_plot, Financial.Class.Value == "DCS" |  Financial.Class.Value == "MRO" | Financial.Class.Value == "HIP" | Financial.Class.Value == "Grant" | Financial.Class.Value == "Medicaid" | Financial.Class.Value == "Commercial")

describe.factor(CIN_revenue_stats_plot$Financial.Class.Value)

library(tidyr)


CIN_revenue_stats_plot_mean =  CIN_revenue_stats_plot %>%
  group_by(Financial.Class.Value, Year.Month) %>%
  summarise_all(funs(mean))


CIN_revenue_stats_plot_total =  CIN_revenue_stats_plot %>%
  group_by(Financial.Class.Value, Year.Month) %>%
  summarise_all(funs(sum))



```
#####################
Plots
Analysis
#############

#############
Notes
#############
We want the total number of services, because this will help understand what is the easiest.  So DCS might be the biggest source of revenue.




```{r}
### Mean payment over time
library(scales)
min <- as.Date("2017-4-1")
max <- as.Date("2019-1-1")

ggplot(CIN_revenue_stats_plot_mean, aes(x = Year.Month, y = mean_payment))+
  geom_line(aes(colour = Financial.Class.Value))+
  ggtitle("Figure 3: Mean payment by financial class")+
  xlab("Time")+
  ylab("Mean payment")+
  scale_x_date(breaks= as.Date(c("2017-06-01", "2018-01-01", "2018-06-01", "2019-01-01", "2019-05-01")), labels = date_format("%m/%Y"))+
  labs(color='Financial class')+
  scale_y_continuous(labels = dollar)
  

### Total number of services
 ggplot(CIN_revenue_stats_plot_total, aes(x = Year.Month, y = Number.of.Payments.Received))+
  geom_line(aes(colour = Financial.Class.Value))+
  ggtitle("Figure 4: Total payments by financial class")+
  xlab("Time")+
  ylab("Total services")+
  scale_x_date(breaks= as.Date(c("2017-06-01", "2018-01-01", "2018-06-01", "2019-01-01", "2019-05-01")), labels = date_format("%m/%Y"))+
  labs(color='Financial class')+
   scale_y_continuous(labels = comma)

### Total revenue
 ### Total number of services

 
 ggplot(CIN_revenue_stats_plot_total, aes(x = Year.Month, y = Payments))+
  geom_line(aes(colour = Financial.Class.Value))+
  ggtitle("Figure 5: Total revenue by financial class")+
  xlab("Time")+
  ylab("Total revenue")+
  scale_x_date(breaks= as.Date(c("2017-06-01", "2018-01-01", "2018-06-01", "2019-01-01", "2019-05-01")), labels = date_format("%m/%Y"))+
  labs(color='Financial class')+
   scale_y_continuous(labels = dollar)
 
 
####How much revenue comes from MRO
CIN_revenue_stats_plot_total


```
##################
Monthly comparisons
Data cleaning
##################

##############
Notes
##############
Subet each of the top six into own data set for later analysis
```{r}
dcs_month =  subset(CIN_revenue_stats_plot_mean, Financial.Class.Value == "DCS")
mro_month = subset(CIN_revenue_stats_plot_mean, Financial.Class.Value == "MRO")
hip_month = subset(CIN_revenue_stats_plot_mean, Financial.Class.Value == "HIP")
grant_month = subset(CIN_revenue_stats_plot_mean, Financial.Class.Value == "Grant")
medicaid_month = subset(CIN_revenue_stats_plot_mean, Financial.Class.Value == "Medicaid")
commercial_month = subset(CIN_revenue_stats_plot_mean, Financial.Class.Value == "Commercial")
```
##################
Monthly comparisons
Analysis
##################

#################
Notes
#################
HIP, GRANT, Medicaid no different

```{r}
.05/15

757/149
########DCS
wilcox.test(dcs_month$mean_payment, mro_month$mean_payment)
wilcox.test(dcs_month$mean_payment, hip_month$mean_payment)
wilcox.test(dcs_month$mean_payment, grant_month$mean_payment)
wilcox.test(dcs_month$mean_payment, medicaid_month$mean_payment)
wilcox.test(dcs_month$mean_payment, commercial_month$mean_payment)

#######MRO
wilcox.test(mro_month$mean_payment, hip_month$mean_payment)
wilcox.test(mro_month$mean_payment, grant_month$mean_payment)
wilcox.test(mro_month$mean_payment, medicaid_month$mean_payment)
wilcox.test(mro_month$mean_payment, commercial_month$mean_payment)

#######HIP
wilcox.test(hip_month$mean_payment, grant_month$mean_payment)
wilcox.test(hip_month$mean_payment, medicaid_month$mean_payment)
wilcox.test(hip_month$mean_payment, commercial_month$mean_payment)

#######Grant
wilcox.test(grant_month$mean_payment, medicaid_month$mean_payment)
wilcox.test(grant_month$mean_payment, commercial_month$mean_payment)

#######Medicaid
wilcox.test(medicaid_month$mean_payment, commercial_month$mean_payment)


```












