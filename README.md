---
title: "Test"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Review revenue.  Get date in mdy format.  Need to change Jul and so far to correct number and add day (1)
```{r}
head(CIN_revenue)
colnames(CIN_revenue)[2] = "location"
head(CIN_revenue)
library(lubridate)
CIN_revenue_dat = CIN_revenue
CIN_revenue_dat$Year.Month = str_replace_all(CIN_revenue_dat$Year.Month, c("Jan" = "1", "Feb"="2", "Mar"="3", "Apr"="4", "May"="5", "Jun"="6", "Jul"="7", "Aug"="8", "Sep" = "9", "Oct"="10", "Nov"="11", "Dec"="12"))
paste("A", 1:6, sep = "")
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

