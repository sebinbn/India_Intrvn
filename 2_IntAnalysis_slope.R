# this file performs intervention analysis on slope. Returns the results in TF_101.
# File follows 4-step process for first identifying transfer function model in 
# pre-intervention data.


Period = cbind(
  Pre = Merge_dat$Date >= as.Date("2018-04-01") & Merge_dat$Date <= as.Date("2019-11-30"),
  Int = Merge_dat$Date >= as.Date("2019-12-01") & Merge_dat$Date <= as.Date("2021-05-31") )


# Identify pre-intervention transfer function -----------------------------

## Step 1: Identify ARIMA model -------------------------------------------
auto.arima(Merge_dat$s101[Period[,"Pre"]])#suggests 2,1,2
#ACF and PACF checked to make own judgement regarding order
acf(na.omit(diff(Merge_dat$s101[Period[,"Pre"]])))
pacf(na.omit(diff(Merge_dat$s101[Period[,"Pre"]]))) #ACF and PACF at 1st lag significant
mod_s101 = arima(Merge_dat$s101[Period[,"Pre"]], order = c(1,1,0)) #MA 1 slightly better than AR1. This is better than AR1. ARMA11 makes both insignificant.
mod_s101
BIC(mod_s101)
acf(mod_s101$residuals);pacf(mod_s101$residuals) #ACF and PACF show residuals are white noise

### Step 1.2 Cross Correlation Function -----------------------------------------
op = par()
par(mfrow = c(2,2), mai = c(0.7,0.7,0.5,0.1))
fitwhite = residuals(Arima(Merge_dat$s101[Period[,"Pre"]], model = mod_s101))
fitwhite1 = residuals(Arima(Merge_dat$WACR[Period[,"Pre"]], model = mod_s101))
ccf(fitwhite1,fitwhite, ylab = "CCF", xlab = "", main = "10yr-1yr ~ WACR", lag.max = 15)
fitwhite1 = residuals(Arima(Merge_dat$Liq[Period[,"Pre"]], model = mod_s101))
ccf(fitwhite1,fitwhite, ylab = "", xlab = "", main = "10yr-1yr ~ Liquidity", lag.max = 15)
fitwhite1 = residuals(Arima(Merge_dat$EFFR[Period[,"Pre"]], model = mod_s101))
ccf(fitwhite1,fitwhite, ylab = "CCF", main = "10yr-1yr ~ EFFR", lag.max = 15)
fitwhite1 = residuals(Arima(Merge_dat$DGS10[Period[,"Pre"]], model = mod_s101))
ccf(fitwhite1,fitwhite, lag.max = 15, ylab = "", main = "10yr-1yr ~ US10yr",)
par(mfrow =  op$mfrow, mai = op$mai) #reverting graphics options

## Step 2 - Linear regression with covariates -----------------------------------

# lags identified from CCF are used in regression

reg_dat = Merge_dat #subsetting smaller time period within lm makes 0 NAs when lagging
reg_dat[c("WACR_12","Liq_5","EFFR_12","DGS10_5")] =
  cbind(dplyr::lag(reg_dat$WACR,n = 12), dplyr::lag(reg_dat$Liq,n = 5),
        dplyr::lag(reg_dat$EFFR,n = 12), dplyr::lag(reg_dat$DGS10,n = 5))
reg101 = lm(s101 ~ WACR_12 + Liq +  EFFR_12 + DGS10_5, 
            data = reg_dat[Period[,"Pre"],]) #adding Liq makes Liq_5 insignificant
summary(reg101)

## Step 3 - ARIMA model on Linear regression errors-----------------------------
acf(diff(reg101$residuals)); pacf(diff(reg101$residuals)) #suggests ARIMA(1,1,1)
auto.arima(reg101$residuals)#suggest ARIMA 1,0,1 with zero mean
# following checks if the residuals have unit root and finds null of unit root is rejected with p-value just at 0.05
summary(ur.df(reg101$residuals, type = "trend",selectlags = "AIC"))
summary(ur.df(na.omit(diff(reg101$residuals)),type = "trend",selectlags = "AIC"))

ar101 = arima(reg101$residuals, order = c(0,1,1)) #this is not better than auto.arima
summary(ar101)
BIC(ar101)


## Step 4 - Fitting Transfer function ------------------------------------------


reg_dat = reg_dat[Period[,"Pre"],]
TF_101 = arima(reg_dat$s101, order = c(1,1,0), 
               xreg = reg_dat[c("WACR_12","Liq","Liq_5","EFFR_12","DGS10_5")])
                                                                  
summary(TF_101) #The transfer function model identified
(1-pnorm(abs(TF_101$coef)/sqrt(diag(TF_101$var.coef))))*2 #calculating p-values. pnorm and not pt used as estimation is via MLE which gives asymptotically normal estimates. details here https://stats.stackexchange.com/questions/8868/how-to-calculate-the-p-value-of-parameters-for-arima-model-in-r

# Removing unnecessary variables ------------------------------------------

rm(Period,mod_s101,fitwhite, fitwhite1,op, ar101)
