# this file performs intervention analysis on 1yr yield. Returns the results in TF_1.
# File follows 4-step process for first identifying transfer function model in 
# pre-intervention data.


Period = cbind(
  Pre = Merge_dat$Date >= as.Date("2018-04-01") & Merge_dat$Date <= as.Date("2019-11-30"),
  Int = Merge_dat$Date >= as.Date("2019-12-01") & Merge_dat$Date <= as.Date("2021-05-31") )


# Identify pre-intervention transfer function -----------------------------

## Step 1: Identify ARIMA model -------------------------------------------
auto.arima(Merge_dat$GIND1Y[Period[,"Pre"]])#suggests 3,2,1
#ACF and PACF checked to make own judgement regarding order
acf(na.omit(diff(Merge_dat$GIND1Y[Period[,"Pre"]])))
pacf(na.omit(diff(Merge_dat$GIND1Y[Period[,"Pre"]]))) #ACF and PACF at 1st lags significant
mod_1y = arima(Merge_dat$GIND1Y[Period[,"Pre"]], order = c(1,1,0)) #AR1 > MA1 >> ARMA11. AR1 chosen
mod_1y
BIC(mod_1y)
acf(mod_1y$residuals);pacf(mod_1y$residuals) #ACF and PACF show residuals are not white noise (ignoring this for now)

### Step 1.2 Cross Correlation Function -----------------------------------------
op = par()
par(mfrow = c(2,2), mai = c(0.7,0.7,0.5,0.1))
fitwhite = residuals(Arima(Merge_dat$GIND1Y[Period[,"Pre"]], model = mod_1y))
fitwhite1 = residuals(Arima(Merge_dat$WACR[Period[,"Pre"]], model = mod_1y))
ccf(fitwhite1,fitwhite, ylab = "CCF", xlab = "", main = "10yr-1yr ~ WACR", lag.max = 15)
fitwhite1 = residuals(Arima(Merge_dat$Liq[Period[,"Pre"]], model = mod_1y))
ccf(fitwhite1,fitwhite, ylab = "", xlab = "", main = "10yr-1yr ~ Liquidity", lag.max = 15)
fitwhite1 = residuals(Arima(Merge_dat$EFFR[Period[,"Pre"]], model = mod_1y))
ccf(fitwhite1,fitwhite, ylab = "CCF", main = "10yr-1yr ~ EFFR", lag.max = 15)
fitwhite1 = residuals(Arima(Merge_dat$DGS10[Period[,"Pre"]], model = mod_1y))
ccf(fitwhite1,fitwhite, lag.max = 15, ylab = "", main = "10yr-1yr ~ US10yr",)
par(mfrow =  op$mfrow, mai = op$mai) #reverting graphics options

## Step 2 - Linear regression with covariates -----------------------------------

# lags identified from CCF are used in regression

reg_dat = Merge_dat # Note: subsetting smaller time period within lm makes 0 NAs when lagging. So no subsetting here
reg_dat[c("WACR_1","Liq_1","EFFR_1","DGS10_1")] = 
  cbind(dplyr::lag(reg_dat$WACR,n = 1), dplyr::lag(reg_dat$Liq,n = 1),
        dplyr::lag(reg_dat$EFFR,n = 1),dplyr::lag(reg_dat$DGS10,n = 1) )
reg1 = lm(GIND1Y ~ WACR + WACR_1 + Liq + EFFR +   DGS10_1, 
           data = reg_dat[Period[,"Pre"],]) 
summary(reg1)

## Step 3 - ARIMA model on Linear regression errors-----------------------------
acf(diff(reg1$residuals)); pacf(diff(reg1$residuals)) #suggests ARIMA(1,1,1)
auto.arima(reg1$residuals)#suggest ARIMA 2,0,0 with zero mean
# following checks if the residuals have unit root and finds null of unit root is rejected
summary(ur.df(reg1$residuals, type = "trend",selectlags = "AIC"))
summary(ur.df(na.omit(diff(reg1$residuals)),type = "trend",selectlags = "AIC"))

ar1 = arima(reg1$residuals, order = c(1,1,1), include.mean = F) 
summary(ar1)
BIC(ar1)
(1-pnorm(abs(ar1$coef)/sqrt(diag(ar1$var.coef))))*2 #calculating p-values.

## Step 4 - Fitting Transfer function ------------------------------------------

reg_dat = reg_dat[Period[,"Pre"],]
TF_1 = arima(reg_dat$GIND1Y, order = c(1,1,0), 
              xreg = reg_dat[c("Liq","WACR","EFFR", "DGS10_1")])

summary(TF_1) #The transfer function model identified
BIC(TF_1)
(1-pnorm(abs(TF_1$coef)/sqrt(diag(TF_1$var.coef))))*2 #calculating p-values. pnorm and not pt used as estimation is via MLE which gives asymptotically normal estimates. details here https://stats.stackexchange.com/questions/8868/how-to-calculate-the-p-value-of-parameters-for-arima-model-in-r

# Removing unnecessary variables ------------------------------------------

rm(Period,mod_1y,fitwhite, fitwhite1,op, ar101)
