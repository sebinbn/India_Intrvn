# this file performs intervention analysis on 1yr yield. Returns the results of 
# transfer fn in TF_1 and intervention analysis in Int_1 and Int_1_Auc.
# File follows 4-step process for first identifying transfer function model in 
# pre-intervention data. Since this gives weird lags for covariates as significant,
# 1 and 2 lags are tried for all covariates in transfer function. After trying both,
# a decision on lags to be chosen in used.
# Differenced liquidity works better in pre-int but not in intervention period.
# Liquidity and dummies at levels are used. For this, the transfer function and 
# intervention analysis is estimated using lagDat_diff

# 1. Creating dataframe with lags -------------------------------------------

# lags used in final transfer-function / intervention specifications
xvars = c("Liq", "DGS10","DGS10_1") 
lagDat = build_lag_data(MergedDat, xvars)
lagDat_diff = build_lag_data(MergedDat_diff, xvars)

# 2. Identify pre-intervention transfer function -----------------------------

## Step 1: Identify ARIMA model -------------------------------------------
auto.arima(MergedDat$GIND1Y[Period[,"Pre"]])#suggests 3,2,1
#ACF and PACF checked to make own judgement regarding order
acf(na.omit(diff(MergedDat$GIND1Y[Period[,"Pre"]])))
pacf(na.omit(diff(MergedDat$GIND1Y[Period[,"Pre"]]))) #ACF and PACF at 1st lags significant
mod_1y = arima(MergedDat$GIND1Y[Period[,"Pre"]], order = c(1,1,0)) #AR1 > MA1 >> ARMA11. AR1 chosen
mod_1y
BIC(mod_1y)
acf(mod_1y$residuals);pacf(mod_1y$residuals) #ACF and PACF show residuals are not white noise (ignoring this for now)

### Step 1.2 Cross Correlation Function -----------------------------------------
op = par()
par(mfrow = c(2,2), mai = c(0.7,0.7,0.5,0.1))
fitwhite = residuals(Arima(MergedDat$GIND1Y[Period[,"Pre"]], model = mod_1y))
fitwhite1 = residuals(Arima(MergedDat$WACR[Period[,"Pre"]], model = mod_1y))
ccf(fitwhite1,fitwhite, ylab = "CCF", xlab = "", main = "1yr ~ WACR", lag.max = 15)
fitwhite1 = residuals(Arima(MergedDat$Liq[Period[,"Pre"]], model = mod_1y))
ccf(fitwhite1,fitwhite, ylab = "", xlab = "", main = "1yr ~ Liquidity", lag.max = 15)
fitwhite1 = residuals(Arima(MergedDat$EFFR[Period[,"Pre"]], model = mod_1y))
ccf(fitwhite1,fitwhite, ylab = "CCF", main = "1yr ~ EFFR", lag.max = 15)
fitwhite1 = residuals(Arima(MergedDat$DGS10[Period[,"Pre"]], model = mod_1y))
ccf(fitwhite1,fitwhite, lag.max = 15, ylab = "", main = "1yr ~ US10yr",)
par(mfrow =  op$mfrow, mai = op$mai) #reverting graphics options

## Step 2 - Linear regression with covariates -----------------------------------

reg1 = lm(GIND1Y ~ WACR + WACR_1 + Liq + EFFR + DGS10_1, 
           data = lagDat[Period[,"Pre"],]) 
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

TF_1 = arima(lagDat_diff[Period_diff[,"Pre"],"GIND1Y"], order = c(0,0,1),
             include.mean = F, xreg = lagDat_diff[Period_diff[,"Pre"],xvars])
print("Transfer function model for 1 yr yield")
print(summary(TF_1)) #The transfer function model identified
BIC(TF_1)
print("TF p-values")
print((1-pnorm(abs(TF_1$coef)/sqrt(diag(TF_1$var.coef))))*2) #calculating p-values. pnorm and not pt used as estimation is via MLE which gives asymptotically normal estimates. details here https://stats.stackexchange.com/questions/8868/how-to-calculate-the-p-value-of-parameters-for-arima-model-in-r
print(TF_1$nobs)

# Intervention Analysis ---------------------------------------------------


## For average impact -----------------------------------------------------------

print("Intervention analysis for 1 yr yield")
Int_1 = arima(
  lagDat_diff[Period_diff[,"Int"],"GIND1Y"], order = c(0,0,1),include.mean = F,
                xreg = lagDat_diff[Period_diff[,"Int"], c(xvars,"D_Ann")])
print(summary(Int_1)) 
print("1yr Int Analysis p-values")
print((1-pnorm(abs(Int_1$coef)/sqrt(diag(Int_1$var.coef))))*2) #calculating p-values. pnorm and not pt used as estimation is via MLE which gives asymptotically normal estimates. details here https://stats.stackexchange.com/questions/8868/how-to-calculate-the-p-value-of-parameters-for-arima-model-in-r

Int_1_Auc = arima(
  lagDat_diff[Period_diff[,"Int"],"GIND1Y"], order = c(1,0,0),include.mean = F,
                    xreg = lagDat_diff[Period_diff[,"Int"], c(xvars,"D_Auc")])
print(summary(Int_1_Auc)) 
print((1-pnorm(abs(Int_1_Auc$coef)/sqrt(diag(Int_1_Auc$var.coef))))*2) #calculating p-values. pnorm and not pt used as estimation is via MLE which gives asymptotically normal estimates. details here https://stats.stackexchange.com/questions/8868/how-to-calculate-the-p-value-of-parameters-for-arima-model-in-r


# Running a single regression with 24 dummies

Int_1_Cum = arima(
  lagDat_diff[Period_diff[,"Int"],"GIND1Y"], order = c(1,0,0),include.mean = F,
                  xreg = lagDat_diff[Period_diff[,"Int"],
                                     c(xvars,paste("D_Ann_",1:24,sep = ""))])
print(summary(Int_1_Cum))
print((1-pnorm(abs(Int_1_Cum$coef)/sqrt(diag(Int_1_Cum$var.coef))))*2)
sum(Int_1_Cum$coef[paste("D_Ann_",1:24,sep = "")])

# Removing unnecessary variables ------------------------------------------

rm(mod_1y,fitwhite, fitwhite1,op, ar1)
