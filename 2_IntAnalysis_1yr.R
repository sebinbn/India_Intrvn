# this file performs intervention analysis on 1yr yield. Returns the results of 
# transfer fn in TF_1 and intervention analysis in Int_1y and Int_1y_Auc.
# File follows 4-step process for first identifying transfer function model in 
# pre-intervention data. Since this gives weird lags for covariates as significant,
# 1 and 2 lags are tried for all covariates in transfer function. After trying both,
# a decision on lags to be chosen in used.
# Differenced liquidity works better in pre-int but not in intervention period.
# Liquidity and dummies at levels are used. For this, the transfer function and 
# intervention analysis is estimated using diff_dat



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
ccf(fitwhite1,fitwhite, ylab = "CCF", xlab = "", main = "1yr ~ WACR", lag.max = 15)
fitwhite1 = residuals(Arima(Merge_dat$Liq[Period[,"Pre"]], model = mod_1y))
ccf(fitwhite1,fitwhite, ylab = "", xlab = "", main = "1yr ~ Liquidity", lag.max = 15)
fitwhite1 = residuals(Arima(Merge_dat$EFFR[Period[,"Pre"]], model = mod_1y))
ccf(fitwhite1,fitwhite, ylab = "CCF", main = "1yr ~ EFFR", lag.max = 15)
fitwhite1 = residuals(Arima(Merge_dat$DGS10[Period[,"Pre"]], model = mod_1y))
ccf(fitwhite1,fitwhite, lag.max = 15, ylab = "", main = "1yr ~ US10yr",)
par(mfrow =  op$mfrow, mai = op$mai) #reverting graphics options

## Step 2 - Linear regression with covariates -----------------------------------

reg1 = lm(GIND1Y ~ WACR + WACR_1 + Liq + EFFR + DGS10_1, 
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
# Various xvariables checked.
xvars = c("Liq","Liq_1", "Liq_2","WACR","WACR_1","WACR_2",
          "EFFR","EFFR_1","EFFR_2", "DGS10","DGS10_1","DGS10_2")
xvars = c("Liq","Liq_1", "WACR","WACR_1",
          "EFFR","EFFR_1", "DGS10","DGS10_1")
xvars = c("Liq","Liq_5","WACR_3", "DGS10","DGS10_1") #weird lags from CCF
xvars = c("Liq","Liq_5","DGS10","DGS10_1") 

TF_1 = arima(diff_dat[Period[,"Pre"],"GIND1Y"], order = c(1,0,0),
             include.mean = F, xreg = diff_dat[Period[,"Pre"],xvars])

summary(TF_1) #The transfer function model identified
BIC(TF_1)
(1-pnorm(abs(TF_1$coef)/sqrt(diag(TF_1$var.coef))))*2 #calculating p-values. pnorm and not pt used as estimation is via MLE which gives asymptotically normal estimates. details here https://stats.stackexchange.com/questions/8868/how-to-calculate-the-p-value-of-parameters-for-arima-model-in-r


# Intervention Analysis ---------------------------------------------------

Int_1y = arima(diff_dat[Period[,"Int"],"GIND1Y"], order = c(1,0,0),include.mean = F,
                xreg = diff_dat[Period[,"Int"], c(xvars,"D_Ann")])

summary(Int_1y) #The transfer function model identified
(1-pnorm(abs(Int_1y$coef)/sqrt(diag(Int_1y$var.coef))))*2 #calculating p-values. pnorm and not pt used as estimation is via MLE which gives asymptotically normal estimates. details here https://stats.stackexchange.com/questions/8868/how-to-calculate-the-p-value-of-parameters-for-arima-model-in-r
Int_1y$nobs

Int_1y_Auc = arima(diff_dat[Period[,"Int"],"GIND1Y"], order = c(1,0,0),include.mean = F,
                    xreg = diff_dat[Period[,"Int"], c(xvars,"D_Auc")])
summary(Int_1y_Auc) #The transfer function model identified
(1-pnorm(abs(Int_1y_Auc$coef)/sqrt(diag(Int_1y_Auc$var.coef))))*2 #calculating p-values. pnorm and not pt used as estimation is via MLE which gives asymptotically normal estimates. details here https://stats.stackexchange.com/questions/8868/how-to-calculate-the-p-value-of-parameters-for-arima-model-in-r
Int_1y_Auc$nobs


# Removing unnecessary variables ------------------------------------------

rm(mod_1y,fitwhite, fitwhite1,op, ar101)
