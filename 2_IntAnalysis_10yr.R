# this file performs intervention analysis on 10yr yield. Returns the results of 
# transfer fn in TF_1 and intervention analysis in Int_10y and Int_10y_Auc.
# File follows 4-step process for first identifying transfer function model in 
# pre-intervention data.

reg_dat[c("EFFR_1","DGS10_1","DGS10_+1")] = 
  cbind(dplyr::lag(reg_dat$EFFR,n = 1), dplyr::lag(reg_dat$DGS10,n = 1),
        dplyr::lead(reg_dat$DGS10,n = 1) )

# Identify pre-intervention transfer function -----------------------------

## Step 1: Identify ARIMA model -------------------------------------------
auto.arima(Merge_dat$GIND10Y[Period[,"Pre"]])#suggests 3,1,3
#ACF and PACF checked to make own judgement regarding order
acf(na.omit(diff(Merge_dat$GIND10Y[Period[,"Pre"]])))
pacf(na.omit(diff(Merge_dat$GIND10Y[Period[,"Pre"]]))) #ACF and PACF at 2nd lags significant
mod_10y = arima(Merge_dat$GIND10Y[Period[,"Pre"]], order = c(2,1,0), fixed = c(0, NA))
# MA 1 and AR1 terms insignificant when added. Comparing 2,1,0  0,1,2 and 2,1,2 
# show MA>AR>ARMA. However, AR(2) chosen
mod_10y
BIC(mod_10y)
acf(mod_10y$residuals);pacf(mod_10y$residuals) #ACF and PACF show residuals are white noise

### Step 1.2 Cross Correlation Function -----------------------------------------
op = par()
par(mfrow = c(2,2), mai = c(0.7,0.7,0.5,0.1))
fitwhite = residuals(Arima(Merge_dat$GIND10Y[Period[,"Pre"]], model = mod_10y))
fitwhite1 = residuals(Arima(Merge_dat$WACR[Period[,"Pre"]], model = mod_10y))
ccf(fitwhite1,fitwhite, ylab = "CCF", xlab = "", main = "10yr ~ WACR", lag.max = 15)
fitwhite1 = residuals(Arima(Merge_dat$Liq[Period[,"Pre"]], model = mod_10y))
ccf(fitwhite1,fitwhite, ylab = "", xlab = "", main = "10yr ~ Liquidity", lag.max = 15)
fitwhite1 = residuals(Arima(Merge_dat$EFFR[Period[,"Pre"]], model = mod_10y))
ccf(fitwhite1,fitwhite, ylab = "CCF", main = "10yr ~ EFFR", lag.max = 15)
fitwhite1 = residuals(Arima(Merge_dat$DGS10[Period[,"Pre"]], model = mod_10y))
ccf(fitwhite1,fitwhite, lag.max = 15, ylab = "", main = "10yr ~ US10yr",)
par(mfrow =  op$mfrow, mai = op$mai) #reverting graphics options

## Step 2 - Linear regression with covariates -----------------------------------

# lags identified from CCF are used in regression

reg10 = lm(GIND10Y ~ Liq + DGS10,# `DGS10_+1`, 
            data = reg_dat[Period[,"Pre"],]) 
summary(reg10) #DGS10_1 always insignificant. DGS10_+1 significant if added, DGS10 sig when leads/lags not included

## Step 3 - ARIMA model on Linear regression errors-----------------------------
acf(diff(reg10$residuals)); pacf(diff(reg10$residuals)) #suggests ARIMA(0,1,0)
auto.arima(reg10$residuals)#suggest ARIMA 2,1,0 with zero mean
# following checks if the residuals have unit root and finds null of unit root is rejected with p-value just at 0.05
summary(ur.df(reg10$residuals, type = "trend",selectlags = "AIC"))
summary(ur.df(na.omit(diff(reg10$residuals)),type = "trend",selectlags = "AIC"))

ar10 = arima(reg10$residuals, order = c(2,1,0), fixed = c(0,NA)) #better than auto.arima
summary(ar10)
BIC(ar10)
(1-pnorm(abs(ar10$coef)/sqrt(diag(ar10$var.coef))))*2 #calculating p-values.

## Step 4 - Fitting Transfer function ------------------------------------------
xvars = c("Liq","EFFR","EFFR_1","WACR","WACR_1", "DGS10","DGS10_1")
xvars = c("Liq","EFFR_1","DGS10","DGS10_1")
#xvars = c("Liq","EFFR_1","DGS10_1")

TF_10 = arima(diff_dat[Period[,"Pre"],"GIND10Y"], order = c(2,0,0),
              fixed = c(0,rep(NA,length(xvars)+1) ), include.mean = F,
               xreg = diff_dat[Period[,"Pre"], xvars])

summary(TF_10) #The transfer function model identified
BIC(TF_10)
(1-pnorm(abs(TF_10$coef[-1])/sqrt(diag(TF_10$var.coef))))*2 #calculating p-values. pnorm and not pt used as estimation is via MLE which gives asymptotically normal estimates. details here https://stats.stackexchange.com/questions/8868/how-to-calculate-the-p-value-of-parameters-for-arima-model-in-r

# Intervention Analysis ---------------------------------------------------

Int_10y = arima(diff_dat[Period[,"Int"],"GIND10Y"], order = c(2,0,0),
                fixed = c(0,rep(NA,length(xvars)+2) ), include.mean = F,
                xreg = diff_dat[Period[,"Int"], c(xvars,"D_Ann")])

summary(Int_10y) #The transfer function model identified
(1-pnorm(abs(Int_10y$coef[-1])/sqrt(diag(Int_10y$var.coef))))*2 #calculating p-values. pnorm and not pt used as estimation is via MLE which gives asymptotically normal estimates. details here https://stats.stackexchange.com/questions/8868/how-to-calculate-the-p-value-of-parameters-for-arima-model-in-r
Int_10y$nobs

Int_10y_Auc = arima(diff_dat[Period[,"Int"],"GIND1Y"],order = c(2,0,0),
                    fixed = c(0,rep(NA,length(xvars)+2) ), include.mean = F,
                    xreg = diff_dat[Period[,"Int"], c(xvars,"D_Auc")])
summary(Int_10y_Auc) #The transfer function model identified
(1-pnorm(abs(Int_10y_Auc$coef[-1])/sqrt(diag(Int_10y_Auc$var.coef))))*2 #calculating p-values. pnorm and not pt used as estimation is via MLE which gives asymptotically normal estimates. details here https://stats.stackexchange.com/questions/8868/how-to-calculate-the-p-value-of-parameters-for-arima-model-in-r
Int_10y_Auc$nobs

# Removing unnecessary variables ------------------------------------------

rm(mod_10y,fitwhite, fitwhite1,op, ar10)
