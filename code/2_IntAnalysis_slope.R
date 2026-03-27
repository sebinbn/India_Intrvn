# Purpose:
# Estimate the transfer-function and intervention-analysis models for the
# 10-year minus 1-year slope, storing the fitted objects used in the summary
# tables and later tabulation steps.
#
# The script follows a four-step workflow that first identifies the
# pre-intervention transfer-function specification.

# Note: Adding upto 2 lags of each covariate and then taking off insignificant 
# variables doesn't work here as all covariates turn up insignificant. So, covariates
# from CCF which show strange lags significant are used in TF and found to be significant.
# Non-differenced Liq is added but is not significant, Liq_5 is significant both 
# differenced and levels.
# So, lags from CCF and Liq are used with Liq vars not differenced.



# 1. Creating dataframe with lags -------------------------------------------

# lags used in final transfer-function / intervention specifications
xvars = c("WACR_12", "Liq", "Liq_5", "EFFR_12", "DGS10_5")
lagDat = build_lag_data(MergedDat, xvars)
lagDat_diff = build_lag_data(MergedDat_diff, xvars)

# 2. Identify pre-intervention transfer function -----------------------------

## Step 1: Identify ARIMA model -------------------------------------------
auto.arima(MergedDat$s101[Period[,"Pre"]])#suggests 2,1,2
#ACF and PACF checked to make own judgement regarding order
acf(na.omit(diff(MergedDat$s101[Period[,"Pre"]])))
pacf(na.omit(diff(MergedDat$s101[Period[,"Pre"]]))) #ACF and PACF at 1st lag significant
mod_s101 = arima(MergedDat$s101[Period[,"Pre"]], order = c(1,1,0)) 
#1,1,0 > 0,1,1 in BIC. 1,1,1 has both insigniicant and of opposite signs.
mod_s101
BIC(mod_s101)
acf(mod_s101$residuals);pacf(mod_s101$residuals) #ACF and PACF show residuals are white noise

### Step 1.2 Cross Correlation Function -----------------------------------------
op = par()
par(mfrow = c(2,2), mai = c(0.7,0.7,0.5,0.1))
fitwhite = residuals(Arima(MergedDat$s101[Period[,"Pre"]], model = mod_s101))
fitwhite1 = residuals(Arima(MergedDat$WACR[Period[,"Pre"]], model = mod_s101))
ccf(fitwhite1,fitwhite, ylab = "CCF", xlab = "", main = "10yr-1yr ~ WACR", lag.max = 15)
fitwhite1 = residuals(Arima(MergedDat$Liq[Period[,"Pre"]], model = mod_s101))
ccf(fitwhite1,fitwhite, ylab = "", xlab = "", main = "10yr-1yr ~ Liquidity", lag.max = 15)
fitwhite1 = residuals(Arima(MergedDat$EFFR[Period[,"Pre"]], model = mod_s101))
ccf(fitwhite1,fitwhite, ylab = "CCF", main = "10yr-1yr ~ EFFR", lag.max = 15)
fitwhite1 = residuals(Arima(MergedDat$DGS10[Period[,"Pre"]], model = mod_s101))
ccf(fitwhite1,fitwhite, lag.max = 15, ylab = "", main = "10yr-1yr ~ US10yr",)
par(mfrow =  op$mfrow, mai = op$mai) #reverting graphics options

## Step 2 - Linear regression with covariates -----------------------------------

# lags identified from CCF are used in regression
reg101 = lm(s101 ~ WACR_12 + Liq  +  EFFR_12 + DGS10_5, 
            data = lagDat[Period[,"Pre"],]) #adding Liq makes Liq_5 insignificant
summary(reg101)

## Step 3 - ARIMA model on Linear regression errors-----------------------------
acf(diff(reg101$residuals)); pacf(diff(reg101$residuals)) #suggests ARIMA(1,1,1)
auto.arima(reg101$residuals)#suggest ARIMA 3,0,0 with zero mean
# following checks if the residuals have unit root and finds null of unit root is rejected with 0.05 significance
summary(ur.df(reg101$residuals, type = "trend",selectlags = "AIC"))
summary(ur.df(na.omit(diff(reg101$residuals)),type = "trend",selectlags = "AIC"))

ar101 = arima(reg101$residuals, order = c(1,1,0))
# 1,1,1 > 0,1,1 > 1,1,0 > 2,1,0. But 1,1,1 has somewhat equal and opposite sign coefficients.
# All better than auto.arima . AR(1) chosen as AR(1) and MA(1) coefficients similar magnitude.
summary(ar101)
BIC(ar101)

## Step 4 - Fitting Transfer function ------------------------------------------

TF_101 = arima(lagDat_diff[Period_diff[,"Pre"], "s101"], order = c(1,0,0),
               include.mean = F, xreg = lagDat_diff[Period_diff[,"Pre"], xvars])
print("Transfer function model for slope")
print(summary(TF_101)) #The transfer function model identified
print((1-pnorm(abs(TF_101$coef)/sqrt(diag(TF_101$var.coef))))*2) #calculating p-values. pnorm and not pt used as estimation is via MLE which gives asymptotically normal estimates. details here https://stats.stackexchange.com/questions/8868/how-to-calculate-the-p-value-of-parameters-for-arima-model-in-r
print(TF_101$nobs)


# Intervention Analysis ---------------------------------------------------

Int_101 = arima(lagDat_diff[Period_diff[,"Int"], "s101"], order = c(1,0,0),
                include.mean = F, xreg = lagDat_diff[Period_diff[,"Int"], 
                                                  c(xvars,"D_Ann")])
print("Intervention analysis for slope")
print(summary(Int_101))
print((1-pnorm(abs(Int_101$coef)/sqrt(diag(Int_101$var.coef))))*2) #calculating p-values. pnorm and not pt used as estimation is via MLE which gives asymptotically normal estimates. details here https://stats.stackexchange.com/questions/8868/how-to-calculate-the-p-value-of-parameters-for-arima-model-in-r
print(Int_101$nobs)

Int_101_Auc = arima(lagDat_diff[Period_diff[,"Int"], "s101"], order = c(1,0,0),
                    include.mean = F, xreg = lagDat_diff[Period_diff[,"Int"], 
                                                      c(xvars,"D_Auc")])
print(summary(Int_101_Auc))
print((1-pnorm(abs(Int_101_Auc$coef)/sqrt(diag(Int_101_Auc$var.coef))))*2) #calculating p-values. pnorm and not pt used as estimation is via MLE which gives asymptotically normal estimates. details here https://stats.stackexchange.com/questions/8868/how-to-calculate-the-p-value-of-parameters-for-arima-model-in-r
Int_101_Auc$nobs


Int_101_Cum = arima(lagDat_diff[Period_diff[,"Int"],"s101"], order = c(1,0,0),include.mean = F,
                  xreg = lagDat_diff[Period_diff[,"Int"], c(xvars,paste("D_Ann_",1:24,sep = ""))])
print((1-pnorm(abs(Int_101_Cum$coef)/sqrt(diag(Int_101_Cum$var.coef))))*2)
sum(Int_101_Cum$coef[paste("D_Ann_",1:24,sep = "")])

# Removing unnecessary variables ------------------------------------------
rm(mod_s101,fitwhite, fitwhite1,op, ar101, reg101, xvars)
