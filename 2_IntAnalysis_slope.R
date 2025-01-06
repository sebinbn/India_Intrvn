# this file performs intervention analysis on slope. Returns the transfer function
# in TF_101 and Int analysis in Int_101.
# File follows 4-step process for first identifying transfer function model in 
# pre-intervention data.

# Note: Adding upto 2 lags of each covariate and then taking off insignificant 
# variables doesn't work here as all covariates turn up insignificant. So, covariates
# from CCF which show strange lags significant are used in TF and found to be significant.
# Non-differenced Liq is added but is not significant, Liq_5 is significant both 
# differenced and levels.
# So, lags from CCF and Liq are used with Liq vars not differenced.


# Identify pre-intervention transfer function -----------------------------

## Step 1: Identify ARIMA model -------------------------------------------
auto.arima(Merge_dat$s101[Period[,"Pre"]])#suggests 2,1,2
#ACF and PACF checked to make own judgement regarding order
acf(na.omit(diff(Merge_dat$s101[Period[,"Pre"]])))
pacf(na.omit(diff(Merge_dat$s101[Period[,"Pre"]]))) #ACF and PACF at 1st lag significant
mod_s101 = arima(Merge_dat$s101[Period[,"Pre"]], order = c(1,1,0)) #1,1,0 > 0,1,1 in BIC. 1,1,1 has both insigniicant and of opposite signs.
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
reg101 = lm(s101 ~ WACR_12 + Liq  +  EFFR_12 + DGS10_5, 
            data = reg_dat[Period[,"Pre"],]) #adding Liq makes Liq_5 insignificant
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
xvars = c("Liq","Liq_1", "Liq_2","WACR","WACR_1","WACR_2",
          "EFFR","EFFR_1","EFFR_2", "DGS10","DGS10_1","DGS10_2")
xvars = c("Liq","WACR","EFFR_1")
xvars = c("WACR_12","Liq","Liq_5","EFFR_12","DGS10_5")

TF_101 = arima(diff_dat[Period[,"Pre"], "s101"], order = c(1,0,0),
               include.mean = F, xreg = diff_dat[Period[,"Pre"], xvars])

summary(TF_101) #The transfer function model identified
(1-pnorm(abs(TF_101$coef)/sqrt(diag(TF_101$var.coef))))*2 #calculating p-values. pnorm and not pt used as estimation is via MLE which gives asymptotically normal estimates. details here https://stats.stackexchange.com/questions/8868/how-to-calculate-the-p-value-of-parameters-for-arima-model-in-r
TF_101$nobs


# Intervention Analysis ---------------------------------------------------

Int_101 = arima(diff_dat[Period[,"Int"], "s101"], order = c(1,0,0),
                include.mean = F, xreg = diff_dat[Period[,"Int"], 
                                                  c(xvars,"D_Ann")])

summary(Int_101) #The transfer function model identified
(1-pnorm(abs(Int_101$coef)/sqrt(diag(Int_101$var.coef))))*2 #calculating p-values. pnorm and not pt used as estimation is via MLE which gives asymptotically normal estimates. details here https://stats.stackexchange.com/questions/8868/how-to-calculate-the-p-value-of-parameters-for-arima-model-in-r
Int_101$nobs

Int_101_Auc = arima(diff_dat[Period[,"Int"], "s101"], order = c(1,0,0),
                    include.mean = F, xreg = diff_dat[Period[,"Int"], 
                                                      c(xvars,"D_Auc")])
summary(Int_101_Auc) #The transfer function model identified
(1-pnorm(abs(Int_101_Auc$coef)/sqrt(diag(Int_101_Auc$var.coef))))*2 #calculating p-values. pnorm and not pt used as estimation is via MLE which gives asymptotically normal estimates. details here https://stats.stackexchange.com/questions/8868/how-to-calculate-the-p-value-of-parameters-for-arima-model-in-r
Int_101_Auc$nobs

# Removing unnecessary variables ------------------------------------------
rm(mod_s101,fitwhite, fitwhite1,op, ar101)
