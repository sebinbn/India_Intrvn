# this file performs intervention analysis on 10yr yield. Returns the results in TF_10.
# File follows 4-step process for first identifying transfer function model in 
# pre-intervention data.


Period = cbind(
  Pre = Merge_dat$Date >= as.Date("2018-04-01") & Merge_dat$Date <= as.Date("2019-11-30"),
  Int = Merge_dat$Date >= as.Date("2019-12-01") & Merge_dat$Date <= as.Date("2021-05-31") )

reg_dat = Merge_dat # Note: subsetting smaller time period within lm makes 0 NAs when lagging. So no subsetting here
reg_dat[c("EFFR_1","DGS10_1","DGS10_+1")] = 
  cbind(dplyr::lag(reg_dat$EFFR,n = 1), dplyr::lag(reg_dat$DGS10,n = 1),
        dplyr::lead(reg_dat$DGS10,n = 1) )

diff_dat = reg_dat[Period[,"Pre"],]
diff_indx = !colnames(diff_dat) %in% c("Date","Liq","D_Ann","D_Auc")
diff_dat[-1,diff_indx] = apply(diff_dat[,diff_indx],2,diff)
diff_dat = diff_dat[-1,]

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
ccf(fitwhite1,fitwhite, ylab = "CCF", xlab = "", main = "10yr-1yr ~ WACR", lag.max = 15)
fitwhite1 = residuals(Arima(Merge_dat$Liq[Period[,"Pre"]], model = mod_10y))
ccf(fitwhite1,fitwhite, ylab = "", xlab = "", main = "10yr-1yr ~ Liquidity", lag.max = 15)
fitwhite1 = residuals(Arima(Merge_dat$EFFR[Period[,"Pre"]], model = mod_10y))
ccf(fitwhite1,fitwhite, ylab = "CCF", main = "10yr-1yr ~ EFFR", lag.max = 15)
fitwhite1 = residuals(Arima(Merge_dat$DGS10[Period[,"Pre"]], model = mod_10y))
ccf(fitwhite1,fitwhite, lag.max = 15, ylab = "", main = "10yr-1yr ~ US10yr",)
par(mfrow =  op$mfrow, mai = op$mai) #reverting graphics options

## Step 2 - Linear regression with covariates -----------------------------------

# lags identified from CCF are used in regression

reg10 = lm(GIND10Y ~ Liq + DGS10, #+ DGS10_1 + `DGS10_+1`, 
            data = reg_dat[Period[,"Pre"],]) #adding Liq makes Liq_5 insignificant
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

TF_10 = arima(diff_dat$GIND10Y, order = c(2,0,0),fixed = c(0,NA,NA,NA,NA,NA),
              include.mean = F,
               xreg = diff_dat[c("Liq","EFFR_1", "DGS10","DGS10_1")])

summary(TF_10) #The transfer function model identified
BIC(TF_10)
(1-pnorm(abs(TF_10$coef[-1])/sqrt(diag(TF_10$var.coef))))*2 #calculating p-values. pnorm and not pt used as estimation is via MLE which gives asymptotically normal estimates. details here https://stats.stackexchange.com/questions/8868/how-to-calculate-the-p-value-of-parameters-for-arima-model-in-r

# Removing unnecessary variables ------------------------------------------

rm(Period,mod_10y,fitwhite, fitwhite1,op, reg_dat, ar10, diff_dat, diff_indx)
