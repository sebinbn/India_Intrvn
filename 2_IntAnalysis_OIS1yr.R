# this file performs intervention analysis on 1yr OIS rate. Returns the transfer
# function in TF_OIS and Int analysis in Int_OIS.
# I find that OIS yield is a random walk. So, transfer fn has no ARIMA structure
# and is therefore a linear regression after first differencing non-stationary
# variables.


# Identify pre-intervention transfer function -----------------------------

## Step 1: Identify ARIMA model -------------------------------------------
auto.arima(Merge_dat$IRSW1[Period[,"Pre"]])#suggests 1,2,2
#ACF and PACF checked to make own judgement regarding order
acf(na.omit(diff(Merge_dat$IRSW1[Period[,"Pre"]])))
pacf(na.omit(diff(Merge_dat$IRSW1[Period[,"Pre"]]))) #ACF and PACF at weird lags significant. No conclusion made.
mod_IRSW1 = arima(Merge_dat$IRSW1[Period[,"Pre"]], order = c(0,1,0)) #1,1,0 > 0,1,1 in BIC. 1,1,1 has both insigniicant and of opposite signs.
mod_IRSW1
BIC(mod_IRSW1)
acf(mod_IRSW1$residuals);pacf(mod_IRSW1$residuals) #ACF and PACF show residuals are white noise
plot(Merge_dat$IRSW1[Period[,"Pre"]])

### Step 1.2 Cross Correlation Function -----------------------------------------
op = par()
par(mfrow = c(2,2), mai = c(0.7,0.7,0.5,0.1))
fitwhite = residuals(Arima(Merge_dat$IRSW1[Period[,"Pre"]], model = mod_IRSW1))
fitwhite1 = residuals(Arima(Merge_dat$WACR[Period[,"Pre"]], model = mod_IRSW1))
ccf(fitwhite1,fitwhite, ylab = "CCF", xlab = "", main = "1yr OIS ~ WACR", lag.max = 15)
fitwhite1 = residuals(Arima(Merge_dat$Liq[Period[,"Pre"]], model = mod_IRSW1))
ccf(fitwhite1,fitwhite, ylab = "", xlab = "", main = "1yr OIS ~ Liquidity", lag.max = 15)
fitwhite1 = residuals(Arima(Merge_dat$EFFR[Period[,"Pre"]], model = mod_IRSW1))
ccf(fitwhite1,fitwhite, ylab = "CCF", main = "1yr OIS ~ EFFR", lag.max = 15)
fitwhite1 = residuals(Arima(Merge_dat$DGS10[Period[,"Pre"]], model = mod_IRSW1))
ccf(fitwhite1,fitwhite, lag.max = 15, ylab = "", main = "1yr OIS ~ US10yr",)
par(mfrow =  op$mfrow, mai = op$mai) #reverting graphics options

## Step 2 - Linear regression with covariates -----------------------------------

# lags identified from CCF are used in regression
regOIS = lm(IRSW1 ~ Liq  +  WACR_3 +  DGS10 + DGS10_1, 
            data = reg_dat[Period[,"Pre"],]) #adding Liq makes Liq_5 insignificant
summary(regOIS)

## Step 3 - ARIMA model on Linear regression errors-----------------------------
acf(diff(regOIS$residuals)); pacf(diff(regOIS$residuals)) #no conclusion can be made
auto.arima(regOIS$residuals)#suggest ARIMA 3,1,0 

arOIS = arima(regOIS$residuals, order = c(0,1,0))
# All ARIMA (1,1,1 and 2,1,2 ) have equal and opposite signs. Series is random walk.
summary(arOIS)
BIC(arOIS)

## Step 4 - Fitting Transfer function ------------------------------------------
summary(lm(IRSW1 ~ 0 + Liq + Liq_1 + Liq_2 + WACR + WACR_1 + WACR_2 + EFFR + EFFR_1 +
             EFFR_2 + DGS10 + DGS10_1 + DGS10_2, data = diff_dat[Period[,"Pre"],]) )
summary(lm(IRSW1 ~ 0 + Liq + Liq_1 + WACR + WACR_1 + EFFR + EFFR_1 +
              DGS10 + DGS10_1 , data = diff_dat[Period[,"Pre"],]))
TF_OIS = lm(IRSW1 ~ 0 + Liq + WACR_3 + DGS10 + DGS10_1, data = diff_dat[Period[,"Pre"],])
summary(TF_OIS) #The transfer function model identified
length(TF_OIS$residuals)


# Intervention Analysis ---------------------------------------------------

Int_OIS = lm(IRSW1 ~ 0 + Liq + WACR_3 + DGS10 + DGS10_1 + D_Ann,
             data = diff_dat[Period[,"Int"],])
summary(Int_OIS) #The transfer function model identified
length(Int_OIS$residuals)

Int_OIS_Auc = lm(IRSW1 ~ 0 + Liq + WACR_3 + DGS10 + DGS10_1 + D_Auc,
                 data = diff_dat[Period[,"Int"],])
summary(Int_OIS_Auc) #The transfer function model identified
length(Int_OIS_Auc$residuals)

# Removing unnecessary variables ------------------------------------------
rm(mod_IRSW1,fitwhite, fitwhite1,op, arOIS)
