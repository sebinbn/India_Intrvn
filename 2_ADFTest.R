# This file runs ADF tests for each variable in pre-intervention and intervention
# periods.

Vars_ADF = c("GIND10Y","GIND1Y","s101","WACR", "Liq","DGS10", "EFFR")
  
ADF_tab = matrix(NaN, length(Vars_ADF), 4)
colnames(ADF_tab) = paste(c(rep("Apr18_Nov19",2), rep("Dec19_May21",2)),
                          rep(c("Lvl","1Diff"),2), sep = "_" )
rownames(ADF_tab) = Vars_ADF
# Specifying intervention and pre-intervention periods
Periods = cbind(
  Merge_dat$Date >= as.Date("2018-04-01") & Merge_dat$Date <= as.Date("2019-11-30"),
  Merge_dat$Date >= as.Date("2019-12-01") & Merge_dat$Date <= as.Date("2021-05-31") )


# Running ADF test in a loop ----------------------------------------------

for (t in 1:2){
  for (Var in Vars_ADF){
    adf_res = summary(ur.df(Merge_dat[Periods[,t],Var], type = "trend", 
                            selectlags = "AIC") )
    ADF_tab[Var,2*t-1] = adf_res@testreg$coefficients["z.lag.1","t value"]
    adf_res = summary(ur.df(na.omit(diff(Merge_dat[Periods[,t],Var])),
                            type = "trend", selectlags = "AIC") )
    ADF_tab[Var,2*t] = adf_res@testreg$coefficients["z.lag.1","t value"]
  }
}
print("The crtical values of all tests are:")
print(adf_res@cval)
# Removing excess variables -----------------------------------------------

rm(Vars_ADF,Periods, adf_res, t, Var)