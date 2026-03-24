# This file runs ADF tests for each variable in pre-intervention and intervention
# periods.

ADF_tab = matrix(NaN, length(Vars_ADF), 4)
colnames(ADF_tab) = paste(c(rep("Apr18_Nov19",2), rep("Dec19_Jun21",2)),
                          rep(c("Lvl","1Diff"),2), sep = "_" )
rownames(ADF_tab) = Vars_ADF


# Running ADF test in a loop ----------------------------------------------

for (t in 1:2){ #loop over Pre and Int periods
  for (Var in Vars_ADF){ #loop over each variable
    adf_res = summary(ur.df(Merge_dat[Period[,t],Var], type = "trend", 
                            selectlags = "AIC") )
    ADF_tab[Var,2*t-1] = adf_res@testreg$coefficients["z.lag.1","t value"]
    adf_res = summary(ur.df(na.omit(diff(Merge_dat[Period[,t],Var])),
                            type = "trend", selectlags = "AIC") )
    ADF_tab[Var,2*t] = adf_res@testreg$coefficients["z.lag.1","t value"]
  }
}
print("The crtical values of all tests are:")
print(adf_res@cval)
# Removing excess variables -----------------------------------------------

rm(Vars_ADF, adf_res, t, Var)