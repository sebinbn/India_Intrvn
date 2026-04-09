# PURPOSE: 1. Runs ADF test for each variable in pre-intervention and intervention
#               periods
#
# INPUT: MergedDat, MergedDat_diff
# 
# OUTPUT: ADF_tab

# Preliminaries  ---------------------------------------------------------

# Specify the variables on which the ADF tests are run.
Vars_ADF = c("GIND10Y","GIND1Y","s101","IRSW1", "WACR", "Liq","DGS10", "EFFR")

ADF_tab = data.frame(
  Apr18_Nov19_Lvl_t       = rep(NA_real_, length(Vars_ADF)),
  Apr18_Nov19_Lvl_p       = rep(NA_real_, length(Vars_ADF)),
  Apr18_Nov19_1Diff_t     = rep(NA_real_, length(Vars_ADF)),
  Apr18_Nov19_1Diff_p     = rep(NA_real_, length(Vars_ADF)),
  Dec19_Jun21_Lvl_t       = rep(NA_real_, length(Vars_ADF)),
  Dec19_Jun21_Lvl_p       = rep(NA_real_, length(Vars_ADF)),
  Dec19_Jun21_1Diff_t     = rep(NA_real_, length(Vars_ADF)),
  Dec19_Jun21_1Diff_p     = rep(NA_real_, length(Vars_ADF)),
  row.names = Vars_ADF
)

# Running ADF test in a loop ----------------------------------------------

for (t in 1:2){ #loop over Pre and Int periods
  
  lvl_subset = MergedDat[MergedDat$Date >= AnalysisPeriod[2*t-1] &
                           MergedDat$Date <= AnalysisPeriod[2*t],]
  diff_subset = MergedDat_diff[MergedDat_diff$Date >= AnalysisPeriod[2*t-1] &
                                 MergedDat_diff$Date <= AnalysisPeriod[2*t],]
  
  for (Var in Vars_ADF){ #loop over each variable
    # ADF at levels
    adf_res = summary(ur.df(lvl_subset[,Var], type = "trend", selectlags = "AIC"))
    ADF_tab[Var,4*t-c(3,2)] = round(c(
      adf_res@testreg$coefficients["z.lag.1","t value"],
      punitroot(adf_res@teststat[,"tau3"], trend = "ct") ),3)
    
    # ADF at first diff
    adf_res = summary(ur.df(diff_subset[,Var], type = "trend", selectlags = "AIC"))
    ADF_tab[Var,4*t-c(1,0)] = round(c(
      adf_res@testreg$coefficients["z.lag.1","t value"],
      punitroot(adf_res@teststat[,"tau3"], trend = "ct") ),3)
    
  }
}

# Save output & Remove temp variables ------------------------------------------

# Save table
filename_path = file.path(OUTPUT, "ADF_results.csv")
write.csv(ADF_tab, file = filename_path )

message(sprintf("ADF test run on variables used in analysis. Result saved in %s",
                paste(getwd(),filename_path,sep = "/") ))


rm(Vars_ADF, lvl_subset, diff_subset,adf_res, t, Var, filename_path)