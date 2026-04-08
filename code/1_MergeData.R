# PURPOSE: 1. Merge yield, covariates and create slope and dummy variables.
#          2. Create levels and first differenced data used later for analysis.
#
# INPUT: BBYield, OISYield, Liq_dat, WACR_dat, EFFR_dat, US10yr_dat
# 
# OUTPUT: MergedDat, MergedDat_diff
#

# 1. Create merged data file -------------------------------------------------

#susbset longer timeframe to  avoid loss of data when taking lags in Intervention Analysis 
MergedDat = BBYield[BBYield$Date >= as.Date("2018-02-01") &
                      BBYield$Date <= as.Date("2021-12-31"),
                    c("Date", "GIND10Y", "GIND1Y")]
MergedDat = merge(MergedDat, OISYield[c("Date", "IRSW1")], by = "Date", all.x = T)
MergedDat = merge(MergedDat, Liq_dat, by = "Date", all.x = T)
MergedDat = merge(MergedDat, WACR_dat, by = "Date", all.x = T)
MergedDat = merge(MergedDat, EFFR_dat, by = "Date", all.x = T)
MergedDat = merge(MergedDat, US10yr_dat, by = "Date", all.x = T) 


# 2. Handling NAs ------------------------------------------------------------

# Understanding where the NAs are
NA_counter = function(df, start_end){
  df = df[df$Date >= start_end[1] & df$Date <= start_end[2] , ]
  obs_count = nrow(df)
  na_count = colSums(is.na(df)) 
  tab_NA = data.frame(
    Variable        = names(na_count),
    NA_count        = na_count,
    NA_pct          = sprintf("%.1f %%", na_count/obs_count*100),
    row.names       = NULL
  )
  message(sprintf(
    "In the daily data sample from %s to %s with %d observations, missing values by variable:",
    start_end[1], start_end[2], obs_count
  ))
  print(tab_NA, row.names = FALSE)
}

NA_counter(MergedDat, AnalysisPeriod[c("Pre_Start","Pre_End")])
NA_counter(MergedDat, AnalysisPeriod[c("Int_Start","Int_End")])

MergedDat[names(MergedDat) != "Date"] = lapply(
  MergedDat[names(MergedDat) != "Date"], function(x) {
    zoo::na.locf(x, na.rm = FALSE)
  })
message("Missing values in MergedDat filled in by na.locf")

# 3. Creating new variables ---------------------------------------------

# Converting all interest rates to bps
MergedDat[!colnames(MergedDat) %in% c("Date", "Liq")] =
  MergedDat[!colnames(MergedDat) %in% c("Date", "Liq")] * 100

# Calculating slope
MergedDat$s101 = MergedDat$GIND10Y - MergedDat$GIND1Y

# Creating Dummies 
MergedDat[c("D_Ann","D_Auc")] = 0
MergedDat$D_Ann[MergedDat$Date %in% Twist_Dates$Announcement] = 1
MergedDat$D_Auc[MergedDat$Date %in% Twist_Dates$Auction] = 1


# Dummies for individual days
for (i in 1:nrow(Twist_Dates)){
  MergedDat[paste(c("D_Ann_","D_Auc_"),i,sep = "")] = 0
  MergedDat[MergedDat$Date == Twist_Dates$Announcement[i],
            paste("D_Ann_",i,sep = "")] = 1
  MergedDat[MergedDat$Date == Twist_Dates$Auction[i],
            paste("D_Auc_",i,sep = "")] = 1
}
 
# 4. Creating first differenced data ---------------------------

MergedDat_diff = MergedDat
diff_indx = !colnames(MergedDat_diff) %in%
  c("Date", "Liq", "D_Ann", "D_Auc",
    paste0("D_Ann_", 1:24), paste0("D_Auc_", 1:24))
MergedDat_diff[-1, diff_indx] = apply(MergedDat_diff[, diff_indx], 2, diff)
MergedDat_diff = MergedDat_diff[-1, ]

# Removing Unnecessary variables -----------------------------------------

rm(NA_counter, diff_indx,i)
