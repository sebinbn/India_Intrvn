# This imports US data downloaded from FRED and some data downloaded from DBIE.
# dates of auctions/announcements

EFFR_dat = read_xlsx("Cov_Data/EFFR.xlsx", sheet = "Daily", range = "A1:B1068")   #Importing EFFR from FRED downloaded excel file. range only till 2022 taken
US10yr_dat = read_xlsx("Cov_Data/DGS10.xlsx", sheet = "Daily", range = "A1:B1068")   #Importing US 10yr yield from FRED downloaded excel file. range only till 2022 taken

FREDclean = function(Dat){
  colnames(Dat)[1] = "Date"
  Dat["Date"] = as.Date(Dat[["Date"]])
  return(Dat)
}
EFFR_dat = FREDclean(EFFR_dat)
US10yr_dat = FREDclean(US10yr_dat)

Liq_dat = read_xlsx
##Converting data to time series##
all_ts = as.xts(alldata[,-1], order.by = alldata$Date)
#This data is for all business days in India. Some of these dates were holidays 
#in the USA and therefore the variables pertaining to these dates have NAs. The
#following replaces NAs with previous value
all_ts$US10yr = na.locf(all_ts$US10yr)
all_ts$EFFR = na.locf(all_ts$EFFR)
all_ts$Liquidity = all_ts$Liquidity/100000 #as number is very large, scaling down to lakhs of crores 

#Importing data on dates of auctions and announcements
dummies = read_xlsx("C:/Users/sbnidhir/OneDrive - University Of Houston/MPhil Thesis/Data/Twist_dummies.xlsx", 
                    col_types = c("date","numeric", "numeric","numeric"))
dummies_ts = as.xts(dummies[,-1], order.by = dummies$Date)

#Calculating empirical slope
slope102 = all_ts$`10yr` - all_ts$`2yr`
slope101 = all_ts$`10yr` - all_ts$`1yr`
slope103 = all_ts$`10yr` - all_ts$`3mo`