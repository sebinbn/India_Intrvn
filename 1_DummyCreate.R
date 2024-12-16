# This file creates dummy variable denoting dates of announcement/auction.

Twist_Dates = read_xlsx("Twist_dates.xlsx")
Twist_Dates = data.frame(lapply(Twist_Dates, as.Date))


##Converting data to time series##
all_ts = as.xts(alldata[,-1], order.by = alldata$Date)
#This data is for all business days in India. Some of these dates were holidays 
#in the USA and therefore the variables pertaining to these dates have NAs. The
#following replaces NAs with previous value
all_ts$US10yr = na.locf(all_ts$US10yr)
all_ts$EFFR = na.locf(all_ts$EFFR)
all_ts$Liquidity = all_ts$Liquidity/100000 #as number is very large, scaling down to lakhs of crores 
