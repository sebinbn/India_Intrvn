# This file merges all data to create Merge_dat and creates dummies as columns in
# merged data.

Merge_dat = BBYield[BBYield$Date >= as.Date("2018-03-28") & 
                      BBYield$Date <= as.Date("2021-05-31"),
                    c("Date","GIND10Y", "GIND1Y", "s101")]
Merge_dat = merge(Merge_dat,Liq_dat, by = "Date", all.x = T)
Merge_dat = merge(Merge_dat,WACR_dat, by = "Date", all.x = T)
Merge_dat = merge(Merge_dat,EFFR_dat, by = "Date", all.x = T)
Merge_dat = merge(Merge_dat,US10yr_dat, by = "Date", all.x = T)


# Creating Dummies --------------------------------------------------------

Merge_dat[c("D_Ann","D_Auc")] = 0
Merge_dat$D_Ann[Merge_dat$Date %in% Twist_Dates$Announcement] = 1
Merge_dat$D_Auc[Merge_dat$Date %in% Twist_Dates$Auction] = 1
