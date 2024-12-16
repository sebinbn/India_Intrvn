# This file uses Merge_dat created by 1_MergeData.R

summary(Merge_dat[Merge_dat$Date >= as.Date("2018-04-01") & 
                    Merge_dat$Date <= as.Date("2019-11-30"),])
