# This file uses Merge_dat created by 1_MergeData.R


# Pre-intervention summary -------------------------------------------------
print("Pre-Intervention Summary")
print(summary(Merge_dat[Merge_dat$Date >= as.Date("2018-04-01") & 
                    Merge_dat$Date <= as.Date("2019-11-30"),
                  !colnames(Merge_dat) %in% c("Date","D_Ann","D_Auc")]))
print("Std.Devn")
print(apply(Merge_dat[Merge_dat$Date >= as.Date("2018-04-01") & 
            Merge_dat$Date <= as.Date("2019-11-30"),
          !colnames(Merge_dat) %in% c("Date","D_Ann","D_Auc")], 2, sd))

# Intervention summary -------------------------------------------------
print("Intervention Summary")
print(summary(Merge_dat[Merge_dat$Date >= as.Date("2019-12-01") & 
                    Merge_dat$Date <= as.Date("2021-05-31"),
                  !colnames(Merge_dat) %in% c("Date","D_Ann","D_Auc")]))
print("Std.Devn")
print(apply(Merge_dat[Merge_dat$Date >= as.Date("2019-12-01") & 
                        Merge_dat$Date <= as.Date("2021-05-31"),
                      !colnames(Merge_dat) %in% c("Date","D_Ann","D_Auc")], 2, sd))
