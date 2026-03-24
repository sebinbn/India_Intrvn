# This file uses Merge_dat created by 1_MergeData.R

cols_to_summ = !colnames(Merge_dat) %in% c("Date","D_Ann","D_Auc")
# Pre-intervention summary -------------------------------------------------
print("Pre-Intervention Summary")
print(summary(Merge_dat[Period[,"Pre"], cols_to_summ]) )
print("Std.Devn")
print(apply(Merge_dat[Period[,"Pre"],cols_to_summ], 2, sd))

# Intervention summary -------------------------------------------------
print("Intervention Summary")
print(summary(Merge_dat[Period[,"Int"],cols_to_summ]) )
print("Std.Devn")
print(apply(Merge_dat[Period[,"Int"],cols_to_summ], 2, sd))

rm(cols_to_summ)