# This file merges all data to create Merge_dat and creates dummies as columns in
# merged data.It then creates reg_dat (with lagged variables added) and 
# diff_dat (first differenced data) 

Merge_dat = BBYield[BBYield$Date >= as.Date("2018-02-01") & 
                      BBYield$Date <= as.Date("2021-06-30"),
                    c("Date","GIND10Y", "GIND1Y")] #susbsetting from Feb 2018 to avoid loss of data when taking lags in Intervention Analysis 
Merge_dat = merge(Merge_dat,OISYield[c("Date","IRSW1")], by = "Date", all.x = T)
Merge_dat = merge(Merge_dat,Liq_dat, by = "Date", all.x = T)
Merge_dat = merge(Merge_dat,WACR_dat, by = "Date", all.x = T)
Merge_dat = merge(Merge_dat,EFFR_dat, by = "Date", all.x = T)
Merge_dat = merge(Merge_dat,US10yr_dat, by = "Date", all.x = T)


# Handling NAs ------------------------------------------------------------

# Days when GIND10Y is missing is omitted.
# Remaining EFFR,DGS10 and GIND1y missing is filled with prev non-NA.

Merge_dat = Merge_dat[!is.na(Merge_dat$GIND10Y),]
Merge_dat[c("EFFR", "DGS10", "GIND1Y")] = na.locf(Merge_dat[c("EFFR", "DGS10","GIND1Y")]) #these are the only three with missing values now
#Merge_dat$GIND1Y = na.approx(Merge_dat$GIND1Y, na.rm = F)
#note that GIND1y interpolation is giving weaker results
colSums(is.na(Merge_dat))
# # following was used to visualize where missing values are to decide how to handle missing values
# ggplot(Merge_dat, aes(x=Date, y = GIND10Y))+
#   geom_line()
# miss_dates = data.frame(Date = Merge_dat$Date, Miss = is.na(Merge_dat$GIND1Y))
# ggplot(miss_dates, aes(x = Date)) +
#   geom_blank() +  # Start with a blank plot to set up the x-axis
#   geom_vline(data = subset(miss_dates, Miss == TRUE),  # Filter for TRUE values in Miss
#              aes(xintercept = as.numeric(Date)), color = "red", linetype = "dashed") +
#   scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +  # Adjust x-axis labels
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis text
        

# Calculating empirical slope ---------------------------------------------

#Converting to bps all interest rates
Merge_dat[!colnames(Merge_dat) %in% c("Date","Liq")] = 
  Merge_dat[!colnames(Merge_dat) %in% c("Date","Liq")]*100
Merge_dat$s101 = Merge_dat$GIND10Y - Merge_dat$GIND1Y


# Creating Dummies --------------------------------------------------------

Merge_dat[c("D_Ann","D_Auc")] = 0
Merge_dat$D_Ann[Merge_dat$Date %in% Twist_Dates$Announcement] = 1
Merge_dat$D_Auc[Merge_dat$Date %in% Twist_Dates$Auction] = 1
Merge_dat$D_Auc[Merge_dat$Date == Twist_Dates$Auction[24] + 2] = 1 #last auction was on Saturday. capturing the impact on next monday
# Merge_dat$D_Ann[Merge_dat$Date %in% Twist_Dates$Announcement] = 1/nrow(Twist_Dates)
# Merge_dat$D_Auc[Merge_dat$Date %in% Twist_Dates$Auction] = 1/nrow(Twist_Dates)
# Merge_dat$D_Auc[Merge_dat$Date == Twist_Dates$Auction[24] + 2] = 1/nrow(Twist_Dates)


# Dummies for individual days
for (i in 1:nrow(Twist_Dates)){
  Date_Ann = Twist_Dates$Announcement[i]
  Date_Auc = Twist_Dates$Auction[i]
  Merge_dat[paste(c("D_Ann_","D_Auc_"),i,sep = "")] = 0
  Merge_dat[Merge_dat$Date == Date_Ann, paste("D_Ann_",i,sep = "")] = 1
  Merge_dat[Merge_dat$Date == Date_Auc, paste("D_Auc_",i,sep = "")] = 1
}
Merge_dat[Merge_dat$Date == Twist_Dates$Auction[24] + 2, "D_Auc_24"] = 1 #capturing last auction tht happened on Saturday on next Monday


# Creating other dataframes needed for analysis ---------------------------

# Required lagged variables are created and stored in reg_dat. This dataframe is 
# subsetted to run linear regression in pre-int period.

reg_dat = Merge_dat # Note: subsetting smaller time period within lm makes no NAs when lagging. So not subsetting here
reg_dat[c("WACR_1","Liq_1","EFFR_1","DGS10_1",
          "WACR_2","Liq_2","EFFR_2","DGS10_2",
          "WACR_3", "Liq_5", "DGS10_+1", "DGS10_5",
          "WACR_12","EFFR_12")] = 
  cbind(dplyr::lag(reg_dat$WACR,n = 1), dplyr::lag(reg_dat$Liq,n = 1),
        dplyr::lag(reg_dat$EFFR,n = 1),dplyr::lag(reg_dat$DGS10,n = 1),
        dplyr::lag(reg_dat$WACR,n = 2), dplyr::lag(reg_dat$Liq,n = 2),
        dplyr::lag(reg_dat$EFFR,n = 2),dplyr::lag(reg_dat$DGS10,n = 2),
        dplyr::lag(reg_dat$WACR,n = 3),dplyr::lag(reg_dat$Liq,n = 5),
        dplyr::lead(reg_dat$DGS10,n = 1),dplyr::lag(reg_dat$DGS10,n = 5),
        dplyr::lag(reg_dat$WACR,n = 12), dplyr::lag(reg_dat$EFFR,n = 12))

# need differenced data to have some variables in levels in transfer function and
# intervention analysis. Specifying differencing order in arima differences all
# variables
diff_dat = reg_dat
diff_indx = !colnames(diff_dat) %in% 
  c("Date","Liq","Liq_1","Liq_2","Liq_5","D_Ann","D_Auc",
    paste("D_Ann_", 1:24, sep = ""),paste("D_Auc_", 1:24, sep = "")) #variables that are not to be first differenced
diff_dat[-1,diff_indx] = apply(diff_dat[,diff_indx],2,diff)
diff_dat = diff_dat[-1,]


# Removing Unnecessary varaiables -----------------------------------------


rm(Date_Ann, Date_Auc)
