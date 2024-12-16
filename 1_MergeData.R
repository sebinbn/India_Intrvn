# This file merges all data to create Merge_dat and creates dummies as columns in
# merged data.

Merge_dat = BBYield[BBYield$Date >= as.Date("2018-03-28") & 
                      BBYield$Date <= as.Date("2021-05-31"),
                    c("Date","GIND10Y", "GIND1Y")]
Merge_dat = merge(Merge_dat,Liq_dat, by = "Date", all.x = T)
Merge_dat = merge(Merge_dat,WACR_dat, by = "Date", all.x = T)
Merge_dat = merge(Merge_dat,EFFR_dat, by = "Date", all.x = T)
Merge_dat = merge(Merge_dat,US10yr_dat, by = "Date", all.x = T)


# Handling NAs ------------------------------------------------------------

# Days when GIND10Y is missing is omitted.
# Remaining EFFR.DGS10 missing is filled with prev non-NA.
# Remaining GIND1Y missing is interpolated.

Merge_dat = Merge_dat[!is.na(Merge_dat$GIND10Y),]
Merge_dat[c("EFFR", "DGS10")] = na.locf(Merge_dat[c("EFFR", "DGS10")])
Merge_dat$GIND1Y = na.approx(Merge_dat$GIND1Y, na.rm = F)

colSums(is.na(Merge_dat))
# # following was used to visualize where missing values are to decide how to handle missin values
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
BBYield$s101 = BBYield$GIND10Y - BBYield$GIND1Y

# Creating Dummies --------------------------------------------------------

Merge_dat[c("D_Ann","D_Auc")] = 0
Merge_dat$D_Ann[Merge_dat$Date %in% Twist_Dates$Announcement] = 1
Merge_dat$D_Auc[Merge_dat$Date %in% Twist_Dates$Auction] = 1
