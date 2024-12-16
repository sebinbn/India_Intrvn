# This imports US data downloaded from FRED and some data downloaded from DBIE and
# dates of auctions/announcements.


# Import and clean FRED data ----------------------------------------------

EFFR_dat = read_xlsx("Cov_Data/EFFR.xlsx", sheet = "Daily", range = "A1:B1068")   #Importing EFFR from FRED downloaded excel file. range only till 2022 taken
US10yr_dat = read_xlsx("Cov_Data/DGS10.xlsx", sheet = "Daily", range = "A1:B1068")   #Importing US 10yr yield from FRED downloaded excel file. range only till 2022 taken

FREDclean = function(Dat){
  colnames(Dat)[1] = "Date"
  Dat["Date"] = as.Date(Dat[["Date"]])
  return(Dat)
}
EFFR_dat = FREDclean(EFFR_dat)
US10yr_dat = FREDclean(US10yr_dat)

# Import and clean DBIE data ----------------------------------------------

Liq_dat = read_xlsx("Cov_Data/WSS Table No. 08 _ Liquidity Operations by RBI.xlsx",
                    range = "B8:S2404" )
Liq_dat = Liq_dat[,c(1,18)]
colnames(Liq_dat) = c("Date", "Liq")
Liq_dat$Date = as.Date(Liq_dat$Date)
Liq_dat$Liq = Liq_dat$Liq/100000 #as number is very large, scaling down to lakhs of crores 

WACR_dat = read_xlsx("Cov_Data/HBS Table No. 220 _ Daily Weighted Average Call_Notice Money Rates.xlsx",
                     range = "B7:H1895" )
#the dates get converted to number. this is reversed and rows for years are removed.
WACR_dat = WACR_dat[!substr(WACR_dat$`1`,1,2) == '20',] #remove rows that are year headings
WACR_dat$`1` = as.Date("1900-01-01") + as.numeric(WACR_dat$`1`) - 2  #converting the numbers back to dates

# Below checks if the borrowing and lending rates are the same. There are are 16 
# days when they are not. These are minor differences and we use the borrowing rate 
WACR_dat[WACR_dat$`4` != WACR_dat$`7`,] 
WACR_dat = WACR_dat[,c(1,4)]
colnames(WACR_dat) = c("Date", "WACR")
WACR_dat$WACR = as.numeric(WACR_dat$WACR) #25Jan2024 has an error that is converted to NA. But it wouldn't matter for my analysis


# Importing Ann/Auc dates -------------------------------------------------

Twist_Dates = read_xlsx("Twist_dates.xlsx")
Twist_Dates = data.frame(lapply(Twist_Dates, as.Date))
