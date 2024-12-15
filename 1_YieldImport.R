# This imports data

####Data import and cleaning####

BBYield = read_xlsx("Bloomberg data/INDYC_3mo30y.xlsx", range = 'A3:T1657')      # Data available for some varaiables upto 1988. But, importing data from 2018 Jan
BBYield$Date = as.character(BBYield[,1]) #converting POSIXct to Date
BBYield[,-1] = lapply(BBYield[,-1], as.numeric)
colnames(BBYield)[1] = "Date"
colnames(BBYield)[-1] = c(substr(colnames(BBYield)[2:11],1,6),
                          substr(colnames(BBYield)[12:ncol(BBYield)],1,7) )     #renaming with shorter names
                                        
BBYield = BBYield[order(BBYield$Date),]
str(BBData) #exploring the structure of the data

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
