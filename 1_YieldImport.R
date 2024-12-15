# This imports data downloaded from Bloomberg

####Data import and cleaning####

BBYield = read_xlsx("Bloomberg data/INDYC_3mo30y.xlsx", range = 'A3:T1657')      # Data available for some variables upto 1998. But, importing data from 2018 Jan
#BBYield$Date = as.Date.POSIXct(BBYield[,1], format = "%Y-%m-%d") #unbale to convert this posixct
#as.Date(as.character(BBYield[1,1]))
BBYield[,-1] = lapply(BBYield[,-1], as.numeric)
colnames(BBYield)[1] = "Date"
colnames(BBYield)[-1] = c(substr(colnames(BBYield)[2:11],1,6),
                          substr(colnames(BBYield)[12:ncol(BBYield)],1,7) )     #renaming with shorter names
                                        
BBYield = BBYield[order(BBYield$Date),]
str(BBYield) #exploring the structure of the data
