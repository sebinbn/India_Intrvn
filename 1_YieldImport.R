# This imports data downloaded from Bloomberg. It then calculates slopes. 

####Data import and cleaning####

BBYield = read_xlsx("Bloomberg data/INDYC_3mo30y.xlsx", range = 'A3:T1657')      # Data available for some variables upto 1998. But, importing data from 2018 Jan
colnames(BBYield)[1] = "Date"
colnames(BBYield)[-1] = c(substr(colnames(BBYield)[2:11],1,6),
                          substr(colnames(BBYield)[12:ncol(BBYield)],1,7) )     #renaming with shorter names
BBYield$Date = as.Date(BBYield$Date, format = "%Y-%m-%d") 
BBYield[,-1] = lapply(BBYield[,-1], as.numeric)

BBYield = BBYield[order(BBYield$Date),]

#Calculating empirical slope
BBYield$s101 = BBYield$GIND10Y - BBYield$GIND1Y

