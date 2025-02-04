# This imports data on yields and OIS rates downloaded from Bloomberg.

####Data import and cleaning####

BBYield = read_xlsx("Bloomberg data/INDYC_3mo30y.xlsx", range = 'A3:T1657')      # Data available for some variables upto 1998. But, importing data from 2018 Jan
colnames(BBYield)[1] = "Date"
colnames(BBYield)[-1] = c(substr(colnames(BBYield)[2:11],1,6),
                          substr(colnames(BBYield)[12:ncol(BBYield)],1,7) )     #renaming with shorter names
BBYield$Date = as.Date(BBYield$Date, format = "%Y-%m-%d") 
BBYield[,-1] = lapply(BBYield[,-1], as.numeric)

BBYield = BBYield[order(BBYield$Date),]

#Importing OIS rates
OISYield = read_xlsx("Bloomberg data/IND_OIS_1mo1y.xlsx", range = 'A3:F1662')      # Data available for some variables upto 1998. But, importing data from 2018 Jan
colnames(OISYield)[1] = "Date"
colnames(OISYield)[-1] = c(substr(colnames(OISYield)[2],1,6),
                          substr(colnames(OISYield)[3:6],1,5) )     #renaming with shorter names
OISYield$Date = as.Date(OISYield$Date, format = "%Y-%m-%d") 
OISYield[,-1] = lapply(OISYield[,-1], as.numeric)

OISYield = OISYield[order(OISYield$Date),]

