# This is the masterfile that uses other files to replicate the results in the 
# working paper Nidhiri(2024). Impact of Special OMOs.

library(readxl) #to import from .xlsx
library(ggplot2)
library(zoo) #for using na.locf
library(urca) #for ur.df
library(forecast) #for auto.arima, Arima (needed to use model option within Arima, not available in stats::arima)

#setwd('C:/Users/sebin/OneDrive - University Of Houston/Research/2nd yr paper/Analysis')
setwd('../')
rm(list = ls())

# Importing Data ----------------------------------------------------------

source("India_Intrvn/1_YieldImport.R") #imports yields and OIS rates
source("India_Intrvn/1_CovariateImport.R") #import EFFR, US10yr, Liq, WACR, and dates of Sp.OMO
source("India_Intrvn/1_MergeData.R") #merge yield (10yr,1yr), OIS (1yr) and covariates and  dummies

# Analysis ----------------------------------------------------------------
#print out summary stats
source("India_Intrvn/2_SummaryStats.R")

#store ADF stat values in ADF_tab. critical values are printed out from one of the
#tests. The critical values aren't changing across various ADF tests.
#the variables on which ADF test is to be conducted is specified
Vars_ADF = c("GIND10Y","GIND1Y","s101","IRSW1", "WACR", "Liq","DGS10", "EFFR")
source("India_Intrvn/2_ADFTest.R")

# the pre-intervention and intervention periods are specified. Since merge_dat has
# same date index as reg_dat and diff_dat created below, the following logical 
# works to subset those dataframes as well.
Period = cbind(
  Pre = Merge_dat$Date >= as.Date("2018-04-01") & Merge_dat$Date <= as.Date("2019-11-30"),
  Int = Merge_dat$Date >= as.Date("2019-12-01") & Merge_dat$Date <= as.Date("2021-06-30") )

## Creating dataframes needed for analysis ---------------------------------

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


## Transfer fn identification & Int Analysis on each dependent var -------------

Int_Ann_Indiv = matrix(NaN, nrow = nrow(Twist_Dates), ncol = 12)
Int_Auc_Indiv = matrix(NaN, nrow = nrow(Twist_Dates), ncol = 12)
#These files run analysis, store results in List items and also print out the results
#The results of individual date intervention analysis is kept in lists and also
#used to create a table with just the coefficient of the dummy. 
source("India_Intrvn/2_IntAnalysis_slope.R")
source("India_Intrvn/2_IntAnalysis_10yr.R")
source("India_Intrvn/2_IntAnalysis_1yr.R")
source("India_Intrvn/2_IntAnalysis_OIS1yr.R")

source("India_Intrvn/3_IntResults_tabulate.R")
## Int Analysis for individual events --------------------------------------


### Transfer function results filling ---------------------------------------




source("India_Intrvn/2_EventStudy.R")

