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
source("India_Intrvn/1_MergeData.R") #merge yield (10yr,1yr), OIS (1yr) and covariates and  dummies and create reg_dat and diff_dat dataframes

# Analysis ----------------------------------------------------------------
#print out summary stats
source("India_Intrvn/2_SummaryStats.R")

#store ADF stat values in ADF_tab. critical values are printed out from one of the
#tests. The critical values aren't changing across various ADF tests.
#the variables on which ADF test is to be conducted is specified
Vars_ADF = c("GIND10Y","GIND1Y","s101","IRSW1", "WACR", "Liq","DGS10", "EFFR")
source("India_Intrvn/2_ADFTest.R")


## Creating indices for subsetting data ----------------------------------

# the pre-intervention and intervention periods are specified. Since merge_dat has
# same date index as reg_dat, the following logical works to subset that dataframe
# as well.
Period = cbind(
  Pre = Merge_dat$Date >= as.Date("2018-04-01") & Merge_dat$Date <= as.Date("2019-11-30"),
  Int = Merge_dat$Date >= as.Date("2019-12-01") & Merge_dat$Date <= as.Date("2021-06-30") )

# Specifying pre-intervention and intervention periods for differenced data
Period_diff = cbind(
  Pre = diff_dat$Date >= as.Date("2018-04-01") & diff_dat$Date <= as.Date("2019-11-30"),
  Int = diff_dat$Date >= as.Date("2019-12-01") & diff_dat$Date <= as.Date("2021-06-30") )

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

#the following uses lists created by above and returns tables TF_tab, Int_tab and Int_Auc_tab
source("India_Intrvn/3_IntResults_tabulate.R")

## Int Analysis for individual events --------------------------------------


### Transfer function results filling ---------------------------------------

x = BBYield[BBYield$Date %in% c(Twist_Dates$Announcement,
                                  Twist_Dates$Announcement-1,
                                  Twist_Dates$Announcement -2), c("Date", "GIND10Y", "GIND1Y")]
y = Merge_dat[Merge_dat$Date %in% c(Twist_Dates$Announcement,
                                Twist_Dates$Announcement-1,
                                Twist_Dates$Announcement -2), c("Date", "GIND10Y", "GIND1Y", "s101")]
z = diff_dat[diff_dat$Date %in% c(Twist_Dates$Announcement,
                                    Twist_Dates$Announcement-1,
                                    Twist_Dates$Announcement -2), c("Date", "GIND10Y", "GIND1Y", "s101")]



source("India_Intrvn/2_EventStudy.R")

