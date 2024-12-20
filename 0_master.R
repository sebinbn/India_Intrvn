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

source("India_Intrvn/1_YieldImport.R")
source("India_Intrvn/1_CovariateImport.R")
source("India_Intrvn/1_MergeData.R")

# Analysis ----------------------------------------------------------------
#print out summary stats
source("India_Intrvn/2_SummaryStats.R")

#store ADF stat values in ADF_tab. critical values are printed out from one of the
#tests. The critical values aren't changing across various ADF tests.
source("India_Intrvn/2_ADFTest.R")

# The CCFs show significance at odd lags. These are given less weightage. Transfer
# function is initially fit with some lags and then lags reduced.
source("India_Intrvn/2_IntAnalysis.R")
source("India_Intrvn/2_EventStudy.R")