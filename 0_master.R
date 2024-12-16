# This is the masterfile that uses other files to replicate the results in the 
# working paper Nidhiri(2024). Impact of Special OMOs.

library(readxl) #to import from .xlsx

#setwd('C:/Users/sebin/OneDrive - University Of Houston/Research/2nd yr paper/Analysis')
setwd('../')


# Importing Data ----------------------------------------------------------

source("India_Intrvn/1_YieldImport.R")
source("India_Intrvn/1_CovariateImport.R")
source("India_Intrvn/1_DummyCreate.R")


# Analysis ----------------------------------------------------------------


