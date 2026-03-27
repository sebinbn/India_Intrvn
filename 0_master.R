# ************* Replication File *************************
# Masterfile that replicates the results in the working paper 
# Nidhiri(2024). Impact of Special OMOs on the Indian Yield Curve.



rm(list = ls())
DATA_RAW = "data/raw"
OUTPUT = "output"

# Stores start/end dates for pre-intervention and intervention periods.
AnalysisPeriod = as.Date(c("2018-04-01", "2019-11-30", "2019-12-01", "2021-06-30"))
names(AnalysisPeriod) = c("Pre_Start", "Pre_End", "Int_Start", "Int_End")

source("code/0_1_setup.R")

# Importing and Cleaning Data ----------------------------------------------------------

# Import yields and OIS rates.
source("code/1_YieldImport.R")

# Import covariates (EFFR, US10yr, Liq, WACR) and the sp. OMO announcement and auction dates.
source("code/1_CovariateImport.R")

# Merge the data, build Sp. OMO date dummies, and create the regression datasets.
source("code/1_MergeData.R")


# Analysis ----------------------------------------------------------------

# Print summary statistics.
source("code/2_SummaryStats.R")

# Store ADF statistics in `ADF_tab`.
# Critical values are printed from one test because they do not vary across these
# specifications.
source("code/2_ADFTest.R")

# Run the intervention analysis scripts.
# These scripts estimate the models, print the results, and store the fitted
# objects used in the summary tables.
source("code/2_IntAnalysisPrelims.R")
source("code/2_IntAnalysis_slope.R")
source("code/2_IntAnalysis_10yr.R")
source("code/2_IntAnalysis_1yr.R")
source("code/2_IntAnalysis_OIS1yr.R")
rm(build_lag_dataset)

# Tabulate the transfer-function and intervention-analysis results.
source("code/3_IntResults_tabulate.R")

## Int Analysis for individual events --------------------------------------

### Transfer function results filling ---------------------------------------

x = BBYield[BBYield$Date %in% c(Twist_Dates$Announcement,
                                  Twist_Dates$Announcement-1,
                                  Twist_Dates$Announcement -2), c("Date", "GIND10Y", "GIND1Y")]
y = MergedDat[
  MergedDat$Date %in% c(Twist_Dates$Announcement, Twist_Dates$Announcement - 1,
                        Twist_Dates$Announcement - 2),
  c("Date", "GIND10Y", "GIND1Y", "s101")]
z = MergedDat_diff[
  MergedDat_diff$Date %in% c(Twist_Dates$Announcement,
                             Twist_Dates$Announcement - 1,
                             Twist_Dates$Announcement - 2), 
  c("Date", "GIND10Y", "GIND1Y", "s101")]

# Run event study, no longer used
#source("code/2_EventStudy.R")

