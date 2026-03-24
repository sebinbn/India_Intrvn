# ************* Replication File *************************
# Masterfile that replicates the results in the working paper 
# Nidhiri(2024). Impact of Special OMOs on the Indian Yield Curve.



rm(list = ls())
DATA_RAW = "data/raw"

source("code/0_1_setup.R")

# Importing and Cleaning Data ----------------------------------------------------------

# Import yields and OIS rates.
source("code/1_YieldImport.R")

# Import covariates (EFFR, US10yr, Liq, WACR) and the sp. OMO announcement and auction dates.
source("code/1_CovariateImport.R")

# Merge the data, build Sp. OMO date dummies, and create the regression datasets.
source("code/1_MergeData.R")


# Analysis ----------------------------------------------------------------

## Creating indices for subsetting data ----------------------------------

# Specify the pre-intervention and intervention periods.
# Since `Merge_dat` has the same date index as `reg_dat`, this logical indexing
# also works for subsetting that dataframe.
Period = cbind(
  Pre = Merge_dat$Date >= as.Date("2018-04-01") & Merge_dat$Date <= as.Date("2019-11-30"),
  Int = Merge_dat$Date >= as.Date("2019-12-01") & Merge_dat$Date <= as.Date("2021-06-30") )

# Specifying pre-intervention and intervention periods for differenced data
# Note that index changes even through subset condition is same
Period_diff = cbind(
  Pre = diff_dat$Date >= as.Date("2018-04-01") & diff_dat$Date <= as.Date("2019-11-30"),
  Int = diff_dat$Date >= as.Date("2019-12-01") & diff_dat$Date <= as.Date("2021-06-30") )

# Print summary statistics.
source("code/2_SummaryStats.R")

# Specify the variables on which the ADF tests are run.
Vars_ADF = c("GIND10Y","GIND1Y","s101","IRSW1", "WACR", "Liq","DGS10", "EFFR")

# Store ADF statistics in `ADF_tab`.
# Critical values are printed from one test because they do not vary across these
# specifications.
source("code/2_ADFTest.R")


## Transfer fn identification & Int Analysis on each dependent var -------------

#Initiate matrices to store results from analysis
Int_Ann_Indiv = matrix(NaN, nrow = nrow(Twist_Dates), ncol = 12)
Int_Auc_Indiv = matrix(NaN, nrow = nrow(Twist_Dates), ncol = 12)

# Run the intervention analysis scripts.
# These scripts estimate the models, print the results, and store the fitted
# objects used in the summary tables.

source("code/2_IntAnalysis_slope.R")
source("code/2_IntAnalysis_10yr.R")
source("code/2_IntAnalysis_1yr.R")
source("code/2_IntAnalysis_OIS1yr.R")

# Tabulate the transfer-function and intervention-analysis results.
source("code/3_IntResults_tabulate.R")

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


# Run event study, no longer used
source("code/2_EventStudy.R")

