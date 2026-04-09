# This file uses Mergeddat created by 1_MergeData.R

# Function to generate and tabulate summary statistics -------------------------

summ_stats <- function(df) {
  
  df = df[, names(df) != "Date", drop = F]
  Stats_tab = data.frame(matrix(nrow = length(names(df)), ncol = 6))
  colnames(Stats_tab) = c("Variable", "Mean", "Median", "Std. Devn", "Min", "Max")
  
  for (i in seq_along(df)) {
   Stats_tab[i, ] = c(names(df)[i],
                       round(mean(df[[i]]), 2),
                       round(median(df[[i]]), 2),
                       round(sd(df[[i]]), 2),
                       round(min(df[[i]]), 2),
                       round(max(df[[i]]), 2))
  }
  
  Stats_tab
}

# 1. Calculate Summary statistics for Pre and Int ------------------------------

cols_to_summ = !colnames(MergedDat) %in% c("Date","D_Ann","D_Auc",
                                           paste0("D_Ann_", 1:24),
                                           paste0("D_Auc_", 1:24))

Summ_pre = summ_stats(MergedDat[
  MergedDat$Date >= AnalysisPeriod["Pre_Start"] &
    MergedDat$Date <= AnalysisPeriod["Pre_End"],
  cols_to_summ]) 
Summ_pre$Variable = paste0(Summ_pre$Variable, "_Pre")

Summ_int = summ_stats(MergedDat[
  MergedDat$Date >= AnalysisPeriod["Int_Start"] &
    MergedDat$Date <= AnalysisPeriod["Int_End"],
  cols_to_summ]) 
Summ_int$Variable = paste0(Summ_int$Variable, "_Int")

SummStats_tab = rbind(Summ_pre, Summ_int)
                     
# 2. Save Output & Remove temp variables --------------------------------------

filename_path = file.path(OUTPUT, "SummaryStats.csv")
write.csv(SummStats_tab, filename_path , row.names = F)
message(sprintf("Summary stats for both Pre-Int and Int periods calculaed, saved in %s",
                paste(getwd(),filename_path,sep = "/") ))

rm(summ_stats, cols_to_summ, Summ_pre, Summ_int, filename_path)