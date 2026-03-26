# This file uses MergedDat created by 1_MergeData.R

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

SummStats_tab = summ_stats(MergedDat[
  MergedDat$Date >= AnalysisPeriod["Pre_Start"] &
    MergedDat$Date <= AnalysisPeriod["Pre_End"],
  cols_to_summ]) 

SummStats_tab = rbind(SummStats_tab, 
                      summ_stats(MergedDat[
                        MergedDat$Date >= AnalysisPeriod["Int_Start"] &
                          MergedDat$Date <= AnalysisPeriod["Int_End"],
                        cols_to_summ])  )

# 2. Save Output ----------------------------------------------------

write.csv(SummStats_tab, file.path(OUTPUT, "SummaryStats.csv"), row.names = F)

rm(summ_stats, cols_to_summ)