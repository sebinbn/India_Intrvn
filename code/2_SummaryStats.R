# This file uses Merge_dat created by 1_MergeData.R

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

# Calculate Summary statistics  -------------------------------

cols_to_summ = !colnames(Merge_dat) %in% c("Date","D_Ann","D_Auc",
                                           paste0("D_Ann_", 1:24),
                                           paste0("D_Auc_", 1:24))
# Pre-intervention summary -------------------------------------------------

SummStats_tab = summ_stats(Merge_dat[Period[,"Pre"], cols_to_summ]) 

# Intervention summary -------------------------------------------------
SummStats_tab = rbind(SummStats_tab, 
                      summ_stats(Merge_dat[Period[,"Pre"], cols_to_summ])  )

write.csv(SummStats_tab, file.path(OUTPUT, "SummaryStats.csv"))

rm(summ_stats, cols_to_summ)