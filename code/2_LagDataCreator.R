
# Helper for creating lagged values.-------------------------------------------

# Note for future that arimax() in TSA lets generate lags of xvars
build_lag_data <- function(df, xvars) {
  
  # identify variables to lag by checking if the name has an underscore followed
  # by a '+' (optional) [+], followed by a number [0-9]  
  lag_vars = unique(xvars[grepl("_[+]?[0-9]+$", xvars)]) 
  
  for (v in lag_vars) {
    if (v %in% names(df)) next # skip a round of for loop is lag already created
    base_var = sub("_[+]?[0-9]+$", "", v)
    lag_order = sub(".*_([+]?[0-9]+)$", "\\1", v)
    
    if (startsWith(lag_order, "+")) {
      df[[v]] = dplyr::lead(df[[base_var]], n = as.integer(sub("^\\+", "", lag_order)))
    } else {
      df[[v]] = dplyr::lag(df[[base_var]], n = as.integer(lag_order))
    }
  }
  
  df
}