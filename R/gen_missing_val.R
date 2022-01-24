# generate m % of missing values
gen_missing_val <- function(data,seed,m){
  set.seed(seed)
  datam <- as.data.frame(lapply(data, function(cc) cc[ sample(c(TRUE, NA), prob = c(1-m, m), size = length(cc), replace = TRUE) ]))
  return(datam)
}