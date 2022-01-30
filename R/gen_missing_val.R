#' generate a_percent % of missing values
#' 
#'given a complete data set, generate a specific percentage of missing values
#' 
#' @param data input complete data, only numeric values
#' @param seed Set the random seed
#' @param na_percent percentage of missing values user intend to generate 
#'
#' @return dataframe with na_percent% missing values
#' 
#'   
#'@export
gen_missing_val <- function(data,seed,na_percent){
  set.seed(seed)
  datam <- as.data.frame(lapply(data, function(cc) cc[ sample(c(TRUE, NA), prob = c(1-na_percent, na_percent), size = length(cc), replace = TRUE) ]))
  return(datam)
}