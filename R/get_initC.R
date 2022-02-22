
#' initialize centroid base on input data
#' 
#'given a complete data set, generate a specific percentage of missing values
#' 
#' @param xdata input data matrix, only numeric values or NAs
#' @param n_cluster number of cluster user specified
#'
#' @return centroid coordinates of all clusters
#' 
#'   
#'@export
gen_initC <- function(xdata,n_cluster){
  initC <- apply(xdata,2, function(x){x[sample(which(complete.cases(x)),n_cluster)]})
  initc <- as.numeric(unlist(initC))
  return(initc)
}
