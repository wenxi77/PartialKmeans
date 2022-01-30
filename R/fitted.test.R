#' test cluster centroids of training set, return average error of testing set and fitted cluster members of testing set
#'
#' 
#' @param testset input testing data, with the form of matrix, only numeric values or NAs. No whole NA rows or columns
#' @param k Number of clusters
#' @param initC the centroids coordinates returned by training set; fitted_Centroid returned by partial_km
#'
#' @return
#' \describe{
#'   \item{(ms_e}{average distance from each row to assigned cluster centroids}
#'   \item{fitted_values}{membership assigned to each row of testing set}
#'   
#' }
#'@export
fitted.test<-function(testset,k,initC){
  nrow_t <- nrow(testset)
  ncol_t <- ncol(testset)
  ss_e <- matrix(0, nrow = nrow_t, ncol = k)
  # record positions of non NAs
  intactPlaces <- apply(testset,1,function(x){setdiff(1:ncol_t,which(is.na(x)))})
  for (j in 1:k)
  {
    for (i in 1:nrow_t){
      ss_e[i,j] = sum((testset[i,] - initC[j,])[intactPlaces[[i]]]^2)
    }
  }
  members <- apply(ss_e,1,which.min)

  return(list(ms_e = sum(ss_e)/(nrow_t*k), fitted_values = members))
}
