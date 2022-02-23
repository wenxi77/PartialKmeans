#' K-means clustering with missing values
#'
#' 
#' @param m input data, with the form of matrix, only numeric values or NAs. No whole NA rows or columns
#' @param k Number of clusters
#' @param initCtrs the initial guesses; row i has the initial guess for cluster i; randomly set the centroids if user does not specify initial value
#' @param nIters number of iterations user specified, default 100
#'
#' @return
#' \describe{
#'   \item{fitted_value}{membership of each row of m}
#'   \item{fitted_Centroid}{the coordinates of the centroids of all clusters}
#'   \item{distance}{distance matrix of each row to each centroids}
#' }
#' @export
Partial_km <- function(m,k,initCtrs,nIters=100){
  #init variables
  iter <- 0
  nrm <- nrow(m)
  ncm <- ncol(m)
  rowsM <- 1:nrm
  colsM <- 1:ncm
  
  # randomly set the centroids if user does not specify initial value
  if (missing(initCtrs)) {
    initCtrs = gen_initC(m,k)
  }
  
  initC <- matrix(initCtrs,byrow = TRUE,nrow=k)
  temp_Ctrs <- matrix(0, nrow = k, ncol = ncm)
  d <- matrix(0, nrow = nrm, ncol = k)
  # record positions of non NAs
  intactPlaces <- apply(m,1,function(x){setdiff(colsM,which(is.na(x)))})

    while(iter <= as.integer(nIters)){
      iter <- iter + 1
      #row i of dists will be the distances to ctrs
      dists <- getDistances(m,initC,intactPlaces,d,rowsM,k)
      ##assign cluster number to each observation
      members <- apply(dists,1,which.min)
      #update the current centroid
      temp_Ctrs <- updateCtrs(m,temp_Ctrs,members,k,initC)
      if(all(temp_Ctrs == initC)){
        break
      }
      else{
        initC <- temp_Ctrs
      }
      
    }
  return(list(fitted_value=members,fitted_Centroid=temp_Ctrs,distance=dists))
}

getDistances <- function(m,ctrs,intactPlaces,d,rowsM,k){
  for (j in 1:k)
  {
    for (i in rowsM){
      d[i,j] = sum((m[i,] - ctrs[j,])[intactPlaces[[i]]]^2)
    }
  }
  return(d)
}
updateCtrs <- function(m,ctrs,members,k,initC){
  ##calculate the new centroid
  for (i in 1:k){
    m_i<-matrix(unlist(m[which(members==i),]),ncol = ncol(m))
    for(j in 1:ncol(m_i)){
      m_ij <- m_i[,j]
      intactCol <- !is.na(m_ij)# find intact column index of m_i
      if (all(intactCol==FALSE)){
        ctrs[i,j] <- initC[i,j]
        warning("whole column of current group are NAs, replace current centroid entry with initial centroid\n")
        #warn_if_first(reason ="all missing values",message="whole column of current group are NAs, replace current centroid entry with initial centroid\n")
      }
      else{
        ctrs[i,j] <- sum(m_ij[which(intactCol)])/sum(intactCol)
      }
    }
  }
  return(ctrs)
}


