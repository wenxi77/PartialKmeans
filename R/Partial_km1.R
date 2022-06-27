#' K-means clustering with missing values
#'
#' 
#' @param m Input data, with the form of matrix, only numeric values or NAs. No whole NA rows or columns
#' @param k Number of clusters
#' @param initCtrs The initial centroid guesses; row i has the initial guess for cluster i; randomly set the centroids if user does not specify initial value
#' @param nIters Number of iterations user specified, default 100
#'
#' @return
#' \describe{
#'   \item{fitted_value}{membership of each row of m}
#'   \item{fitted_Centroid}{the coordinates of the centroids of all clusters}
#'   \item{distance}{distance matrix of each row to each centroids}
#' }
#' @export
Partial_km1 <- function(m,k,initCtrs,nIters=100){
  #init variables
  iter <- 0
  nrm <- nrow(m)
  ncm <- ncol(m)
  rowsM <- 1:nrm
  colsM <- 1:ncm
  
  # randomly set the centroids if user does not specify initial value
  if (missing(initCtrs)) {
     # body of this 'if' changed by NM, 6/23/22
    mcc <- na.exclude(m)
    randomRowNumbers <- sample(1:nrow(mcc),k)
    initCtrs <- mcc[randomRowNumbers,]
  }

# NM, 6/25/22 outline

#    1.  create R list intactNonNALocs, one element for each observed
#    intactness pattern; e.g. if intactness[['3,8,22']] = c(15,99), then
#    rows 15 and 99 in the original data have elements 3, 8 and 22
#    intact and all else NAs

splitByNALocs <- function(inData) 
{
   nonNALocs <- function(rowNum) 
       paste(which(!is.na(inData[rowNum,])),collapse=',')
   
   tmp <- sapply(1:nrow(inData),nonNALocs) 
   tmp <- data.frame((1:nrow(inData)),tmp)
   split(tmp[,1],tmp[,2])
}

#    2.  from intactNonNALocs, create a list patternCounts, where e.g.
#    patternCounts[['3,8,22']] would have subelements pattern =
#    c(3,8,22) and count = length(intactNonNALocs[['3,8,22']])

###    counts <- sapply(intactNonNALocs,length)
###    intactDF <- data.frame(counts=counts)
###    row.names(intactDF) <- names(intactNonNALocs)

# e.g. the call getBitVectors(intactNonNALocs,ncol(inData))
# returns a matrix; i-th row is a bit vector, element j being 1 or 0,
# according to whether j appears in names(intactNonNALocs)[i]
getBitVectors <- function(patternNames,p)
{

   getBitVector <- function(intactPattern,p) 
   {
      tmp <- rep(0,p)
      numPattern <- as.numeric(strsplit(intactPattern,',')[[1]])
      tmp[numPattern] <- 1
      tmp
   }

   tmp <- t(sapply(names(intactNonNALocs),getBitVector,p))
   row.names(tmp) <- names(intactNonNALocs)
   tmp
}

# returns list, indexed by i in 1:p, with element i; input is output of
# getBitVectors()
whichContainI <- function(bitVectors) 
{
   p <- ncol(bitVectors)

   getHaveI <- function(i) 
   {
      tmp <- which(bitVectors[,i] == 1)
      row.names(bitVectors)[tmp]
   }

   lapply(1:p,getHaveI)
}

# finally, for each pattern in names(intactNonNALocs), need to new class
# memberships, using pdist(), then find the new centroid totals and
# counts for each i in 1:p, and update the centroids 

## getCentroids <- function(ctrTots) 
## {
## 
## }


##   # NM removed, 6/23/22
##   # initC <- matrix(initCtrs,byrow = TRUE,nrow=k)

##   # NM started replacing, 6/25/22
##   temp_Ctrs <- matrix(0, nrow = k, ncol = ncm)
##   d <- matrix(0, nrow = nrm, ncol = k)
##   # record positions of non NAs
##   intactPlaces <- apply(m,1,function(x){setdiff(colsM,which(is.na(x)))})
## 
##   while(iter <= as.integer(nIters)){
##     iter <- iter + 1
##     #row i of dists will be the distances to ctrs
##     dists <- getDistances(m,initC,intactPlaces,d,rowsM,k)
##     ##assign cluster number to each observation
##     members <- unlist(apply(dists,1,which.min))
##     #update the current centroid
##     temp_Ctrs <- updateCtrs(m,temp_Ctrs,members,k,initC)
##     if(all(temp_Ctrs == initC)){
##       break
##     }
##     else{
##       initC <- temp_Ctrs
##     }
##     
##   }
## 
##   return(list(fitted_values=members,fitted_Centroid=temp_Ctrs,distance=dists))
## }
## 
## getDistances <- function(m,ctrs,intactPlaces,d,rowsM,k){
##   for (j in 1:k)
##   {
##     for (i in rowsM){
##       intact<-intactPlaces[[i]]
##       d[i,j] = sum((m[i,] - ctrs[j,])[intact]^2)/length(intact)
##     }
##   }
##   return(d)
## }
## updateCtrs <- function(m,ctrs,members,k,initC){
##   ##calculate the new centroid
##   for (i in 1:k){
##     m_i<-matrix(unlist(m[which(members==i),]),ncol = ncol(m))
##     for(j in 1:ncol(m_i)){
##       m_ij <- m_i[,j]
##       intactCol <- !is.na(m_ij)# find intact column index of m_i
##       if (all(intactCol==FALSE)){
##         ctrs[i,j] <- initC[i,j]
##         warning("whole column of current group are NAs, replace current centroid entry with initial centroid\n")
##         #warn_if_first(reason ="all missing values",message="whole column of current group are NAs, replace current centroid entry with initial centroid\n")
##       }
##       else{
##         ctrs[i,j] <- sum(m_ij[which(intactCol)])/sum(intactCol)
##       }
##     }
##   }
##   return(ctrs)
## }


