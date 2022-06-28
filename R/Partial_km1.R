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
}

# for each possible intactness pattern, finds the rows in inData with
# that pattern; output z is an R list; e.g. z[['1,3,18'] = c(12,200)
# would mean that in rows 12 and 200, elements 1, 3 and 18 are intact
# and the rest are NAs

splitByNALocs <- function(inData) 
{
   nonNALocs <- function(rowNum) 
       paste(which(!is.na(inData[rowNum,])),collapse=',')
   
   tmp <- sapply(1:nrow(inData),nonNALocs) 
   tmp <- data.frame((1:nrow(inData)),tmp)
   split(tmp[,1],tmp[,2])
}

getNumPatterns <- function(intactLocs) 
{
   lapply(names(intactLocs),
      function(patt) as.numeric(strsplit(patt,',')[[1]]))
}

# intactNonNALocs is the output of splitByNALocs(); p is number of cols
# in original data
#
# the call getBitVectors(intactNonNALocs,ncol(inData))
# returns a matrix m; i-th row is a bit vector, element j being 1 or 0,
# according to whether j appears in names(intactNonNALocs)[i]; row names
# in the matrix will be the list element names in intactNonNALocs, i.e.
# the different intactness patterns
#
# e.g. say p = 4 and m[3,] = c(0,1,1,0); then 2 and 3 appear in the 3rd
# intactness pattern, while 1 and 4 do not

getBitVectors <- function(intactNonNALocs,p)
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

# inData is our original dataset; # intactLocs is the output of
# intactLocs; bitVecs is the output of getBitVectors(); ctrds is the
# matrix of centroids, one centroid per row

findClusterMembers <- function(inData,intactLocs,bitVecs,ctrds) 
{
   require(pdist)

   doOnePattern <- function(patt)  # e.g. patt = '3,8,9,21'
   {
      rows <- intactLocs[[patt]]
      cols <- which(bitVecs[patt,] == 1)
      dists <- pdist(inData[rows,cols,drop=F],ctrds[,cols,drop=F])
      dists <- as.matrix(dists)
      apply(dists,1,which.min)
   }

   tmp <- sapply(names(intactLocs),doOnePattern)
   names(tmp) <- names(intactLocs)
   tmp
}

# clusterMembers is the output from findClusterMembers(); ctrds is the
# centroids matrix

updateCtrds <- function(clusterMembers,ctrds) 
{
   ctrds[,] <- 0

   for (i in 1:length(clusterMembers)) {
      patt <- names(clusterMembers)[i]
      numPattern <- as.numeric(strsplit(patt,',')[[1]])
      rows <- intactLocs[[patt]]
      for (j in 1:length(rows)) {
         rw <- rows[j]
         contribToSum <- rw[numPattern]
         destCluster <- clusterMembers[[i]][j]
         ctrd[destCluster,numPattern] <-
            ctrd[destCluster,numPattern] + contribToSum
      }
   }

   ctrds
}

# finally, for each pattern in names(intactNonNALocs), need to find new
# class memberships, using pdist(), then find the new centroid totals
# and counts for each i in 1:p, and update the centroids 

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


