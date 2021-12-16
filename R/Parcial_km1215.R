
# arguments:
#   m: input data, with the form of matrix. No whole NA rows or columns
#   k:  the number of clusters
#   initCtrs: the initial guesses; row i has the initial guess for cluster i;
#   nIters: number of iterations user specified

# value:
#   membership of each row of m



Parcial_km1215 <- function(m,k,initCtrs,nIters){
  #init variables
  iter <- 0
  nrm <- nrow(m)
  ncm <- ncol(m)
  rowsM <- 1:nrm
  colsM <- 1:ncm
  # set the centroids to the user-specified initial value
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
    temp_Ctrs <- updateCtrs(m,temp_Ctrs,members,k)
    if(all(temp_Ctrs == initC)){
      break
    }
    else{
      initC <- temp_Ctrs
    }
    
  }
  return(members)
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
updateCtrs <- function(m,ctrs,members,k){
  ##calculate the new centroid
  for (i in 1:k){
    m_i<-m[which(members==i),]
    for(j in 1:ncol(m_i)){
      m_ij <- m_i[,j]
      intactCol <- !is.na(m_ij)# find intact column index of m_i
      ctrs[i,j] <- sum(m_ij[which(intactCol),])/sum(intactCol)
    }
  }
  return(ctrs)
}
#   updateCtrs2 <- function(m,ctrs,members){
#   for (i in 1:k){
#         m_i<- m[which(members==i),]
# 
#         ctrs[i,] <- apply(m_i,2,function(x){sum(x[which(!is.na(x))])/sum(!is.na(x))})
#   }
#   return(ctrs)
# }
