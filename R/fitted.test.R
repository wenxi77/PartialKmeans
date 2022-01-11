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
  # for (j in 1:K)#each centroid
  #         {
  #         ss_d[,j] = apply(testset,1,function(x){sum(x[which(!is.na(x))]-initC[j,][which(!is.na(x))])^2})
  #         }
  
  return(list(ms_e = sum(ss_e)/(nrow_t*ncol_t), fitted_values = members))
}
