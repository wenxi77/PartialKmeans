Parcial_km<-function(m,k,initCentroids,nIters){
  iter <- 0
  initC<-matrix(initCentroids,byrow = TRUE,nrow=k)
  #creating temporary centroid
  temp_Centroid <- matrix(0, nrow = k, ncol = ncol(m))
  #creating distance matrix
  d <- matrix(0, nrow = nrow(m), ncol = k)
  
  while(iter<=as.integer(nIters)){
    iter <- iter + 1
    for (j in 1:k)
    {
      
      d[,j] = apply(m,1,function(x){sum((x-initC[j,])[which(!is.na(x))]^2)})
    }
    ##assign cluster number to each observation
    v<-apply(d,1,which.min)
    ##calculate the new centroid
    for (i in 1:k){
      m_i<-m[which(v==i),]
      temp_Centroid[i,] <- apply(m_i,2,function(x){sum(x[which(!is.na(x))])/sum(!is.na(x))})
    }
    #update the current centroid
    if(all(temp_Centroid == initC)){
      break
    }
    else{
      initC <- temp_Centroid
    }
    
  }
  return(list(fitted_value=v,fitted_Centroid=temp_Centroid))
}
test_accuracy<-function(actual_y,fit_y){
  acc_rate<-sum(fit_y==actual_y,na.rm = TRUE)/length(fit_y)
  return(acc_rate)
}
