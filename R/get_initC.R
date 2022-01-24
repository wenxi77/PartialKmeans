## initialize centroid
gen_initC <- function(xdata,n_cluster){
  initC <- apply(xdata,2, function(x){x[sample(complete.cases(x) %>% which(),n_cluster)]})
  initc <- as.data.frame(initC) %>% unlist() %>%as.numeric()
  return(initc)
}