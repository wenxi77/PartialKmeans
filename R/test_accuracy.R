#' test accuracy rate of kmeans clustering fitted values
#' 
#' @param actual_y actual labels/clusters each row of data set from
#' @param fit_y fitted value of each row of data set
#'
#' @return  accuracy rate of given sets of data
#' 
#' }
#' 
#'@export
test_accuracy<-function(actual_y,fit_y){
  acc_rate<-sum(fit_y==actual_y,na.rm = TRUE)/length(fit_y)
  return(acc_rate)
}
