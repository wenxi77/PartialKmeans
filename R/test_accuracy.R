test_accuracy<-function(actual_y,fit_y){
  acc_rate<-sum(fit_y==actual_y,na.rm = TRUE)/length(fit_y)
  return(acc_rate)
}
