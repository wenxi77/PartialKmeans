optimal_k <- function(num_k,train,test){
  mse_df <- tibble(k=1:num_k)
  for (k in 1:num_k) {
    initc <- gen_initC(train_BC[,-10],k)
    partial_model <- Partial_km(train[,-10],k,initc,100)
    partial_test <- fitted.test(test[,-10],k,partial_model$fitted_Centroid)
    mse_df$error[k] <- partial_test$ms_e
  }
  p <- ggplot(data = mse_df, aes(x = k, y = error)) +
    geom_point()+geom_line()
  return(list(error_df=mse_df, plot=p))
}
