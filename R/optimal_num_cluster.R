#' find the optimal number of clusters
#' 
#' @param num_k number of clusters 1:num_k user specify to test on
#' @param train training set of input data, only numeric values or NAs
#' @param test testing set of input data, only numeric values or NAs
#' @return  
#' \describe{
#'   \item{error_df}{dataframe of number of clusters vs average distance from testing data to assigned centroid coordinates}
#'   \item{p}{line plot of error_df}
#'   
#' }
#' 
#' }
#' 
#'@export
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
