
#' train test split
#' 
#'given a whole data set, split into training and testing data
#' 
#' @param data input data matrix, only numeric values or NAs
#' @param seed Set the random seed
#'
#' @return  
#' \describe{
#'   \item{Train}{training set}
#'   \item{test}{testing set}
#'   
#' }
#' 
#'   
#'@export
gen_train_test <- function(data,seed) {
  set.seed(seed)
  trainIndex <- createDataPartition(1:dim(data)[1], p = .7,
                                    list = FALSE,
                                    times = 1)
  Train <- data[ trainIndex,]
  Valid <- data[-trainIndex,]
  return(list(train=Train,test=Valid))
}