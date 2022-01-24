# train test split
#return $train, $test
gen_train_test <- function(data,seed) {
  set.seed(seed)
  trainIndex <- createDataPartition(1:dim(data)[1], p = .7,
                                    list = FALSE,
                                    times = 1)
  Train <- data[ trainIndex,]
  Valid <- data[-trainIndex,]
  return(list(train=Train,test=Valid))
}