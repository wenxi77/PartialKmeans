
#' density, histogram plot
#' 
#'plot distance frequency of observations with different number of NAs
#' 
#' @param model instance of Partial_km function on training set
#' @param x_data input data matrix, only numeric values or NAs
#' @param plot_type user specify "histogram" or "density"
#'
#' @return  
#' \describe{
#'   \item{plot_df}{dataframe of number of NAs each row vs distance of each row to assigned cluster centroids}
#'   \item{plot}{"histogram" or "density" plot}
#'   
#' }
#' 
#'@export
plot_distance <- function(model,x_data,plot_type){
  library(ggplot2)
  library(hrbrthemes)
  library(viridis)
  dis_per_row <- apply(model$distance,1,function(x){x[which.min(x)]})#get the smallest dis for each row
  na_per_row <- apply(x_data,1,function(x){sum((is.na(x)))})
  na_dis_per_row <- tibble(num_na=factor(na_per_row),dis_per_row)
  if (plot_type=="histogram"){
    p <- ggplot(na_dis_per_row, aes(x=dis_per_row, fill=num_na)) +
      geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
      theme_ipsum() +
      labs(fill="")
  }
  if (plot_type=="density"){
    p <-  ggplot(na_dis_per_row,aes(x=dis_per_row, color=num_na,fill=num_na)) +
      geom_density(adjust=1.5, alpha=.4) +
      theme_ipsum()
  }
  return(list(plot_df=na_dis_per_row,plot=p))
}
