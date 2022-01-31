Partial_km0130.R

#' K-means clustering with missing values
#'
#' 
#' @param m input data, with the form of matrix, only numeric values or NAs. No whole NA rows or columns
#' @param k Number of clusters
#' @param initCtrs the initial guesses; row i has the initial guess for cluster i;
#' @param nIters number of iterations user specified
#'
#' @return
#' \describe{
#'   \item{fitted_value}{membership of each row of m}
#'   \item{fitted_Centroid}{the coordinates of the centroids of all clusters}
#'   \item{distance}{distance matrix of each row to each centroids}
#' }


fitted.test.R
#' test cluster centroids of training set, return average error of testing set and fitted cluster members of testing set
#'
#' 
#' @param testset input testing data, with the form of matrix, only numeric values or NAs. No whole NA rows or columns
#' @param k Number of clusters
#' @param initC the centroids coordinates returned by training set; fitted_Centroid returned by partial_km
#'
#' @return
#' \describe{
#'   \item{(ms_e}{average distance from each row to assigned cluster centroids}
#'   \item{fitted_values}{membership assigned to each row of testing set}
#'   
#' }
#'@export

gen_missing_val.R
#' generate a_percent % of missing values
#' 
#'given a complete data set, generate a specific percentage of missing values
#' 
#' @param data input complete data, only numeric values
#' @param seed Set the random seed
#' @param na_percent percentage of missing values user intend to generate 
#'
#' @return dataframe with na_percent% missing values
#' 
#'   
#'@export

get_initC.R
#' initialize centroid base on input data
#' 
#'given a complete data set, generate a specific percentage of missing values
#' 
#' @param xdata input data matrix, only numeric values or NAs
#' @param n_cluster number of cluster user specified
#'
#' @return centroid coordinates of all clusters
#' 
#'   
#'@export

gen_train_test.R
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

plot_distance.R
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


test_accuracy.R
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

optimal_num_cluster.R
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
