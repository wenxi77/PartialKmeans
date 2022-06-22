\name{Partial_km}
\alias{Partial_km}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
K-means clustering handle missing values
}
\description{
When calculating the squared distances of each data point to the centroid, we only consider the partial distance with entries of non-NA values.
}
\usage{
Partial_km(m, k, initCtrs, nIters = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m}{
input data, with the form of matrix, only numeric values or NAs. No whole NA rows or columns
}
  \item{k}{
Number of clusters
}
  \item{initCtrs}{
the initial guesses; row i has the initial guess for cluster i; randomly set the centroids if user does not specify initial value}
  \item{nIters}{
number of iterations user specified, default 100
}
}
\value{
\describe{
\item{fitted_value}{membership of each row of m}
\item{fitted_Centroid}{the fitted coordinates of the centroids of all clusters}
\item{distance}{squared distance matrix of each row to each centroids}
}
}

\author{
Wenxi Zhang; Norman Matloff
}



\section{Warning}{if whole column of current group are all NAs, current centroid entry will be replaced with initial centroid
}


\examples{
#get data samle
data(HouseVotes84)
head(HouseVotes84)
#        Class   V1 V2 V3   V4   V5 V6 V7 V8 V9 V10  V11  V12
# 1 republican    n  y  n    y    y  y  n  n  n   y <NA>    y
# 2 republican    n  y  n    y    y  y  n  n  n   n    n    y
# 3   democrat <NA>  y  y <NA>    y  y  n  n  n   n    y    n
# 4   democrat    n  y  y    n <NA>  y  n  n  n   n    y    n
# 5   democrat    y  y  y    n    y  y  n  n  n   n    y <NA>
# 6   democrat    n  y  y    n    y  y  n  n  n   n    n    n
#   V13 V14 V15  V16
# 1   y   y   n    y
# 2   y   y   n <NA>
# 3   y   y   n    n
# 4   y   n   n    y
# 5   y   y   y    y
# 6   y   y   y    y
#convert characters to numerical numbers
encode<-function(x){
  factor(x,levels = c('n', 'y'),labels = c(1, 2))%>%as.numeric()
}
#x variables
house_votes<-apply(HouseVotes84[2:17],2,encode)
#y var
votes_y<-as.numeric(factor(HouseVotes84[,1]))
house.votes <- cbind(votes_y,house_votes)
house_model <- Partial_km(house.votes[,2:17],k=2,initc,100)
house_model$fitted_value
house_model$fitted_Centroid
house_model$distance

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
