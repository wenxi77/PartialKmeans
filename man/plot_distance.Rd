\name{plot_distance}
\alias{plot_distance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
density, histogram plot
}
\description{
density, histogram plot of the squared distance between the centroid and each row, categorized by the number of NAs in each row
}
\usage{
plot_distance(model, x_data, plot_type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
instance of Partial_km function on training set
}
  \item{x_data}{
input data matrix, only numeric values or NAs
}
  \item{plot_type}{
user specify "histogram" or "density"
}
}

\value{

\describe{
\item{plot_df}{dataframe of number of NAs each row vs distance of each row to assigned cluster centroids}
\item{plot}{"histogram" or "density" plot}
}
}
\description{
Visualize the influence of the number of missing values for each observation by drawing density plots of the distance between the centroid and each observation. All distances are categorized by the number of NAs in each observation.
}
\author{
Wenxi Zhang; Norman Matloff
}

\examples{
data(HouseVotes84)
#convert characters to numerical numbers
encode<-function(x){
  factor(x,levels = c('n', 'y'),labels = c(1, 2))%>%as.numeric()
}
#x variables
house_votes<-apply(HouseVotes84[2:17],2,encode)
#y var
votes_y<-as.numeric(factor(HouseVotes84[,1]))
house.votes <- cbind(votes_y,house_votes)
house_model <- Partial_km(house.votes[,2:17],k=2)
plot_distance(house_model,house.votes[,-1],plot_type="density")$plot
plot_distance(house_model,house.votes[,-1],plot_type="histogram")$plot
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
