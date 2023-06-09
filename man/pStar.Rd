% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pStar.R
\name{pStar}
\alias{pStar}
\title{Report "stars" for a vector of p-values}
\usage{
pStar(p)
}
\arguments{
\item{p}{A vector of p-values}
}
\value{
A vector of significances according to the star system
}
\description{
Report "stars" for a vector of p-values
}
\examples{
set.seed(11)
p_values <- runif(20)
data.frame(p_values, p_star = pStar(p_values))
}
