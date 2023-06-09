% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalise.R
\name{normalise}
\alias{normalise}
\title{Normalisation (ripped from some other package)}
\usage{
normalise(X, method, refIdx, noiseInt = c(11, 12))
}
\arguments{
\item{X}{Data to normalise (samples in rows, variables in columns)}

\item{method}{Constant sum (method<-'CS'); Constant noise (method<-'CN'); Qoutient probabilistic method (method<-'PQN'); Linear baseline normalisation (method<-'LBN'); Auto-scaling (method<-'AS'); Pareto scaling (method<-'PS')}

\item{refIdx}{Parameter for PQN (hard setting reference sample; defaults to median)}

\item{noiseInt}{Parameter for method 'CN'}
}
\value{
Normalised data
}
\description{
Normalisation (ripped from some other package)
}
