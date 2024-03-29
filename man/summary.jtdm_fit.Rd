% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary.jtdm_fit.R
\name{summary.jtdm_fit}
\alias{summary.jtdm_fit}
\title{Prints the summary of a fitted jtdm}
\usage{
\method{summary}{jtdm_fit}(object, ...)
}
\arguments{
\item{object}{a model fitted with \code{jtdm_fit}}

\item{...}{additional arguments}
}
\value{
A printed summary of the fitted jtdm
}
\description{
Prints the summary of a fitted jtdm
}
\examples{
data(Y)  
data(X)  
m = jtdm_fit(Y=Y, X=X, 
             formula=as.formula("~GDD+FDD+forest"), sample = 1000)  
summary(m)
}
\author{
Giovanni Poggiato
}
