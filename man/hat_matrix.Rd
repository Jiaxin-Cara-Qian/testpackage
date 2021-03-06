% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hat_matrix.R
\name{hat_matrix}
\alias{hat_matrix}
\title{hat_matrix}
\usage{
hat_matrix(fit.model)
}
\arguments{
\item{fit.model}{takes an R object, returned by \link[stats]{lm}}
}
\value{
A list containg the following components:
\itemize{
  \item{maximum hi} - {which subject has the maximum value of the leverage}
  \item{hi>2*mean(H)} - {what subjects are outliers in the X space, and their leverage values}
}
}
\description{
Finds the index of the largest leverage value
}
\examples{
##Use demo_data (See ?demo_data for more information about the dataset)
m = lm(fev~A+H+M+S, data = demo_data)
hat_matrix(m)
hat_matrix(m)[1]
hat_matrix(m)[2]


}
