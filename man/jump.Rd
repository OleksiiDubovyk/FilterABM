% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/disperse.R
\name{jump}
\alias{jump}
\title{Helper function, randomly jump values in a vector}
\usage{
jump(x)
}
\arguments{
\item{x}{Integer vector}
}
\value{
Vector of the same length as input vector
}
\description{
For every value in a vector, randomly add or subtract one.
The min value in the vector can only be added one, and the max value can only be subtracted from.
}
