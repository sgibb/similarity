\name{as.binary.matrix}
\alias{as.binary.matrix}
\title{Converts a matrix into binary one.}
\usage{
  as.binary.matrix(x)
}
\arguments{
  \item{x}{matrix}
}
\value{
  matrix
}
\description{
  This function converts a matrix into binary (0/1).
  \code{NA} values become zero and \code{!NA} values one.
}
\examples{
library("similarity")

m <- matrix(c(1, 2, NA, 2), nrow=2, ncol=2)
as.binary.matrix(m)
#     [,1] [,2]
#[1,]    1    0
#[2,]    1    1
}

