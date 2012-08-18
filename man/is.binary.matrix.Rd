\name{is.binary.matrix}
\alias{is.binary.matrix}
\title{Is a matrix binary one?}
\usage{
  is.binary.matrix(x)
}
\arguments{
  \item{x}{matrix}
}
\value{
  TRUE/FALSE
}
\description{
  Tests whether given matrix is a binary one.
}
\examples{
library("similarity")

m <- matrix(c(1, 2, NA, 2), nrow=2, ncol=2)
b <- matrix(c(1, 1, 0, 1), nrow=2, ncol=2)

is.binary.matrix(m)    # FALSE
is.binary.matrix(1:10) # FALSE
is.binary.matrix(b)    # TRUE
}

