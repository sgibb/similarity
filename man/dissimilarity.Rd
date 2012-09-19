\name{dissimilarity}
\alias{dissimilarity}
\title{Similarity measurement for binary matrices.}
\usage{
  dissimilarity(x, ...)
}
\arguments{
  \item{x}{a \code{\link{matrix}}. Please take care that
  rows contain the samples and columns the features.}

  \item{...}{further arguments passed to
  \code{\link{similarity}}.}
}
\description{
  This function calculates different dissimilarity indices
  for a binary matrix. It is only a wrapper around
  \code{\link{similarity}} and
  \code{\link{as.dist.similarity}}.
}
\examples{
library("similarity")

a <- matrix(c(1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 0, 1),
            ncol=3, nrow=4, byrow=TRUE)
dissimilarity(a, "soerensen")
}
\seealso{
  \code{\link{similarity}},
  \code{\link{as.dist.similarity}}
}

