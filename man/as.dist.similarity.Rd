\name{as.dist.similarity}
\alias{as.dist.similarity}
\title{Converts a similarity matrix into a distance matrix.}
\usage{
  \method{as.dist}{similarity} (m, diag = FALSE,
    upper = FALSE)
}
\arguments{
  \item{m}{matrix}

  \item{diag}{logical, should
  \code{\link[stats]{print.dist}} prints the diagonal?}

  \item{upper}{logical, should
  \code{\link[stats]{print.dist}} prints the upper
  triangel?}
}
\value{
  an object of class \code{\link[stats]{dist}}.
}
\description{
  This function converts a \code{\link{similarity}} matrix
  into a \code{\link[stats]{dist}} matrix.
}
\details{
  To transform the similarity matrix into a distance matrix
  the following equation is used: \eqn{d = sqrt{s[i,i] +
  s[j,j] - 2s[i,j]}}
}
\examples{
library("similarity")

a <- matrix(c(1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 0, 1),
            ncol=3, nrow=4, byrow=TRUE)
s <- similarity(a, "jaccard")
as.dist(s)
}
\seealso{
  \code{\link[stats]{dist}},
  \code{\link[stats]{print.dist}}
}

