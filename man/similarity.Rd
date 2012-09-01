\name{similarity}
\alias{similarity}
\title{Similarity measurement for binary matrices.}
\usage{
  similarity(x,
    method = c("soerensen-dice", "soerensen", "dice", "jaccard", "simple", "simplematching", "rogers", "tanimoto", "rogers-tanimoto"))
}
\arguments{
  \item{x}{a binary \code{\link{matrix}}. Please take care
  that rows contain the samples and columns the features.}

  \item{method}{the similarity index to be used.}
}
\value{
  An object of class \emph{similarity}.

  The lower triangle of the similarity matrix stored by
  columns in a vector.

  \itemize{ \item{\code{Size:} \code{integer}, number of
  observations.} \item{\code{Labels:} \code{character},
  contains names of the observations.}
  \item{\code{diagonalValues:} \code{double}, diagonal
  values.} \item{\code{method:} \code{character}, the
  method used.} \item{\code{call:} \code{language}, the
  \code{call} used to create the object.} }
}
\description{
  This function calculates different similarity indices for
  a binary matrix.
}
\details{
  \code{p} = number of binary variables, \eqn{n_{00} +
  n_{01} + n_{10} + n_{11}} \cr \code{i} and \code{j} = two
  observations \cr \code{n_{00}} = \eqn{\sum\limits_{k=1}^p
  I(x_{ik} = 0, x_{jk} = 0} \cr \code{n_{01}} =
  \eqn{\sum\limits_{k=1}^p I(x_{ik} = 0, x_{jk} = 1} \cr
  \code{n_{10}} = \eqn{\sum\limits_{k=1}^p I(x_{ik} = 1,
  x_{jk} = 0} \cr \code{n_{11}} = \eqn{\sum\limits_{k=1}^p
  I(x_{ik} = 1, x_{jk} = 1} \cr

  Asymmetric indices: \tabular{lr}{ Soerensen/Dice
  (\code{soerensen-dice}) \tab \eqn{\frac{2n_{11}}{n_{01} +
  n_{10} + n_{11}}} \cr

  Jaccard (\code{jaccard}) \tab \eqn{\frac{n_{11}}{n_{01} +
  n_{10} + n_{11}}} \cr }

  Symmetric indices: \tabular{lr}{ Simple Matching
  (\code{simple}) \tab \eqn{\frac{n_{00} + n_{11}}{p}} \cr

  Rogers and Tanimoto (\code{rogers-tanimoto}) \tab
  \eqn{\frac{n_{00} + n_{11}}{n_{00} + 2(n_{01} + n_{10}) +
  n_{11}}} \cr }
}
\examples{
library("similarity")

a <- matrix(c(1, 1, 1, 0, 0, 0, 1, 1, 0, 1, 0, 1),
            ncol=3, nrow=4, byrow=TRUE)
similarity(a, "soerensen")
}
\references{
  Dice, L. R. (1945), \dQuote{Measures of the amount of
  ecological association between species.}, \emph{Ecology}
  \bold{26}: 297-302. \cr

  Jaccard, P. (1901), \dQuote{\'Etude comparative de la
  distribution florale dans une portion des Alpes et des
  Jura}, \emph{Bulletin de la Soci\'et\'e Vaudoise des
  Sciences Naturelles} \bold{37}: 547-579. \cr

  Rogers, D. J. and Tanimoto, T. T. (1960), \dQuote{A
  computer program for classifying plants.}, \emph{Science}
  \bold{132}: 1115-1118. \cr

  Sokal, R. R. and Michener, C. D. (1956), \dQuote{A
  statistical method for evaluating systematic
  relationships.}, \emph{University of Kansas Science
  Bulletin} \bold{38}: 1409-1438. \cr

  Soerensen, T. (1948), \dQuote{A method of establishing
  groups of equal amplitude in plant sociology based on
  similarity of species content.}, \emph{Biologiske
  Skrifter} \bold{4}: 1-34. \cr
}

