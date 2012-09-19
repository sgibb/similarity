## Copyright 2012 Sebastian Gibb
## <mail@sebastiangibb.de>
##
## This is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## It is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## See <http://www.gnu.org/licenses/>

#' Similarity measurement for binary matrices.
#'
#' This function calculates different similarity indices for a binary matrix.
#'
#' @param x a binary \code{\link{matrix}}. Please take care that rows contain
#'  the samples and columns the features.
#' @param method the similarity index to be used.
#'
#' @details
#'
#' \code{p} = number of binary variables, \eqn{n_{00} + n_{01} + n_{10} + n_{11}} \cr
#' \code{i} and \code{j} = two observations \cr
#' \code{n_{00}} = \eqn{\sum\limits_{k=1}^p I(x_{ik} = 0, x_{jk} = 0} \cr
#' \code{n_{01}} = \eqn{\sum\limits_{k=1}^p I(x_{ik} = 0, x_{jk} = 1} \cr
#' \code{n_{10}} = \eqn{\sum\limits_{k=1}^p I(x_{ik} = 1, x_{jk} = 0} \cr
#' \code{n_{11}} = \eqn{\sum\limits_{k=1}^p I(x_{ik} = 1, x_{jk} = 1} \cr
#'
#' Asymmetric indices:
#' \tabular{lr}{
#'  Soerensen/Dice (\code{soerensen-dice}) \tab 
#'  \eqn{\frac{2n_{11}}{n_{01} + n_{10} + n_{11}}} \cr
#'
#'  Jaccard (\code{jaccard}) \tab 
#'  \eqn{\frac{n_{11}}{n_{01} + n_{10} + n_{11}}} \cr
#' }
#'
#' Symmetric indices:
#' \tabular{lr}{
#'  Simple Matching (\code{simple}) \tab 
#'  \eqn{\frac{n_{00} + n_{11}}{p}} \cr
#'
#'  Rogers and Tanimoto (\code{rogers-tanimoto}) \tab 
#'  \eqn{\frac{n_{00} + n_{11}}{n_{00} + 2(n_{01} + n_{10}) + n_{11}}} \cr
#' }
#'  
#' @return An object of class \emph{similarity}.
#'
#'  The lower triangle of the similarity matrix stored by columns in a
#'  vector.
#'
#'  \itemize{
#'    \item{\code{Size:} \code{integer}, number of observations.}
#'    \item{\code{Labels:} \code{character}, contains names of the observations.}
#'    \item{\code{diagonalValues:} \code{double}, diagonal values.}
#'    \item{\code{method:} \code{character}, the method used.}
#'    \item{\code{call:} \code{language}, the \code{call} used to create the object.}
#'  }
#'
#' @references
#'  Dice, L. R. (1945),
#'  \dQuote{Measures of the amount of ecological association between species.},
#'  \emph{Ecology} \bold{26}: 297-302. \cr
#'
#'  Jaccard, P. (1901),
#'  \dQuote{\'Etude comparative de la distribution florale dans une portion des Alpes et des Jura},
#'  \emph{Bulletin de la Soci\'et\'e Vaudoise des Sciences Naturelles} \bold{37}: 547-579. \cr
#'
#'  Rogers, D. J. and Tanimoto, T. T. (1960),
#'  \dQuote{A computer program for classifying plants.},
#'  \emph{Science} \bold{132}: 1115-1118. \cr
#'
#'  Sokal, R. R. and Michener, C. D. (1956),
#'  \dQuote{A statistical method for evaluating systematic relationships.},
#'  \emph{University of Kansas Science Bulletin} \bold{38}: 1409-1438. \cr 
#'  
#'  Soerensen, T. (1948),
#'  \dQuote{A method of establishing groups of equal amplitude in plant sociology based on similarity of species content.},
#'  \emph{Biologiske Skrifter} \bold{4}: 1-34. \cr
#'
#' @export
#' @examples
#' library("similarity")
#'
#' a <- matrix(c(1, 1, 1, 0, 0, 0, 1, 1, 0, 1, 0, 1),
#'             ncol=3, nrow=4, byrow=TRUE)
#' similarity(a, "soerensen")
#'
#' @rdname similarity
#'
similarity <- function(x, method=c("soerensen-dice", "soerensen", "dice",
                                   "jaccard", "simple", "simplematching",
                                   "rogers", "tanimoto", "rogers-tanimoto")) {
  method <- match.arg(method, c("soerensen-dice", "soerensen", "dice",
                                "jaccard", "simple", "simplematching",
                                "rogers", "tanimoto", "rogers-tanimoto"),
                      several.ok=FALSE)

  stopifnot(is.binary.matrix(x))

  rowNames <- row.names(x)
  nr <- nrow(x)
  nc <- ncol(x)
  
  n11 <- tcrossprod(x)
  n01 <- tcrossprod(1-x, x)
  n10 <- tcrossprod(x, 1-x)
  n00 <- nc - n11 - n01 - n10

  s <- switch(method,
    dice=,
    soerensen=,
    "soerensen-dice"={ 2*n11/(n01+n10+2*n11) },
    jaccard={ n11/(n01+n10+n11) },
    simple=,
    simplematching={ (n00+n11)/(n00+n01+n10+n11) },
    rogers=,
    tanimoto=,
    "rogers-tanimoto"={ (n00+n11)/(n00+2*(n01+n10)+n11) },
    stop(sQuote(method), " is no suitable method!")
  )

  ## store diagonale (only needed for as.matrix)
  diag <- diag(s)

  s <- s[lower.tri(s)]

  attr(s, "Size") <- nr
  attr(s, "Labels") <- rowNames
  attr(s, "diagonalValues") <- diag
  attr(s, "method") <- method
  attr(s, "call") <- match.call()

  class(s) <- "similarity"

  return(s)
}

#' @method as.matrix similarity
#' @export
#'
as.matrix.similarity <- function(x, ...) {
  n <- attr(x, "Size")
  m <- matrix(0, n, n)

  m[lower.tri(m)] <- x
  m <- m + t(m)
  m[row(m) == col(m)] <- attr(x, "diagonalValues")

  rowNames <- attr(x, "Labels")

  if(is.null(rowNames)) {
    dimnames(m) <- list(seq_len(n), seq_len(n))
  } else {
    dimnames(m) <- list(rowNames, rowNames)
  }

  return(m)
}

#' @method print similarity
#' @export
#'
print.similarity <- function(x, ...) {
  stats:::print.dist(x, ...)
}

