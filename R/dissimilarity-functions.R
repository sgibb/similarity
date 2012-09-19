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
#' This function calculates different dissimilarity indices for a binary matrix.
#' It is only a wrapper around \code{\link{similarity}} and
#' \code{\link{as.dist.similarity}}.
#'
#' @param x a \code{\link{matrix}}. Please take care that rows contain the
#'  samples and columns the features.
#' @param ... further arguments passed to \code{\link{similarity}}.
#'
#' @export
#' @examples
#' library("similarity")
#'
#' a <- matrix(c(1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 0, 1),
#'             ncol=3, nrow=4, byrow=TRUE)
#' dissimilarity(a, "soerensen")
#'
#' @seealso \code{\link{similarity}}, \code{\link{as.dist.similarity}}
#' @rdname dissimilarity
#'
dissimilarity <- function(x, ...) {
  s <- similarity(x=x, ...)
  return(as.dist(s))
}

