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

#' Converts a similarity matrix into a distance matrix.
#'
#' This function converts a \code{\link{similarity}} matrix into a
#' \code{\link[stats]{dist}} matrix.
#'
#' @details
#' To transform the similarity matrix into a distance matrix the following
#' equation is used: \eqn{d = 1 - s}
#' 
#' @method as.dist similarity
#' @param m matrix
#' @param diag logical, should \code{\link[stats]{print.dist}} prints the diagonal?
#' @param upper logical, should \code{\link[stats]{print.dist}} prints the upper
#'  triangel?
#' @return an object of class \code{\link[stats]{dist}}. 
#' @seealso \code{\link[stats]{dist}}, \code{\link[stats]{print.dist}} 
#' @importFrom stats as.dist
#' @export
#' @examples
#' library("similarity")
#'
#' a <- matrix(c(1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 0, 1),
#'             ncol=3, nrow=4, byrow=TRUE)
#' s <- similarity(a, "jaccard")
#' as.dist(s)
#'
as.dist.similarity <- function(m, diag=FALSE, upper=FALSE) {
  m <- as.matrix(m)
  ## turn similarity into a dissimilarity matrix
  d <- sqrt(outer(X=diag(m), Y=diag(m), FUN="+") - 2 * m)

  return(as.dist(d, diag=diag, upper=upper))
}

