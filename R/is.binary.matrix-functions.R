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

#' Is a matrix binary one?
#'
#' Tests whether given matrix is a binary one.
#' 
#' @param x matrix
#' @return TRUE/FALSE
#' @export
#' @examples
#' library("similarity")
#'
#' m <- matrix(c(1, 2, NA, 2), nrow=2, ncol=2)
#' b <- matrix(c(1, 1, 0, 1), nrow=2, ncol=2)
#'
#' is.binary.matrix(m)    # FALSE
#' is.binary.matrix(1:10) # FALSE
#' is.binary.matrix(b)    # TRUE
#'
is.binary.matrix <- function(x) {
  return(is.matrix(x) && all(x == 1 | x == 0))
}
