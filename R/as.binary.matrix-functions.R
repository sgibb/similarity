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

#' Converts a matrix into binary one.
#'
#' This function converts a matrix into binary (0/1).
#' \code{NA} values become zero and \code{!NA} values one.
#' 
#' @param x matrix
#' @return matrix
#' @export
#' @examples
#' library("similarity")
#'
#' m <- matrix(c(1, 2, NA, 2), nrow=2, ncol=2)
#' as.binary.matrix(m)
#' #     [,1] [,2]
#' #[1,]    1    0
#' #[2,]    1    1
#'
as.binary.matrix <- function(x) {
  stopifnot(is.matrix(x))

  if (is.binary.matrix(x)) {
    return(x)
  }

  na <- is.na(x)
  x[na] <- 0
  x[!na] <- 1

  return(x)
}

