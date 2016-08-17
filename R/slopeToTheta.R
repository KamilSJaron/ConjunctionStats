#' @title slopeToTheta
#'
#' @description
#' \code{slopeToTheta}
#'
#' @param slope
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export

# TODO: header

slopeToTheta <- function(slope){
  # slope = - (3 + theta) / (1 + theta)
  # slope * (1 + theta) + theta = - 3
  # theta  = -3 - slope / (slope + 1)
  return( (-3 - slope) / (slope + 1) )
}
