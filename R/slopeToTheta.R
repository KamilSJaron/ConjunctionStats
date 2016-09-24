#' @title slopeToTheta
#'
#' @description
#' \code{slopeToTheta} function made for backward engenerring. It computes theta
#' from the slope of the log log distribution (which is linear)
#'
#' @param slope
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export

slopeToTheta <- function(slope){
  # slope = - (3 + theta) / (1 + theta)
  # slope * (1 + theta) + theta = - 3
  # theta  = -3 - slope / (slope + 1)
  return( (-3 - slope) / (slope + 1) )
}
