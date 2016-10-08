#' @title getIntegratedSelection
#'
#' @description
#' \code{getIntegratedSelection} computes area under curve of selection function
#' defined by the Barton's selection function 1 - s (4x (1 - x)^beta)
#'
#' @param selection
#'
#' @param beta
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export


getIntegratedSelection <- function(selection, beta){
  return( 1 - (4 * s / (b^2 + 3*b + 2)))
}
