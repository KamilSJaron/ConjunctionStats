#' @title getAUFC
#'
#' @description
#' \code{getAUFC} computes Area Under Fitness Curve
#' defined by the Barton's fitness function 1 - s (4x (1 - x)^beta)
#' where, s is selection against the most admixed individuals and
#' beta is setrength of epistasic effect
#'
#' @param selection
#'
#' @param beta
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export

GetAUFC <- function(selection, beta){
  return( 1 -
          (4^beta * selection * gamma(beta + 1)^2) /
          (gamma(2 * beta + 2))
        )
}
