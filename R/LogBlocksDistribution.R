#' @title LogBlocksDistribution
#'
#' @description
#' \code{LogBlocksDistribution}
#'
#' @param x
#'
#' @param S
#'
#' @param R
#'
#' @param M
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export

# TODO: header

LogBlocksDistribution <- function(x,S,R,M){
  theta = S / R
  logy = log((M * theta * 2) / (S * (1 + theta)^2)) - (((3 + theta) * x) / (1 + theta))
  return(logy)
}
