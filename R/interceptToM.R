#' @title interceptToM
#'
#' @description
#' \code{interceptToM}
#'
#' @param intercept
#'
#' @param selection
#'
#' @param theta
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export

# TODO: header

interceptToM <- function(intercept, selection, theta){
  # intercept = log((M * theta * 2) / (S * (1 + theta)^2))
  # exp(intercept) = (M * theta * 2) / (S * (1 + theta)^2)
  # (exp(intercept) * (S * (1 + theta)^2)) / (theta * 2) = M   
  return( (exp(intercept) * (selection * (1 + theta)^2)) / (theta * 2) )
}
