#' @title hmean
#'
#' @description
#' \code{hmean} computes an harmonic mean of all input non-NA values
#'
#' @param x input vector of numbers
#'
#' @param ... just to avoid error when more parameters are passed (like na.rm)
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export

hmean <- function(x, ...){
  sum(!is.na(x)) / sum(1 / x, na.rm = T)
}
