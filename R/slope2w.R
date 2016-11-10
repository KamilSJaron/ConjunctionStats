#' @title slope2w
#'
#' @description
#' \code{slope2w} width represented as parameter of logistic function have to be
#' multiplied by four to get a slope in hypertangent function. This function
#' computes width as an inverse of slope after conversion to hypertangent units
#'
#' @param slope
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export

slope2w <- function(slope){
    return(4 / slope)
}
