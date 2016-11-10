#' @title ss2w
#'
#' @description
#' \code{ss2w}
#'
#' @param ss is s*, effective selection
#'
#' @param sigma is square root of dispersal
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export

ss2w <- function(ss,sigma = sqrt(0.5)){
    return((2 * sigma) / sqrt(ss))
}
