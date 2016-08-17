#' @title ss2w
#'
#' @description
#' \code{ss2w}
#'
#' @param ss 
#'
#' @param sigma
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export

# TODO: header
# comment: s-star to width

ss2w = function(ss,sigma = sqrt(0.5)){
	return((2 * sigma) / sqrt(ss))
}
