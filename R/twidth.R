#' @title twidth
#'
#' @description
#' \code{twidth}
#'
#' @param s
#'
#' @param sigma
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export

# TODO: header
# comment: theoretical width. sigma^2=dispersal; s=selection

twidth = function(s, sigma = sqrt(0.5)){
	fact = sqrt(8)
	return((fact * sigma) / sqrt(s))
}
