#' @title sigmoid
#'
#' @description
#' \code{sigmoid}
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export

# TODO: header

sigmoid = function(params, x) {
	params[1] / (1 + exp(-params[2] * (x - params[3])))
}
