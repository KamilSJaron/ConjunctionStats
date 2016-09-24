#' @title sigmoid
#'
#' @description
#' \code{sigmoid} logistic function, parameters a, b, c are members of vector
#' params
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export

sigmoid = function(params, x) {
	params[1] / (1 + exp(-params[2] * (x - params[3])))
}
