#' @title BlocksDistribution
#'
#' @description
#' \code{BlocksDistribution} is a function called inside a plot function. 
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

BlocksDistribution <- function(x,S,R,M){
	theta = S / R
	y = ((M * theta) / (S * (1 + theta))) * 
		((2 * x^(-1 * ((3+theta)/(1+theta)))) / (1 + theta))
	return(y)
}
