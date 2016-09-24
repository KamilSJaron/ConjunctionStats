#' @title BlocksDistribution
#'
#' @description
#' \code{BlocksDistribution} is a function called inside a plot function.
#' It computes a theorerical distribution of blocks in case of two neigbouring
#' demes given selection against heterozygotes, recombination rate and
#' number of migrants per generation.
#'
#' @param x proportion of blocks (0 .. 1)
#'
#' @param S overall selecion acting on genome of hybrids
#'
#' @param R mean number of recombination events per generation per individual
#'
#' @param M number of migrants exchanged every generation
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export

BlocksDistribution <- function(x,S,R,M){
	theta = S / R
	y = ((M * theta) / (S * (1 + theta))) *
		((2 * x^(-1 * ((3+theta)/(1+theta)))) / (1 + theta))
	return(y)
}
