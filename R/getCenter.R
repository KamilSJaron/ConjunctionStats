#' @title getCenter
#'
#' @description
#' \code{getCenter}is a function called inside \code{Fill2DSetting} and \code{FillSetting}
#'
#' @param simtab
#'
#' @param GradTableLine
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export

# TODO: header

getCenter <- function(simtab,GradTableLine){
	out <- tryCatch(
		{
			binit <- 4 * (sqrt(8)*(sqrt(0.5) / sqrt(GradTableLine$s)))^-1
			y = simtab$meanHI
			x = simtab$order
			fitModel = nls(y ~ a / (1 + exp(-b * (x - c))), 
			start = list(a = 1, b = binit, c = length(x) / 2))
			return(coef(fitModel)[3])
		},
		error <- function(cond) {
			return(NA)
		}
	)    
	return(out)
}
