#' @title getBoth
#'
#' @description
#' \code{getBoth} is a function called inside \code{FillSetting}.
#'
#' @param simtab
#'
#' @param s
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export

# TODO: header

getBoth <- function(simtab,s){
	out <- tryCatch(
		{
			binit <- 4 * (sqrt(8)*(sqrt(0.5) / sqrt(s)))^-1
			y = simtab$meanHI
			x = simtab$order

			fitModel = nls(y ~ a / (1 + exp(-b * (x - c))),start = list(a = 1, b = binit, c = length(x) / 2))
			res <- c(coef(fitModel)[2],coef(fitModel)[3])
			return(res)
		},
		error=function(cond) {
			return(NA)
		}
	)
	return(out)
}
