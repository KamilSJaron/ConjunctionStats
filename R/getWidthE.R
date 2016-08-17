#' @title getWidthE
#'
#' @description
#' \code{getWidthE} is a function called inside \code{FillSetting}.
#'
#' @param simtab
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export

# TODO: header

getWidthE <- function(simtab){
	y = logit(simtab$meanHI[simtab$meanHI > 0.2 & simtab$meanHI < 0.80])
	x = simtab$order[simtab$meanHI > 0.2 & simtab$meanHI < 0.80]
	if(length(x) > 1){
		mod8020 <- lm(y ~ x)
		return(4 / mod8020$coefficients[2])
	} else {
		y = logit(simtab$meanHI[simtab$meanHI > 0.15 & simtab$meanHI < 0.85])
		x = simtab$order[simtab$meanHI > 0.15 & simtab$meanHI < 0.85]
		if(length(x) > 1){
			mod8020 <- lm(y ~ x)
			return(4 / mod8020$coefficients[2])
		} else {
			return(NA)
		}
	}
}
