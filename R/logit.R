#' @title logit
#'
#' @description
#' \code{logit} transformation
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export

logit <- function(vector){
	return(log(vector / (1 - vector)))
}
