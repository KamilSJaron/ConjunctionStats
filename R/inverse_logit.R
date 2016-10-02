#' @title inverse_logit
#'
#' @description
#' \code{inverse_logit} is function a reverse to logit transformation
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export

inverse_logit <- function(vector){
	return(exp(vector) / (1 + exp(vector)))
}
