#' @title inverse_logit
#'
#' @description
#' \code{inverse_logit}
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export

# TODO: header

inverse_logit=function(vector){
	return(exp(vector) / (1 + exp(vector)))
}
