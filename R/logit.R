#' @title logit
#'
#' @description
#' \code{logit}
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export

# TODO: header

logit=function(vector){
	return(log(vector / (1 - vector)))
}
