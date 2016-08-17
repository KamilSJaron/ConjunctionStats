#' @title EquilG
#'
#' @description
#' \code{EquilG}
#'
#' @param: theta
#' 
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export

# TODO: header
# comment: percentage of genome acting together under selection

EquilG <- function(theta){
	return(1 - (1 / theta))
}
