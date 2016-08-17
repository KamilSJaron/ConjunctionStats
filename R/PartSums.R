#' @title PartSums
#'
#' @description
#' \code{PartSums}
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export

# TODO: header

PartSums=function(vector){
	out <- c(vector[1])
	for(i in 2:length(vector)){
		out <- c(out,(out[i-1] + vector[i]))
	}
	return(out)
}
