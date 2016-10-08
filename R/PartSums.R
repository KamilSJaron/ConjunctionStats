#' @title PartSums
#'
#' @description
#' \code{PartSums} computes a cumulative sum a of a vector
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export

PartSums <- function(vector){
	out <- c(vector[1])
	for(i in 2:length(vector)){
		out <- c(out,(out[i-1] + vector[i]))
	}
	return(out)
}
