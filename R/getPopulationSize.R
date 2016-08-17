#' @title getPopulationSize
#'
#' @description
#' \code{getPopulationSize} 
#'
#' @param LogFile
#'
#' @param generation
#' 
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#
#' @examples{
#'   
#' }
#'
#' @export

# TODO: header
# comment: for 0D

getPopulationSize = function(LogFile,generation){
	Js <- c()
	counter = 0
	for(l in LogFile){
		counter = counter + 1 
		if("Selection" %in% (strsplit(l,split=':')[[1]])){
			counter <- 0
		}
		if(counter == generation){
			if(generation == 0){
				Js <- c(Js,as.numeric(strsplit(ll,split=' ')[[1]][3]))
			} else {
				Js <- c(Js,as.numeric(strsplit(l,split=' ')[[1]][3]))
			}
		}
		ll = l
	}
	return(Js)
}
