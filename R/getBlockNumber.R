#' @title getBlockNumber
#'
#' @description
#' \code{getBlockNumber}
#'
#' @param namePattern
#'
#' @param save
#'
#' @param lim
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#
#' @export

# TODO: header

getBlockNumber <- function(namePattern,save = 1,lim = 9){
	nameIn <- c()
	BlockNumber <- c()
	for(i in 1:lim){
		if(i > 10){
			ord <- i - 1
		} else {
			ord <- paste(0,i-1,sep='')
		}
		if(save == 0){
			nameIn <- c(nameIn,paste(paste(namePattern,'s',ord,sep='_'),'dat',sep='.'))
		}else{
			nameIn <- c(nameIn,paste(paste(namePattern,'s',ord,save,sep='_'),'dat',sep='.'))
		}
	}
	for(Line in 1:lim){
		blocks <- read.csv(nameIn[Line],header = F)
		BlockNumber <- c(BlockNumber,length(blocks$V1))
	}
	return(BlockNumber)
}
