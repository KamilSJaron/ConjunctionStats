#' @title Fill2DSummary
#'
#' @description
#' \code{Fill2DSummary} integrates the the output of \code{ReadSummary} and \code{ReadSetting} functions into a list of data.frame objects. Warning: choose this function if Conjunction simulation was run in a 2D world; choose alternative function \code{FillSummary} instead if Conjunction simulation was run in a 1D world.
#'
#' @param sim A string of characters indicating a ReadSummary object. Please refer to documentation of \code{ReadSummary} function for detailed usage. 
#'
#' @param GradTable
#'
#' @return A data.frame object, integrating information about the settings used in the simulation run and the summary of the simulation run in a R-friendly format.
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#
#' @examples{
#'    mySim=ReadSummary(nameIn='../../Conjunction/out')
#'    myReadSetting=ReadSetting(InputFile='../../Conjunction/setting.txt')
#'    myCenters=myReadSetting$C
#'    myFill2DSummary = Fill2DSummary(sim=mySim, centers=myCenters)
#' }
#'
#' @export

#TODO: comment inline, notes

Fill2DSummary <- function(sim,GradTable){
	lsize <- sim[[1]]$DEME[sim[[1]]$DOWN == 0] 
	for(i in 1:length(sim)){
		B1s <- seq(max(sim[[i]]$order),max(sim[[i]]$order),length = (lsize + 1))
		B0s <- seq(0,0,length = (lsize + 1))
		for(h in 0:lsize){
			selected <- seq(h,max(sim[[i]]$DEME),by = lsize+1)
			subtable <- sim[[i]][sim[[i]]$DEME %in% selected,]	
			if(any(subtable$meanHI == 1)){
				B1 <- min(subtable$order[subtable$meanHI == 1]):max(subtable$order[subtable$meanHI == 1])
				B1s[h+1] <- max(c(B1[!(B1 %in% subtable$order[subtable$meanHI == 1])],min(subtable$order[subtable$meanHI == 1]) - 1))
			}
			if(any(subtable$meanHI == 0)){
				B0 <- min(subtable$order[subtable$meanHI == 0]):max(subtable$order[subtable$meanHI == 0])
				B0s[h+1] <- min(c(B0[!(B0 %in% subtable$order[subtable$meanHI == 0])]),max(subtable$order[subtable$meanHI == 0]) + 1)
			}
		}
		minB0 <- min(B0s)
		maxB1 <- max(B1s)
		
		sim[[i]] <- sim[[i]][sim[[i]]$order %in% (minB0:maxB1),]
		ordervec <- sim[[i]]$order - minB0 + 1
		sim[[i]]$order = ordervec
	}
	
	return(sim)
}
