#' @title FillSummary
#'
#' @description
#' \code{FillSummary} integrates the the output of \code{ReadSummary} and \code{ReadSetting} functions into a list of data.frame objects. Warning: choose this function if Conjunction simulation was run in a 1D world; choose alternative function \code{Fill2DSummary} instead if Conjunction simulation was run in a 2D world.
#'
#' @param sim A string of characters indicating a ReadSummary object. Please refer to documentation of \code{ReadSummary} function for detailed usage. 
#'
#' @param centers A string of characters indicating a center variable of ReadSetting object. Please refer to documentation of \code{ReadSetting} function for detailed usage. 
#'
#' @return A data.frame object, integrating information about the settings used in the simulation run and the summary of the simulation run in a R-friendly format.
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#
#' @examples{
#'    mySim=ReadSummary(nameIn='../../Conjunction/out')
#'    myReadSetting=ReadSetting(InputFile='../../Conjunction/setting.txt')
#'    myCenters=myReadSetting$C
#'    myFillSummary = FillSummary(sim=mySim, centers=myCenters)
#' }
#'
#' @export

#TODO: comment inline, notes

FillSummary <- function(sim,centers){
	for(i in 1:length(sim)){	
		B1max <- max(sim[[i]]$order)
		B0min <- 1
		if(any(sim[[i]]$meanHI == 1)){
			B1 <- min(sim[[i]]$order[sim[[i]]$meanHI == 1]):max(sim[[i]]$order[sim[[i]]$meanHI == 1])
			B1max <- max(c(B1[!(B1 %in% sim[[i]]$order[sim[[i]]$meanHI == 1])],min(sim[[i]]$order[sim[[i]]$meanHI == 1]) - 1))
		}
		if(any(sim[[i]]$meanHI == 0)){
			B0 <- min(sim[[i]]$order[sim[[i]]$meanHI == 0]):max(sim[[i]]$order[sim[[i]]$meanHI == 0])
			B0min <- min(c(B0[!(B0 %in% sim[[i]]$order[sim[[i]]$meanHI == 0])]),max(sim[[i]]$order[sim[[i]]$meanHI == 0]) + 1)
		}
		sim[[i]] <- sim[[i]][sim[[i]]$order %in% (B0min:B1max),]
		ordervec <- sim[[i]]$order - B0min + 1
		sim[[i]]$order = ordervec
		sim[[i]]$centered <- ordervec - centers[i]
	}
	return(sim)
}
