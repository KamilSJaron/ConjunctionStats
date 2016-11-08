#' @title Fill2DSettingByHZAR
#'
#' @description
#' \code{Fill2DSettingByHZAR} computes a cline width and its center for every row of demes in 2D simulation
#' Warning: choose this function if Conjunction simulation was run in a 2D world;
#' choose alternative function \code{FillSetting} instead if Conjunction simulation was run in a 1D world.
#'
#' @param sim A string of characters indicating a ReadSummary object. Please refer to documentation of \code{ReadSummary} function for detailed usage.
#'
#' @param GradTable A string of characters indicating a ReadSetting object. Please refer to documentation of \code{ReadSetting} function for detailed usage.
#'
#' @return A data.frame object, integrating information about the settings used in the simulation run and the summary of the simulation run in a R-friendly format.
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export

Fill2DSettingByHZAR <- function(sim,GradTable){
	# lsize is the number of demes simulated perpendicular to the cline
	lsize <- sim[[1]]$DEME[sim[[1]]$DOWN == 0] + 1
	for(i in 1:lsize){
		GradTable[[paste("width_",i,sep='')]] <- 0
		GradTable[[paste("center_",i,sep='')]] <- 0
	}
	GradTable$total_demes <- 0
	deme_matrix <- matrix(sim[[1]]$DEME, nrow = lsize + 1)

	for(i in 1:length(sim)){
		GradTable$total_demes[i] <- nrow(sim[[i]])
		for(h in 1:lsize){
			subtable <- sim[[i]][sim[[i]]$DEME %in% deme_matrix[h,],]
			mknAdaA <- SummaryToHZAR(subtable, GradTable[i,])
			mknAdaAmodelData <- FitHZARmodel(mknAdaA);
			center <- unlist(mknAdaAmodelData$ML.cline$param.free[1])
			width <- unlist(mknAdaAmodelData$ML.cline$param.free[2])
			GradTable[[paste("center_",h,sep='')]][i] <- center
			GradTable[[paste("width_",h,sep='')]][i] <- width
		}
	}
	return(GradTable)
}
