#' @title FillSettingByNLMByNLM
#'
#' @description
#' \code{FillSettingByNLMByNLM} integrates the the output of \code{ReadSummary} and \code{ReadSetting} functions into a data.frame object.
#' Warning: choose this function if Conjunction simulation was run in a 1D world;
#' choose alternative function \code{Fill2DSetting} instead if Conjunction simulation was run in a 2D world.
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

FillSettingByNLM <- function(sim,GradTable){
    GradTable$slope <- 0
    GradTable$center <- 0
    #GradTable$ss <- 0
    GradTable$total_demes <- 0
    GradTable$width <- 0
    for(i in 1:length(sim)){
        GradTable$total_demes[i] <- nrow(sim[[i]])
        if(any(sim[[i]]$meanHI == 1)){
            B1 <- min(sim[[i]]$order[sim[[i]]$meanHI == 1]):max(sim[[i]]$order[sim[[i]]$meanHI == 1])
            maxB1 <- max(c(B1[!(B1 %in% sim[[i]]$order[sim[[i]]$meanHI == 1])],min(sim[[i]]$order[sim[[i]]$meanHI == 1]) - 1))
        } else {
            maxB1 <- max(sim[[i]]$order)
        }
        if(any(sim[[i]]$meanHI == 0)){
            B0 <- min(sim[[i]]$order[sim[[i]]$meanHI == 0]):max(sim[[i]]$order[sim[[i]]$meanHI == 0])
            minB0 <- min(c(B0[!(B0 %in% sim[[i]]$order[sim[[i]]$meanHI == 0])]),max(sim[[i]]$order[sim[[i]]$meanHI == 0]) + 1)
        } else {
            minB0 <- 1
        }
        sim[[i]] <- sim[[i]][sim[[i]]$order %in% (minB0:maxB1),]
        ordervec <- sim[[i]]$order - minB0 + 1
        sim[[i]]$order = ordervec
        GradTable$slope[i] <- getSlope(sim[[i]],GradTable[i,])
        GradTable$center[i] <- getCenter(sim[[i]],GradTable[i,])
        # ss is bit unfortunate name
        #meanf <- sim[[i]]$meanf[order(sim[[i]]$order)]
        #GradTable$ss[i] <- getSstar(meanf, 0.05, 1)
    }
    GradTable$width <- 4 * (GradTable$slope^-1)
    return(GradTable)
}
