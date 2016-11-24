#' @title ReadSpatialSummary
#'
#' @description
#' \code{ReadSpatialSummary}
#'
#' @param summaryFile
#'
#' @return A list of data.frame objects, in which each list item, a data.frame, is a simulation run, and every row of the data.frame is a deme.
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export

ReadSpatialSummary <- function(summaryFile){
#   index of red simulation
    i = 0
#   sim is list of all simulations
    sim <- list()
    for(l in summaryFile){
        if("EDGE" %in% strsplit(l,split=' ')[[1]]){
#           saving sequence only if it is not first occurance of key sequence EDGE
            if(i > 0){
                names(tableTemp) <- ColNames
                tableTemp$order <- 0
                tableTemp$order[which(!(tableTemp$LEFT %in% tableTemp$DEME))] <- 1
                nextRight <- tableTemp$RIGHT[tableTemp$order == 1]
                for(j in 2:nrow(tableTemp)){
                    tableTemp$order[tableTemp$DEME == nextRight] <- j
                    nextRight <- tableTemp$RIGHT[tableTemp$order == j]
                }
                sim[[i]] <- tableTemp
            }
#           restart table for simulation and add 1 to index
            i = i + 1
            tableTemp <- data.frame()
        } else {
            newl <- strsplit(l, split = " ")[[1]][strsplit(l, split = " ")[[1]] != ""]
            if(newl[1] == 'DEME'){
                ColNames <- newl
                if(ColNames[4] == 'mean'){
                    ColNames <- ColNames[-4]
                    ColNames[4] <- 'meanf'
                }
                if(ColNames[6] == 'mean'){
                    ColNames <- ColNames[-6]
                    ColNames[6] <- 'meanf'
                }
            }
            newl <- strsplit(l, split = " ")[[1]][strsplit(l, split = " ")[[1]] != ""]
            if(grepl("[[:digit:]]",newl[1])){
                newl[1] <- gsub("[^0-9]", "", newl[1])
                tableTemp <- rbind(tableTemp,as.numeric(newl))
            }
        }
    }
    names(tableTemp) <- ColNames
    tableTemp$order <- 0
    tableTemp$order[which(!(tableTemp$LEFT %in% tableTemp$DEME))] <- 1
    nextRight <- tableTemp$RIGHT[tableTemp$order == 1]
    for(j in 2:nrow(tableTemp)){
        tableTemp$order[tableTemp$DEME == nextRight] <- j
        nextRight <- tableTemp$RIGHT[tableTemp$order == j]
    }
    sim[[i]] <- tableTemp
    return(sim)
}
