#' @title Read0DSummary
#'
#' @description
#' \code{Read0DSummary}
#'
#' @param summaryFile
#'
#' @return A list of data.frame objects, in which each list item, a data.frame, is a simulation run, and every row of the data.frame is a deme.
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export

Read0DSummary <- function(summaryFile){
    sim <- list()
    header <- strsplit(LogFile[2], split = " ")[[1]][strsplit(LogFile[2], split = " ")[[1]] != ""]
    i <- 1

    for(line_of_sim in seq(3,length(LogFile), by = 3)){
        l <- LogFile[line_of_sim]
        newl <- strsplit(l, split = " ")[[1]][strsplit(l, split = " ")[[1]] != ""]
        new_sim <- data.frame(matrix(as.numeric(newl), ncol = length(header)))
        colnames(new_sim) <- header
        sim[[i]] <- new_sim
        i <- i + 1
    }
    return(sim)
}
