#' @title ReadSummary
#'
#' @description
#' \code{ReadSummary} parses the output file of a simulation run using Conjunction software and converts it to a list of data.frame objects.
#'
#' @param nameIn A string of characters indicating the simulation output file to be parsed. The output file can be defined following instructions on: https://github.com/KamilSJaron/Conjunction/wiki#usage
#'
#' @return A list of data.frame objects, in which each list item, a data.frame, is a simulation run, and every row of the data.frame is a deme.
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @examples{
#'    myReadSummary = ReadSummary(nameIn='./mySimulationOutput')
#' }
#'
#' @export

ReadSummary <- function(nameIn){
    summaryFile <- readLines(nameIn)

    is_zero = "0D" %in% strsplit(summaryFile[1],' ')[[1]]

    if(is_zero){
      return(Read0DSummary(summaryFile))
    } else {
      return(ReadSpatialSummary(summaryFile))
    }
}
