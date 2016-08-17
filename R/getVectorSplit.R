#' @title getVectorSplit
#'
#' @description
#' \code{getVectorSplit} is a function called inside\code{ReadSetting} to integrate information from settings.txt file by parsing its content.
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export

getVectorSplit <- function(l){
  return(as.double(
    unlist(strsplit(
      strsplit(
        strsplit(l[2],
                 split=c('\\['))[[1]][2],
        split='\\]')[[1]][1],
      split="\\, |\\,|;"[[1]]))))
}
