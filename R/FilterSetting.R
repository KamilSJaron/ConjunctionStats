#' @title FilterSetting
#'
#' @description
#' \code{FilterSetting} wilter out values lower than treshold of parameters starting by the name speciefied by par
#'
#' @param GradTable
#'
#' @param par name pattern of parameters to be filtered (ex. width)
#'
#' @param treshold values to be replaced by NA (ex. 1)
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export

FilterSetting <- function(GradTable, par, treshold){
  selected_cols <- substr(colnames(GradTable), 1, nchar(par)) == par
  z <- as.matrix(GradTable[, selected_cols])
  z[z < treshold] <- NA
  GradTable[, selected_cols] <- z
  return(GradTable)
}
