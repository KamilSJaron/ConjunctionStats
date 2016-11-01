#' @title PlotAverages
#'
#' @description
#' \code{PlotAverages} add averages to a plot, usually combined with PlotStat.
#'
#' @param GradTable A dataframe (usually output of \code{FillSetting}) or
#' \code{FillSettingByHZAR}
#'
#' @param stat The variable to be plot on y axis
#'
#' @param par1 The variable to be plot on x axis
#'
#' @param par2 The variable used for the coloring of points
#'
#' @param pal is a vector of colous used for colouring categories of the second parameter. By default the pallete will be generated as 'Set1' color brewer pallete with apropriate number of categories
#'
#' @param ... is passed to lines (not to ploints)
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export

PlotAverages <- function(GradTable, stat, par1, par2, pal = NA, ...){
  if(any(is.na(pal))){
    pal <- brewer.pal(length(unique(GradTable[,par2])), 'Set1')
  }
  for(par_state in unique(GradTable[,par2])){
    col <- pal[which(par_state == unique(GradTable[,par2]))]
    SubTable <- subset(GradTable, GradTable[,par2] == par_state)
    lines(tapply(SubTable[,stat],
          SubTable[,par1], mean) ~ sort(unique(SubTable[,par1])), col = col, ...)
    points(tapply(SubTable[,stat],
           SubTable[,par1], mean) ~ sort(unique(SubTable[,par1])),
           col = col, pch = 20)
  }
}
