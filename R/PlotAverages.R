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
#' @param plot_sd F; plot with averages also standard deviation
#'
#' @param epsilon 0.01 is width of the end of standard deviation bar
#'
#' @param epsilon 0.01 is width of the end of standard deviation bar
#'
#' @param ... is passed to lines (not to ploints)
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @importFrom RColorBrewer brewer.pal
#' @export

PlotAverages <- function(GradTable, stat, par1, par2, pal = NA, plot_sd = F,
                         epsilon = 0.01, add = T,
                         xlim = NA, ylim = NA, xlab = '', ylab = '', ...){
  if(any(is.na(pal))){
    pal <- brewer.pal(length(unique(GradTable[,par2])), 'Set1')
  }
  if(!add){
    if(any(is.na(xlim))){
      ylim <- c(min(GradTable[,stat]), max(GradTable[,stat]))
    }
    if(any(is.na(ylim))){
      ylim <- c(min(GradTable[,par1]), max(GradTable[,par1]))
    }
    plot(numeric(0), xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab)
  }
  for(par_state in unique(GradTable[,par2])){
    col <- pal[which(par_state == unique(GradTable[,par2]))]
    SubTable <- subset(GradTable, GradTable[,par2] == par_state)
    SubTable <- SubTable[order(SubTable[,par1]),]
    x <- unique(SubTable[,par1])
    y <- tapply(SubTable[,stat], SubTable[,par1], mean)
    sd <- tapply(SubTable[,stat], SubTable[,par1], sd)
    lines(x, y, col = col, ...)
    points(x, y, col = col, pch = 20)
    if(plot_sd){
      segments(x, y-sd, x, y+sd, col = col)
      segments(x-epsilon, y-sd, x + epsilon, y-sd, col = col)
      segments(x-epsilon, y+sd, x + epsilon, y+sd, col = col)
    }
  }
}
