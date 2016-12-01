#' @title PlotBoxplots
#'
#' @description
#' \code{PlotBoxplots} is a function for ploting a set of boxplots of a stat
#' of a set of simulations with respect to two parameters specified by user
#'
#' @param GradTable A dataframe (usually output of \code{FillSetting}) or
#' \code{FillSettingByHZAR}
#'
#' @param stat The variable to be plot on y axis (used for construction of histograms)
#'
#' @param par1 The variable to be plot on x axis
#'
#' @param par2 The variable used for the coloring of histograms
#'
#' @param legend_position is changing a position of the legend in the picture
#'
#' @param pal is a vector of colous used for colouring categories of the second parameter. By default the pallete will be generated as 'Set1' color brewer pallete with apropriate number of categories
#'
#' @param xlim is equvalent of parameter xlim from plot
#'
#' @param ylim is equvalent of ylim from plot
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @examples
#'    PlotBoxplots(GradTable, 'width', 's', 'D', 'topright')
#'
#'
#' @importFrom RColorBrewer brewer.pal
#' @export

PlotBoxplots <- function(GradTable, stat, par1, par2,
                           legend_position = NA, pal = NA, xlim = NA, ylim = NA){
  if (any(is.na(xlim))) {
    xlim = c(min(GradTable[, par1]), max(GradTable[, par1]))
  }
  if (any(is.na(ylim))) {
    ylim = c(min(GradTable[, stat], na.rm = T), max(GradTable[,
        stat], na.rm = T))
  }
  if(any(is.na(pal))){
      pal <- brewer.pal(length(unique(GradTable[, par2])), "Set1")
  }

  bins <- length(unique(GradTable[, par2])) + 1
  step <- min(diff(sort(unique(GradTable[, par1])))) / bins
  box_width <- diff(xlim) / ((length(unique(GradTable[, par1])) + 1) *
                             length(unique(GradTable[, par2])))

  plot(numeric(0), xlim = xlim + c(-step,step), ylim = ylim,
       xlab = par1, ylab = stat)

  if(stat == 'width' & par1 == 's'){
    x <- seq(0, 1, by = 0.01)
    lines(x, twidth(x, sqrt(0.5)))
  }

  for(p1 in unique(GradTable[, par1])){
    for(p2 in unique(GradTable[, par2])){
      p2_order <- which(p2 == unique(GradTable[, par2]))
      p1_at <- p1 - (((bins / 2) - p2_order) * step)
      boxplot(GradTable[GradTable[, par2] == p2 & GradTable[, par1] == p1, stat],
              at = p1_at,
              boxwex = box_width,
              add = T,
              col = pal[p2_order], pch = 20, cex = 1 / bins)
    }
  }
  legend(legend_position, col = pal, legend = unique(GradTable[, par2]),
         pch = 20, title = par2, bty = "n")
}
