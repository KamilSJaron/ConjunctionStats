#' @title PlotStat
#'
#' @description
#' \code{PlotStat} is a function for ploting a stat of set of simulation with respect to one or two parameters specified by user
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
#' @param legend_position is changing a position of the legend in the picture
#'
#' @param add if = T funciton will just add points to existing plot
#'
#' @param pal is a vector of colous used for colouring categories of the second parameter. By default the pallete will be generated as 'Set1' color brewer pallete with apropriate number of categories
#'
#' @param xlim is equvalent of parameter xlim from plot
#'
#' @param ylim is equvalent of ylim from plot
#'
#' @param ... addenitional arguments passed to plot function
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @importFrom RColorBrewer brewer.pal
#' @export

PlotStat <- function(GradTable, stat = 'width_l', par1 = 's', par2 = NA,
                       legend_position = 'topright', add = F, pal = NA,
                       xlim = NA, ylim = NA, ...){
  # testing
  if(!(par1 %in% colnames(GradTable))){
    message('parameter par1 has to be name of one of the columns of the input table')
  }
  if(!(par2 %in% colnames(GradTable)) & !is.na(par2)){
    message('parameter par2 has to be name of one of the columns of the input table')
    message('or not specified (NA)')
  }

  # if limit are not specified
  if(any(is.na(xlim))){
    xlim = c(min(GradTable[,par1]),max(GradTable[,par1]))
  }
  if(any(is.na(ylim))){
    ylim = c(min(GradTable[,stat], na.rm = T),max(GradTable[,stat], na.rm = T))
  }
  # ploting frame
  if(add == F){
    plot(numeric(0),
      xlim = xlim,
      ylim = ylim, ...)
  }

  # ploting data
  if(is.na(par2)){
    points(GradTable[,par1],  GradTable[,stat], pch = 20)
  } else {
    if(any(is.na(pal))){
      pal <- brewer.pal(length(unique(GradTable[,par2])), 'Set1')
    }
    for(par_state in unique(GradTable[,par2])){
      SubTable <- subset(GradTable, GradTable[,par2] == par_state)
      points(SubTable[,par1],  SubTable[,stat],
        pch = 20,
        col = pal[which(par_state == unique(GradTable[,par2]))])
    }
    legend(legend_position, col = pal, legend = unique(GradTable[,par2]),
      title = par2, pch = 20)
  }
}
