#' @title PlotWidths
#'
#' @description
#' \code{PlotWidths} is a function for ploting widths with respect to one or two parameters specified by user
#'
#' @param GradTable A filled table of simulations
#'
#' @param par1 The variable to be plot on x axis
#'
#' @param par2 The variable used for the coloring of points
#'
#' @param legend_position is changing a position of the legend in the picture
#'
#' @param if add = T funciton will just add points to existing plot
#'
#' @param pal is a vector of colous used for colouring categories of the second parameter. By default the pallete will be generated as 'Set1' color brewer pallete with apropriate number of categories
#'
#' @param xlim is equvalent of parameter xlim from plot
#'
#' @param ylim is equvalent of ylim from plot
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export

PlotWidths <- function(GradTable, par1 = 's', par2 = NA,
                       legend_position = 'topright', add = F, pal = NA,
                       xlim = NA, ylim = NA){
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
    ylim = c(min(GradTable$width_l, na.rm = T),max(GradTable$width_l, na.rm = T))
  }
  # ploting frame
  if(add == F){
    plot(numeric(0),
      xlim = xlim,
      ylim = ylim,
      xlab = par1, ylab = 'cline width')
  }

  # ploting data
  if(is.na(par2)){
    points(GradTable[,par1],  GradTable$width_l, pch = 20)
  } else {
    if(any(is.na(pal))){
      require(RColorBrewer)
      pal <- brewer.pal(length(unique(GradTable[,par2])), 'Set1')
    }
    for(par_state in unique(GradTable[,par2])){
      SubTable <- subset(GradTable, GradTable[,par2] == par_state)
      points(SubTable[,par1],  SubTable$width_l,
        pch = 20,
        col = pal[which(par_state == unique(GradTable[,par2]))])
    }
    legend(legend_position, col = pal, legend = unique(GradTable[,par2]),
      title = par2, pch = 20)
  }
}
