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
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export

PlotWidths <- function(GradTable, par1 = 's', par2 = NA){
  # testing
  if(!(par1 %in% colnames(GradTable))){
    message('parameter par1 has to be name of one of the columns of the input table')
  }
  if(!(par2 %in% colnames(GradTable)) & !is.na(par2)){
    message('parameter par2 has to be name of one of the columns of the input table')
    message('or not specified (NA)')
  }
  # ploting
  if(is.na(par2)){
    plot(GradTable[,par1],  GradTable$width_l,
      xlab = par1, ylab = 'cline width',
      pch = 20)
  } else {
    require(RColorBrewer)
    plot(numeric(0),
      xlim = c(min(GradTable[,par1]),max(GradTable[,par1])),
      ylim = c(min(GradTable$width_l, na.rm = T),max(GradTable$width_l, na.rm = T)),
      xlab = par1, ylab = 'cline width')
    col_palette <- brewer.pal(length(unique(GradTable[,par2])), 'Set1')
    for(par_state in unique(GradTable[,par2])){
      SubTable <- subset(GradTable, GradTable[,par2] == par_state)
      points(SubTable[,par1],  SubTable$width_l,
        pch = 20,
        col = col_palette[which(par_state == unique(GradTable[,par2]))])
    }
    legend('topright', col = col_palette, legend = unique(GradTable[,par2]),
      title = par2, pch = 20)
  }
}
