#' @title Plot2DZone
#'
#' @description
#' \code{PlotBoxplots} is a function for ploting a set of boxplots of a stat
#' of a set of simulations with respect to two parameters specified by user
#'
#' @param GradTable A dataframe of 2D simulation (usually output of \code{FillSetting})
#'
#' @param stat The variable to be plot on x axis
#'
#' @param par1 The variable used for coloring
#'
#' @param par2 The variable used for line type
#'
#' @param pal is a vector of colours used for colouring categories of par1.
#' By default the pallete will be generated as 'RdYlGn' color brewer pallete with apropriate number of categories
#'
#' @param center T to centralize ploted stat for every simulation; F to plot stat without
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @examples
#'    Plot2DZone(GradTable, 'width', center = T)
#'    Plot2DZone(GradTable, 'center')
#'
#'
#' @importFrom RColorBrewer brewer.pal
#' @export

Plot2DZone <- function(GradTable, stat = 'width', par1 = 's', par2 = 'b', pal = NA, center = T){

  selected_cols <- which(substr(colnames(GradTable), 1, 5) == substr(stat, 1, 5))

  if(any(is.na(pal))){
    pal <- brewer.pal(length(unique(GradTable[,par1])),"RdYlGn")
  }

  z <- as.matrix(GradTable[, selected_cols])

  if(center){
    xlim = c(min(z - rowMeans(z)),max(z - rowMeans(z)))
    xlab = paste('deviation of', stat)
  } else {
    xlim = c(0, max(z[!is.na(z)]))
    xlab = stat
  }

  plot(numeric(0),
       xlim = xlim,
       ylim = c(0,length(selected_cols)-1),
       ylab = 'horizontal position', xlab = xlab,
       yaxt = "n",cex.lab=0.8, cex.axis=0.8)

  ycoor <- seq(length(selected_cols),-1, by = -1)

  for(val2 in unique(GradTable[,par2])){
    lty = which(unique(GradTable[,par2]) == val2)

    for(val1 in unique(GradTable[,par1])){
      col = pal[which(unique(GradTable[,par1]) == val1)]
      z <- as.matrix(GradTable[GradTable[,par2] == val2 &
                               GradTable[,par1] == val1, selected_cols])
      for(i in 1:nrow(z)){
        if(center){
          xcoor <- z[i,] - mean(z[i,])
        } else {
          xcoor <- z[i,]
        }
        lines(ycoor ~ c(xcoor[length(xcoor)], xcoor, xcoor[1]),
              col = col, lty = lty, lwd = 0.8)
      }
    }
  }

  legend('left', pch = 20, legend = unique(GradTable[,par1]),
         title = par1, col = pal, cex = 0.75, bty = "n")
  legend('bottomleft', lty = 1:3, legend = unique(GradTable[,par2]),
         title = par2, cex = 0.75, bty = "n")
  axis(2, at=0:7, labels=c(8:1), cex.axis = 0.8)
}
