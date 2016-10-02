#' @title PlotSim
#'
#' @description
#' \code{PlotSim} is a function for ploting an overview of one simulation
#'
#' @param onesim is a data frame of stats of one simulation
#'
#' @param Gradline a line of GradTable A filled table of simulations
#'
#' @param legend_position is changing a position of the legend in the picture
#'
#' @param add if True funciton will just add points to existing plot
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

PlotSim <- function(onesim, Gradline,
                    legend_position = 'bottomright', add = F, pal = NA,
                    xlim = NA, ylim = NA){

  # if limit are not specified
  if(any(is.na(xlim))){
    xlim = c(min(onesim$centered),max(onesim$centered))
  }
  if(any(is.na(ylim))){
    ylim = c(0,1)
  }
  # ploting frame
  if(add == F){
    plot(numeric(0),
      xlim = xlim,
      ylim = ylim,
      xlab = 'centralized index', ylab = '')
  }

# ploting data
  if(any(is.na(pal))){
    require(RColorBrewer)
    pal <- brewer.pal(4, 'Set1')
  }

	# meanf f(heter)   meanHI  var(HI)   var(p)       LD
  points(onesim$centered, onesim$meanf,  pch = 20, col = pal[1])
  lines(onesim$centered, onesim$meanf, col = pal[1])

	points(onesim$centered, onesim$meanHI,  pch = 20, col = pal[2])
  lines(onesim$centered, onesim$meanHI, col = pal[2])

	points(onesim$centered, onesim$LD,  pch = 20, col = pal[3])
  lines(onesim$centered, onesim$LD, col = pal[3])

	points(onesim$centered, onesim[,'f(heter)'],  pch = 20, col = pal[4])
  lines(onesim$centered, onesim[,'f(heter)'], col = pal[4])

  legend(legend_position, col = pal, pch =20, inset = 0.08,
		legend = c('mean fitness','mean HI', 'LD', 'f(heter)'))
}
