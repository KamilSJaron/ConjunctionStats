#' @title PlotSim
#'
#' @description
#' \code{PlotSim} is a function for ploting an overview of one simulation
#'
#' @param onesim is a data frame of stats of one simulation
#'
#' @param GradTable A dataframe (usually output of \code{FillSetting}) or
#' \code{FillSettingByHZAR}
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
#' @importFrom RColorBrewer brewer.pal
#' @export

PlotSim <- function(onesim, Gradline,
                    legend_position = 'bottomright', add = F, pal = NA,
                    xlim = NA, ylim = NA, center = F){

  onesim$centered <- c(1:nrow(onesim))
  if(center){
    onesim$centered <- onesim$centered - Gradline$center
  }

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
  if (any(is.na(pal))) {
    pal <- brewer.pal(4, 'Set1')
  }

  # meanf f(heter)   meanHI  var(HI)   var(p)       LD
  vars <- c('meanf', 'meanHI', 'LD', 'f(heter)')
  for (i in 1:length(vars)) {
    points(onesim$centered, onesim[,vars[i]],  pch = 20, col = pal[i])
    lines(onesim$centered, onesim[,vars[i]], col = pal[i])
  }

  legend(legend_position, col = pal, pch = 20, inset = 0.08,
         legend = vars, bty = "n")
}
