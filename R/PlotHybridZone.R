#' @title PlotHybridZone
#'
#' @description
#' \code{PlotHybridZone}
#'
#' @param sim A list (usually output of \code{ReadSummary})
#'
#' @param center set to 'start' to center by a start of the simulation (deme 0),
#'        set to NA not to center at all, set to corresponding data.frame with
#'        simulations (usually output of \code{FillSetting}) to center by computed
#'        center in
#'
#' @param logit = T if average hybrid should be plot in logit transfomation
#'
#' @param add = F, if lines should be added to current plot, or
#'
#' @param xlim is equvalent of parameter xlim from plot
#'
#' @param ylim NA means c(-10,10) for logit and c(0,1) for non-logit clines
#'
#' @param ... is passed to function \code{lines} (for instance col for change of colour)
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @importFrom RColorBrewer brewer.pal
#' @export

PlotHybridZone <- function(sim, center = 'start', logit = T, add = F, xlim = NA, ylim = NA, ...){

  if(any(is.na(xlim))){
    maxdemes <- max(unlist(lapply(sim, nrow)))
    if(any(is.na(center))){
      xlim <- c(0, maxdemes)
    } else {
      xlim <- c(-maxdemes / 2, maxdemes / 2)
    }
  }

  if(any(is.na(ylim))){
    if(logit){
      ylim = c(-10,10)
    } else {
      ylim = c(0,1)
    }
  }
  # ploting frame
  if(add == F){
    plot(numeric(0),
    xlim = xlim,
    ylim = ylim,
    xlab = '', ylab = '')
  }

  for(index in 1:length(sim)){
    onesim <- sim[[index]]

    if(all(is.null(colnames(center)))){
      if(center == 'start'){
        x <- onesim$order - onesim$order[onesim$DEME == 0]
      } else {
        x <- onesim$order
      }
    } else {
      x <- onesim$order - center$center[index]
    }

    if(logit){
      y <- logit(onesim$meanHI)
    } else {
      y <- onesim$meanHI
    }

    lines(x, y, ...)
  }
}
