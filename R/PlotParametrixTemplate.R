#' @title PlotParametricTemplate
#'
#' @description
#' \code{PlotParametricTemplate} plots an empty plot for parameter
#' plotting function \code{PlotParametricSpace}
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}

PlotParametricTemplate <- function(xlim, x_by, cex = 2){
  # bottom, left, top, and right; title, x, y
  par(mar=c(4,2 + cex / 2,0,0), mgp = c(0, -0.5 + cex / 1.2, 0))
  plot(numeric(0), ann=F, xlim=xlim, ylim=c(0,1),
       yaxt="n", bty="n", xaxt="n", cex.lab=cex, las=1)
  axis(1, at=xlim, labels=c("",""), lwd.ticks=0)
  if(!is.na(x_by)){
    axis(1, at=seq(xlim[1] , xlim[2], by=x_by), lwd=0, lwd.ticks=1, cex.axis=cex)
  }
}
