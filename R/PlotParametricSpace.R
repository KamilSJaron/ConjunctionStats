#' @title PlotParametricSpace
#'
#' @description
#' \code{PlotParametricSpace} plots range of selections, betas, lambdas and
#' number of chromosomes in one set of simulations
#'
#' @param GradTable
#'
#' @param global_margins set margins around plot
#'
#' @param denisity_haching set density of lines in rectangles
#'
#' @param cex is size of all letters in plot (2 or 3 are reasonable)
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export

PlotParametricSpace <- function(GradTable, global_margins = 0.1, # in inches
                                denisity_haching = 10, cex = 2){

  par(mfrow=c(4,1), tcl=-0.5, family="serif", las=1, omi=c(global_margins,
                                                           global_margins,
                                                           global_margins,
                                                           global_margins))
  # SELECTION (min, max, by)
  PlotParametricTemplate(c(0,1), 0.05, cex)
  rect(min(GradTable$s), 0, max(GradTable$s), 1, density = denisity_haching)
  mtext("s", side=2, line=0.5, cex = cex)

  # BETA (min, max, by)
  PlotParametricTemplate(c(-4, 4), NA, cex)
  beta_axis <- c(1/16, 1/4, 1, 4, 16)
  axis(1, labels=beta_axis, at=log2(beta_axis), lwd=0, lwd.ticks=1, cex.axis=cex)
  rect(log2(min(GradTable$b)), 0, log2(max(GradTable$s)), 1,
       density = denisity_haching)
  mtext(expression(beta), side=2, line=0.5, cex = cex)

  # LAMBDA (min, max, by)
  PlotParametricTemplate(c(0, 2), 0.2, cex)
  if(min(GradTable$r) == max(GradTable$r)){ext_width = 0.02} else {ext_width = 0}
  rect(min(GradTable$r) - ext_width, 0 + ext_width, max(GradTable$r), 1,
       density = denisity_haching)
  mtext(expression(lambda), side=2, line=0.5, cex = cex)

  # CHROMOSOMES (min, max, by)
  PlotParametricTemplate(c(0, 20), 5, cex)
  # -0.1, +0.1 is defined for situation of one simulated value only
  if(min(GradTable$C) == max(GradTable$C)){ext_width = 0.1} else {ext_width = 0}
  rect(min(GradTable$C) - ext_width, 0, max(GradTable$C) + ext_width, 1,
       density = denisity_haching)
  mtext("C", side=2, line=0.5, cex = cex)
}
