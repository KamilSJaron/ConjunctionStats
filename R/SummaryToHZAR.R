#' @title SummaryToHZAR
#'
#' @description
#' \code{SummaryToHZAR} is a funciton converting list of summaries to HZAR objects
#'
#' @param onesim is a \code{data.frame}, member of list of summaries loaded by \code{ReadSummary}.
#' Package hzar has to be installed.
#'
#' @param gradline is a line of setting table corresponding to the simulation
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export

# TO DO: 2D clines
SummaryToHZAR <- function(onesim, gradline){
  library(hzar)

  hzartab <- hzar.doMolecularData1DPops(distance = onesim$order,
    pObs = onesim$meanHI,
    nEff = rep(gradline$D, nrow(onesim)))

  return(hzartab)
}
