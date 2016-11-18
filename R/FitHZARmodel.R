#' @title FitHZARmodel
#'
#' @description
#' \code{FitHZARmodel} takes hzar object and use some arbitrarly chosen parameters to fit the model
#'
#' @param AdaA an hzar object
#'
#' @param tails 'none' for single locus clines
#'
#' @return A fitted hzar model
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#
#' @examples{
#'    AdaA <- SummaryToHZAR(sim[[1]], GradTable[1,])
#'    AdaAmodelData <- FitHZARmodel(AdaA);
#' }
#'
#' @import hzar
#' @export

FitHZARmodel <- function(AdaA, tails = "mirror"){
  # note there is no 2D model in HZAR

  AdaAmodel <- hzar.makeCline1DFreq(AdaA, scaling="fixed",tails=tails);

  #‘hzar.model.addBoxReq’ adds requirements to any and all of the
  #   parameters center, width, deltaM, deltaL, and deltaR.
  AdaAmodel <- hzar.model.addBoxReq(AdaAmodel, 1 , nrow(AdaA$frame) / 4);
  AdaAmodel <- hzar.model.addMaxWidth(AdaAmodel, nrow(AdaA$frame) / 2);
  AdaAmodelFitR <- hzar.first.fitRequest.old.ML(model=AdaAmodel ,
                                                   AdaA,
                                                   verbose=0);
  AdaAmodelFitR$mcmcParam$chainLength <- 2e3;
  AdaAmodelFitR$mcmcParam$burnin <- 5e2;
  AdaAmodelFit <- hzar.doFit(AdaAmodelFitR)
  AdaAmodelData <- hzar.dataGroup.add(AdaAmodelFit);
  return(AdaAmodelData)
}
