#' @title FitHZARmodel
#'
#' @description
#' \code{FitHZARmodel} takes hzar object and use some arbitrarly chosen parameters to fit the model
#'
#' @param mknAdaA an hzar object
#'
#' @return A fitted hzar model
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#
#' @examples{
#'    mknAdaA <- SummaryToHZAR(sim[[1]], GradTable[1,])
#'    mknAdaAmodelData <- FitHZARmodel(mknAdaA);
#' }
#'
#' @import hzar
#' @export

FitHZARmodel <- function(mknAdaA){
  mknAdaAmodel <- hzar.makeCline1DFreq(mknAdaA, scaling="fixed",tails="none");
  mknAdaAmodel <- hzar.model.addBoxReq(mknAdaAmodel, 2 , nrow(mknAdaA$frame) / 2);
  mknAdaAmodelFitR <- hzar.first.fitRequest.old.ML(model=mknAdaAmodel ,
                                                   mknAdaA,
                                                   verbose=FALSE);
  mknAdaAmodelFitR$mcmcParam$chainLength <- 2e3;
  mknAdaAmodelFitR$mcmcParam$burnin <- 5e2;
  mknAdaAmodelFit <- hzar.doFit(mknAdaAmodelFitR)
  mknAdaAmodelData <- hzar.dataGroup.add(mknAdaAmodelFit);
  return(mknAdaAmodelData)
}
