#' @title FitHZARmodel
#'
#' @description
#' \code{FitHZARmodel} takes hzar object and use some arbitrarly chosen parameters to fit the model
#'
#' @param AdaA an hzar object
#'
#' @param tails 'none' for logistic clines, 'mirror' for logistic with exponenctial tails
#'
#' @param BoxReq c(min, max) adds requirements to any and all of the parameters center, width, deltaM, deltaL, and deltaR.
#'
#' @param chainLength sets the length of mc for exploring a parameter space
#'
#' @param burnin is number of points that at explored in tyhe parametric space before the chain is initiated
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

FitHZARmodel <- function(AdaA, tails = "none", BoxReq = NA, chainLength = 5e3, burnin = 1e3){
  AdaAmodel <- hzar.makeCline1DFreq(AdaA, scaling="fixed",tails=tails);

  #‘hzar.model.addBoxReq’ adds requirements to any and all of the
  #   parameters center, width, deltaM.
  if(any(is.na(BoxReq)) | length(BoxReq) != 2){
    BoxReq <- c(0, nrow(AdaA$frame))
  }

  AdaAmodel <- hzar.model.addBoxReq(AdaAmodel, BoxReq[1] , BoxReq[2]);
  AdaAmodelFitR <- hzar.first.fitRequest.old.ML(model=AdaAmodel,
                                                   AdaA,
                                                   verbose=FALSE);
  out <- tryCatch(
      {
        AdaAmodelFitR$mcmcParam$chainLength <- chainLength;
        AdaAmodelFitR$mcmcParam$burnin <- burnin;
        AdaAmodelFit <- hzar.doFit(AdaAmodelFitR)
        AdaAmodelData <- hzar.dataGroup.add(AdaAmodelFit);
        return(AdaAmodelData)
      },
      error=function(cond) {
          AdaAmodelData <- hzar.dataGroup.add(AdaAmodelFitR);
          AdaAmodelData$ML.cline$param.free <- c(NA, NA)
          AdaAmodelData$ML.cline$logLike <- NA
          return(AdaAmodelData)
      }
  )
  return(out)
}
