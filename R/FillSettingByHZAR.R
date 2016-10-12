#' @title FillSettingByHZAR
#'
#' @description
#' \code{FillSettingByHZAR} integrates the the output of \code{ReadSummary} and
#' \code{ReadSetting} functions into a data.frame object using package hzar
#'
#' @param sim A string of characters indicating a ReadSummary object.
#' Please refer to documentation of \code{ReadSummary} function for detailed usage.
#'
#' @param GradTable A string of characters indicating a ReadSetting object.
#' Please refer to documentation of \code{ReadSetting} function for detailed usage.
#'
#' @return A data.frame object with setting and estimates of width and centery by hzar
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#
#' @examples{
#'    mysim <- ReadSummary(nameIn='../../Conjunction/out')
#'    myGradTable <- ReadSetting(InputFile='../../Conjunction/setting.txt')
#'    myGradTable <- FillSetting(sim=mysim, GradTable=myGradTable)
#'    myGradTable <- FillSettingByHZAR(sim=mysim, GradTable=myGradTable)
#' }
#'
#' @export

FillSettingByHZAR <- function(sim, GradTable){
  GradTable$width_H <- NA
  GradTable$center_H <- NA

  for(i in 1:length(sim)){
    mknAdaA <- SummaryToHZAR(sim[[i]], GradTable[i,])
    #hzar.plot.obsData(mknAdaA);
    mknAdaAmodel <- hzar.makeCline1DFreq(mknAdaA, scaling="fixed",tails="none");
    mknAdaAmodel <- hzar.model.addBoxReq(mknAdaAmodel, 1 , nrow(sim[[i]]));
    mknAdaAmodelFitR <- hzar.first.fitRequest.old.ML(model=mknAdaAmodel ,
                                         mknAdaA,
                                         verbose=FALSE);
    mknAdaAmodelFitR$mcmcParam$chainLength <- 2e3;
    mknAdaAmodelFitR$mcmcParam$burnin <- 5e2;
    mknAdaAmodelFit <- hzar.doFit(mknAdaAmodelFitR)
    # plot(hzar.mcmc.bindLL(mknAdaAmodelFit))
    mknAdaAmodelData <- hzar.dataGroup.add(mknAdaAmodelFit);
    # center, maybe unlist shoud go in the end
    GradTable$center_H[i] <- unlist(mknAdaAmodelData$ML.cline$param.free[1])
    # width
    GradTable$width_H[i] <- unlist(mknAdaAmodelData$ML.cline$param.free[2])
  }
  return(GradTable)
}
