#' @title FillSettingByHZAR
#'
#' @description FillSettingByHZAR integrates the the output of \code{ReadSummary} and
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
#'
#' @import hzar
#' @export

FillSettingByHZAR <- function(sim, GradTable){
  GradTable$width <- NA
  GradTable$center <- NA

  for(i in 1:length(sim)){
    mknAdaA <- SummaryToHZAR(sim[[i]], GradTable[i,])
    mknAdaAmodelData <- FitHZARmodel(mknAdaA);
    # center
    GradTable$center[i] <- unlist(mknAdaAmodelData$ML.cline$param.free[1])
    # width
    GradTable$width[i] <- unlist(mknAdaAmodelData$ML.cline$param.free[2])
  }
  return(GradTable)
}
