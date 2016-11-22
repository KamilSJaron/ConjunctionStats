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
#' @param tails 'none' fit logistic model,
#' 'mirror' will fit a logistic function with exponenctial tails,
#' 'auto' will choose a model with greater likelihood
#'
#' @return An input data.frame with, log likelihood of fitted model, estimates of width, center (tails = 'none'), deltaM and tauM (tail = 'auto' | 'mirror').
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @import hzar
#' @export

FillSettingByHZAR <- function(sim, GradTable, tails = 'none'){
  GradTable$width <- NA
  GradTable$center <- NA
  GradTable$LogL <- NA
  if(tails == 'mirror' | tails == 'auto'){
    GradTable$deltaM <- NA
    GradTable$tauM <- NA
  }

  for(i in 1:length(sim)){
    AdaA <- SummaryToHZAR(sim[[i]], GradTable[i,])
# auto choses model by likelihood
    if(tails == 'auto'){
        logit_model <- FitHZARmodel(AdaA, tails = 'none');
        tail_model <- FitHZARmodel(AdaA, tails = 'mirror');
        # chose modelwith greater log likelihood
        if(tail_model$ML.cline$logLike > logit_model$ML.cline$logLike){
          AdaAmodelData <- tail_model
        } else {
          AdaAmodelData <- logit_model
          # if parameters deltaM and tauM were not fitted, report NA
          AdaAmodelData$ML.cline$param.free[3:4] <- NA
        }
    } else {
      AdaAmodelData <- FitHZARmodel(AdaA, tails);
    }

    # center
    GradTable$center[i] <- unlist(AdaAmodelData$ML.cline$param.free[1])
    # width
    GradTable$width[i] <- unlist(AdaAmodelData$ML.cline$param.free[2])
    if(tails == 'mirror' | tails == 'auto'){
      GradTable$deltaM[i] <- unlist(AdaAmodelData$ML.cline$param.free[3])
      GradTable$tauM[i] <- unlist(AdaAmodelData$ML.cline$param.free[4])
    }
    # log likelihood
    GradTable$LogL[i] <- AdaAmodelData$ML.cline$logLike
  }
  return(GradTable)
}
