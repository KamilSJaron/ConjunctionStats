#' @title FillSetting
#'
#' @description
#' \code{FillSetting} computes widths and centers of set of clines (simulations).
#' Both 1D and 2D simulations can be input
#'
#' @param sim A list of simulation summaries, output of \code{ReadSummary}.
#' Please refer to documentation of \code{ReadSummary} function for detailed usage.
#'
#' @param GradTable A data.frame of simulations, output of \code{ReadSetting}.
#' Please refer to documentation of \code{ReadSetting} function for detailed usage.
#'
#' @param method set to 'hzar' for fit performed by package hzar (default) or
#' 'nlm' for fit performed by fit of logistic curve to cline using non-linear least square method.
#' 'nlm' mehtod is faster, but does not perform well on clines deviating a lot from logistic shape
#' (multilocus clines, clines under strong drift ...)
#'
#' @param tails ('mirror') to fit logistic model with exponenctial functions as tails;
#' ('none') to fit logistic model only
#'
#' @return A data.frame object, integrating information about the settings used in the simulation run and the summary of the simulation run in a R-friendly format.
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#
#' @examples{
#'    mysim=ReadSummary(nameIn='../../Conjunction/out')
#'    myGradTable=ReadSetting(InputFile='../../Conjunction/setting.txt')
#'    myFilledSetting = FillSetting(sim=mysim, GradTable=myGradTable)
#' }
#'
#' @export

FillSetting <- function(sim, GradTable, method = 'hzar', tails = 'none'){

    if(method == 'hzar'){
        # (1st condition) 2D models contail indexes of demes UP and DOWN of every deme
        # (2nd condition) I want 2D model with only one deme forming torus (i.e. just 1D with smaller dirpersal to be treated as 1D)
        if("UP" %in% colnames(sim[[1]]) & any(sim[[1]]$UP != sim[[1]]$DOWN)){
            # 2D HZAR
            GradTable <- Fill2DSettingByHZAR(sim, GradTable, tails)
        } else {
            # 1D HZAR
            GradTable <- FillSettingByHZAR(sim, GradTable, tails)
        }
    }

    if(method == 'nlm'){
        if("UP" %in% colnames(sim[[1]])){
            # 2D NLM
            GradTable <- Fill2DSettingByNLM(sim, GradTable)
        } else {
            # 1D NLM
            GradTable <- FillSettingByNLM(sim, GradTable)
        }
    }

    return(GradTable)
}
