#' @title FillClosestS
#'
#' @description
#' \code{FillClosestS} is a function that finds effective selection (ss)
#' for each multilocus cline in the input data.frame.
#' ss is a selection acting on onelocus cline that is needed to maintaing cline of the same width.
#'
#' @param multilocus a multilocus data frame filled by \code{FillSettingByHZAR}
#'
#' @param onelocus a onelocus data frame filled by \code{FillSettingByHZAR}.
#' selections used for onelocus simulation should cover whole possible range (0.01 to 0.99)
#'
#' @return A data.frame object (multilocus) with new column ss;
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#
#' @examples{
#'    # load data
#'    multilocus_sim = ReadSummary('multilocus_sim.out')
#'    ml_GradTable = ReadSetting('multilocus_setting.txt')
#'    ml_GradTable = FillSettingByHZAR(multilocus_sim, ml_GradTable)
#'    onelocus_sim = ReadSummary('onelocus_sim.out')
#'    onelocus_GradTable = ReadSetting('multilocus_setting.txt')
#'    onelocus_GradTable = FillSettingByHZAR(onelocus_sim, onelocus_GradTable)
#'
#'    # compute ss
#'    myGradTable = FillClosestS(ml_GradTable, onelocus_GradTable)
#' }
#'
#' @export

FillClosestS <- function(multilocus, onelocus){
  multilocus$ss <- NA
  for(line in 1:nrow(multilocus)){
    ss <- onelocus$s[which.min(abs(onelocus$width_H - multilocus$width_H[line]))]
    multilocus$ss[line] <- ss
  }
  return(multilocus)
}
