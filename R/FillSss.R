#' @title FillSss
#'
#' @description
#' \code{FillClosestS} is a function that finds effective selection (ss)
#' for each multilocus cline in the input data.frame.
#' ss is a selection acting on onelocus cline that is needed to maintaing cline of the same width.
#'
#' @param multilocus a multilocus data frame filled by \code{FillClosestS}
#'
#' @return A data.frame object (multilocus) with new column ss;
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#
#' @examples{
#'    # compute ss
#'    myGradTable = FillClosestS(ml_GradTable, onelocus_GradTable)
#'    myGradTable = FillSss(myGradTable, 1)
#' }
#'
#' @export

FillSss <- function(GradTable, s_norm = 0.007, ss_norm = 0.005, sss_norm = 0.006){
  GradTable$sss <- GradTable$ss / GradTable$s
  GradTable$sss_norm <- GradTable$sss + rnorm(nrow(GradTable), 0, sss_norm)
  GradTable$ss_norm <- GradTable$ss + rnorm(nrow(GradTable), 0, ss_norm)
  GradTable$s_norm <- GradTable$s + rnorm(nrow(GradTable), 0, s_norm)
  return(GradTable)
}
