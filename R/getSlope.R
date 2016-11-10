#' @title getSlope
#'
#' @description
#' \code{getSlope} is a function called inside\code{FillSetting} and \code{Fill2DSetting}
#'
#' @param simtab
#'
#' @param GradTableLine
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export

# TODO: header

getSlope <- function(simtab,GradTableLine){
    out <- tryCatch(
        {
          if(GradTableLine$s == 0){
            binit <- (2 +GradTableLine$total_demes) / (GradTableLine$total_demes^1.2)
          } else {
            binit <- 4 * (sqrt(8)*(sqrt(0.5) / sqrt(GradTableLine$s)))^-1
          }
            y = simtab$meanHI
            x = simtab$order
            fitModel = nls(y ~ a / (1 + exp(-b * (x - c))),
            start = list(a = 1, b = binit, c = length(x) / 2))
            return(coef(fitModel)[2])
        },
        error=function(cond) {
            return(NA)
        }
    )
    return(out)
}
