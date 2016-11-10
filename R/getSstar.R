#' @title getSstar
#'
#' @description
#' \code{getSstar} is a function called inside\code{FillSetting}.
#'
#' @param meanf
#'
#' @param c
#'
#' @param depth
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export

# TODO: header
# comment: check s* in Kamil's thesis, deth= number of iterations, c=?

getSstar <- function(meanf,c,depth) {
    meanf <- 1 - meanf
    x <- c(1:length(meanf))
    a = max(meanf)
    b = mean(x)
    c = c
    out <- tryCatch(
        {
            return(coefficients(
                nls(meanf ~ a * exp(-((x - b)^2 / (2 * c^2)))
                , start=list(a=a,b=b,c=c)))['a']
                )
        },
        error=function(cond) {
            if(depth > 256){
                return(NA)
            } else {
                return(getSstar(meanf,c + 0.05,depth+1))
            }
        }
    )
    return(out)
}
