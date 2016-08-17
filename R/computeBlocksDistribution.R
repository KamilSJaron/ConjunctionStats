#' @title computeBlocksDistribution
#'
#' @description
#' \code{computeBlocksDistribution}
#'
#' @param x
#'
#' @param blocks
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export

# TODO: header

computeBlocksDistribution <- function(x,blocks){
  y <- c()
  number_of_blocks <- 0
  for(k in (0:100)){
    number_of_blocks <- sum(blocks < ((7/8)^k) & blocks > ((7/8)^(k+1)))
    y <- c(y,log(number_of_blocks / (((7/8)^k)-((7/8)^(k+1)))))
  }
  return(y)
}
