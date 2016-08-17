#' @title plotBLockDistr
#'
#' @description
#' \code{plotBLockDistr} 
#'
#' @param blocks
#'
#' @param gradline
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export

# TODO: header

plotBLockDistr <- function(blocks, gradline){
  colour = colorRampPalette(brewer.pal(9,"Spectral"))(10000)[gradline$G]
  x <- c()
  y <- c()
  number_of_blocks <- 0
  for(k in (0:100)){
    number_of_blocks <- sum(blocks < ((7/8)^k) & blocks > ((7/8)^(k+1)))
    x <- c(x,(log((7/8)^k) + log((7/8)^(k+1))) / 2)
    y <- c(y,log(number_of_blocks / (((7/8)^k)-((7/8)^(k+1)))))
  }
  lines(x,y, col = colour)
  points(x,y, pch = 20, col = colour)  
  
  eqy <- log(BlocksDistribution(exp(x), gradline$s, gradline$lambda , gradline$M * (1 - gradline$s)))
  lines(x,eqy, col = colour)
}
