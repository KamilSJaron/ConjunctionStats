#' @title juncitons2vector
#'
#' @description
#' \code{juncitons2vector} converts a strings with blocks to vectors of ones one zeros (A and B loci)
#'
#' @param ch the string
#'
#' @return A vectors of loci
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}

juncitons2vector <- function(ch){
    rep(rep(c(0, 1), length = length(ch)), ch)
}
