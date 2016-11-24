#' @title twidth
#'
#' @description
#' \code{twidth} given selection and dispersal function returns
#' diffusion approximation of width of one locus cline
#'
#' @param s selection
#'
#' @param sigma dispersal
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export
# comment: theoretical width. sigma^2=dispersal; s=selection

twidth <- function(s, sigma = sqrt(0.5)){
    fact = sqrt(8)
    return((fact * sigma) / sqrt(s))
}
