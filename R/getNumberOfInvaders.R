#' @title getNumberOfInvaders
#'
#' @description
#' \code{getNumberOfInvaders} 
#'
#' @param sim
#'
#' @param path
#'
#' @return patterm
#' 
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#
#' @examples{
#'   
#' }
#'
#' @export

# TODO: header, ask for .dat

getNumberOfInvaders <- function(sim, path = '.', pattern = '.dat'){
  #path = '../160525_1d_coupling/'
  #sim = sim_1M
  #pattern = '.dat'
  nameIn <- dir(path, pattern = pattern)
  nameIn <- paste0(path,nameIn)
  if(length(sim) != length(nameIn)){
    print('Not matching number of files with number')
    return(0)
  }
  invaders <- rep(0,length(nameIn))
  for(file_index in 1:length(nameIn)){
    blockFile <- readLines(nameIn[file_index]) # read the file with blocks
    blockFile <- strsplit(blockFile,'\t')
    simtab <- sim[[file_index]][order(sim[[file_index]]$order),] # take apropriate simulation table
    BinvaderDemes <- simtab$DEME[simtab$meanHI < 0.5 & simtab$meanHI != 0]
    AinvaderDemes <- simtab$DEME[simtab$meanHI > 0.5 & simtab$meanHI != 1]
    for(AlineIndex in seq(1, length(blockFile), by = 2)){ # go thought odd lines (A lines)
      Aline <- blockFile[[AlineIndex]]
      deme_index <- strtoi(Aline[1])
      if(deme_index %in% BinvaderDemes){
        Bline <- blockFile[[AlineIndex+1]]
        if(length(Bline) > 1){
          invaders[file_index] = invaders[file_index] + 1
        }
      }
      if(deme_index %in% AinvaderDemes & length(Aline) > 1){
        invaders[file_index] = invaders[file_index] + 1
      }
    }
  }
  return(invaders);
}
