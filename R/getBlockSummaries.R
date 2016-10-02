#' @title getBlockSummaries
#'
#' @description
#' \code{getBlockSummaries}
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

getBlockSummaries <- function(path = '.', pattern = '.dat'){
  AblocksList <- list(list())
  BblocksList <- list(list())
  nameIn <- dir(path, pattern = pattern)
  nameIn <- paste0(path,nameIn)
  for(file_index in 1:length(nameIn)){
    blockFile <- readLines(nameIn[file_index]) # read the file with blocks
    last_deme_index = unlist(strsplit(blockFile[1],'\t'))[1]; # read first deme index
    AblocksList[[file_index]] <- list()
    BblocksList[[file_index]] <- list()
    AblocksDeme <- c() # init / clear vectors for blocks
    BblocksDeme <- c()
    for(AlineIndex in seq(1, length(blockFile), by = 2)){ # go thought odd lines (A lines)
      Aline <- unlist(strsplit(blockFile[AlineIndex],'\t'))
      deme_index <- Aline[1]
      if(last_deme_index != deme_index){
        AblocksList[[file_index]][[last_deme_index]] <- strtoi(AblocksDeme)
        BblocksList[[file_index]][[last_deme_index]] <- strtoi(BblocksDeme)
        AblocksDeme <- c() # new deme, clear vectors
        BblocksDeme <- c()
      }
      AblocksDeme <- c(AblocksDeme,Aline[-1]) # add block sizes to vector without deme index (first value of each row)
      BblocksDeme <- c(BblocksDeme,unlist(strsplit(blockFile[AlineIndex+1],'\t'))[-1]) # read the B line (even line)
      last_deme_index <- deme_index;
    }
    AblocksList[[file_index]][[last_deme_index]] <- strtoi(AblocksDeme)
    BblocksList[[file_index]][[last_deme_index]] <- strtoi(BblocksDeme)
  }
  return(list('A' = AblocksList, 'B' = BblocksList));
}
