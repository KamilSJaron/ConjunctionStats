#' @title ReadBlocks
#'
#' @description
#' \code{ReadBlocks} loads a block file in a nested lists
#' a list of demes, where deme is a list of individuals, where individual is a list of chromosomes
#' (simulation are diploid therefore this number is always off)
#' and finally chromosome is a vector of blocks starting by As
#' (ex. c(40) is a pure A chromosome of 40 loci
#'      c(0, 20) is a pure B chromosome of 20 loci
#'      c(0,5,1,4) is ch of three blocks of total 10 loci and it could be writen as BBBBBABBBB)
#'      c(5,1,4) is ch of three blocks of total 10 loci and it could be writen as AAAAABAAAA)
#'
#' @param filename a name of block output to be read
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export

ReadBlocks <- function(filename){
    all_block_data <- strsplit(readLines(filename)[-1], '\t')
    # read the vector of demes
    deme_indices <- as.numeric(unlist(lapply(all_block_data, function(x){ x[1] })))
    # a list of demes
    sim <- list()
    for(deme_index in unique(deme_indices)){
        deme_data <- all_block_data[deme_indices == deme_index]
        # a list of individuals
        deme <- list()
        for(ind_index in 1:length(deme_data)){
            ind <- deme_data[[ind_index]]
            # a list of chromosomes, cheomosome is a vector of blocks
            deme[[ind_index]] <- lapply(strsplit(ind[-1], ','), as.numeric)
        }
        sim[[deme_index+1]] <- deme
    }
    return(sim)
}
