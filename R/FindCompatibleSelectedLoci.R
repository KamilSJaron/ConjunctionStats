#' @title FindCompatibleSelectedLoci
#'
#' @description
#' compute a compatible selected loci to input number loci (a value or a vector)
#'
#' @param loci a vector of loci
#'
#' @param force_list force list output [F] (see return)
#'
#' @return a vector (for one locus) or a list of vectors (for vector of loci) of compatible loci
#' each vector is sorted from the highest to lowest possible SL
#' ans since SL == L are always compatible, the first value of the vector corresponds to L.
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export

FindCompatibleSelectedLoci <- function(loci, force_list = F){
    sl_list <- list()
    for(locus in loci){
        sl <- c()
        for(tested_l in locus:2){
            if((locus - tested_l) %% (tested_l - 1) == 0){
                sl <- c(sl,tested_l)
            }
        }
        sl_list[[length(sl_list)+1]] <- sl
    }
    if(length(loci) == 1 & !force_list){
        return(sl)
    } else {
        return(sl_list)
    }
}
