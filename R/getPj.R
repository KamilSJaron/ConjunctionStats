#' @title getPj
#'
#' @description
#' \code{getPj} 
#'
#' @param namePattern
#'
#' @param save
#'
#' @return lim
#' 
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#
#' @examples{
#'   
#' }
#'
#' @export

# TODO: header
# comment: check Stuart's paper; for 0D

getPj <- function(namePattern,save = 1,lim = 9){
    nameIn <- c()
    Pjs <- c()
    for(i in 1:lim){
        if(i > 10){
            ord <- i - 1
        } else {
            ord <- paste(0,i-1,sep='')
        }
        if(save == 0){
            nameIn <- c(nameIn,paste(paste(namePattern,'s',ord,sep='_'),'dat',sep='.'))
        }else{
            nameIn <- c(nameIn,paste(paste(namePattern,'s',ord,save,sep='_'),'dat',sep='.'))
        }
    }
    for(Line in 1:lim){
        blocks <- read.csv(nameIn[Line],header = F)
        Pjs <- c(Pjs,sum(blocks$V1))
    }
    return(Pjs)
}
