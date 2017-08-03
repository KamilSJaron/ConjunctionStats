#' @title plotBLockDistr
#'
#' @description
#' \code{plotBLockDistr}
#'
#' @param blocks is a nested list of blocks loaded using \code{ReadBlocks}
#'
#' @param onesim is a data frame of stats of one simulation (element of list loaded by \code{ReadSummary})
#'
#' @param gradline a line of corresponding simulation setting (line of data.frame loaded by \code{ReadSetting})
#'
#' @param horizontal [F] plot space on horizontal axis and chromosome on vertical axis
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export


PlotBlocks <- function(blocks, onesim, gradline, horizontal = F){
    # blocks -> matrix
    m <- matrix(0, nrow = length(blocks) * length(blocks[[1]]), ncol = sum(blocks[[1]][[1]][[1]]))
    line_index = 1
    for(deme_index in c(onesim$DEME + 1)){
        deme <- blocks[[deme_index]]
        for(individual_index in 1:length(deme)){
            ind <- deme[[individual_index]]
            for(chromosome_index in seq(1,length(ind), by = 2)){
                s1 <- ind[[chromosome_index]]
                s2 <- ind[[chromosome_index + 1]]
                m_line <- rowMeans(cbind(juncitons2vector(s1), juncitons2vector(s2)))
            }
            m[line_index,] <- m_line
            line_index = line_index + 1
        }
    }

    # plot the matrix
    if( horizontal ){
        loci <- 1
        individials <- 2
        image(t(m), axes=F)
    } else {
        loci <- 2
        individials <- 1
        image(m, axes=F)
    }

    loci_w = 1 / (gradline$L * 2)
    sel_loci <- seq(0, 1, length = gradline$SL)

    sel_loci_borders <- rep(sel_loci, each = 2) - rep(c(-loci_w, loci_w), gradline$SL)
    deme_borders <- seq(0, 1, length = nrow(onesim) + 1)


    axis(loci, at = sel_loci, labels = paste('SL', 1:gradline$SL))
    axis(individials, at=seq(0,1, length = nrow(onesim)), labels=onesim$DEME)

    if( horizontal ){
        title("Introgression plot",
              xlab='Chromosome (selected loci are highlited)',
              ylab='individuals sorted by demes [deme index]')
        for(i in sel_loci_borders){
            lines(c(i, i), c(0,1))
        }

        for(i in deme_borders){
            lines(c(0 - loci_w, 1 + loci_w), lwd = 0.5, c(i, i))
        }
    } else {
        title("Introgression plot",
              xlab='individuals sorted by demes [deme index]',
              ylab='Chromosome (selected loci are highlited)')
        for(i in sel_loci_borders){
            lines(c(0,1), c(i, i))
        }

        for(i in deme_borders){
            lines(c(i, i), c(0 - loci_w, 1 + loci_w), lwd = 0.5)
        }
    }

    # legend? image.plot(sample, legend.only=T)
}
