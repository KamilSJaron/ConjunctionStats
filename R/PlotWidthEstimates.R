#' @title PlotWidthEstimates
#'
#' @description
#' \code{PlotWidthEstimates} plots a mean hybrid indices with logit transformation
#' and estimates of width using Endler, fit of logistic curve and HZAR
#'
#' @param GradLine a line from filled setting data.frame. Output of \code{FillSetting}.
#'
#' @param onesim corresponding data.frame from list of simulation summaries
#'
#' @param range defines how many demes around center should be plotted (NA for all)
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @importFrom RColorBrewer brewer.pal
#' @export

PlotWidthEstimates <- function(GradLine, onesim, range = NA){
    palette <- brewer.pal(5,"RdYlBu")
    if(is.na(range)){
        plot(logit(meanHI) ~ order, data = onesim, pch=20)
    } else {
        plot(logit(meanHI) ~ order,
                 data = onesim,
                 pch=20,
                 xlim = c(GradLine$center - range, GradLine$center + range))
    }

# HZAR width (approximated by logistic curve)
    x <- seq(0.01,max(onesim$order),by=0.01)
    y <- logit(sigmoid(c(1,4 / (GradLine$width),GradLine$center_H),x))
    lines(x,y, col = palette[1])

# logistic plot
    x <- seq(0.01,max(onesim$order),by=0.01)
    y <- logit(sigmoid(c(1,GradLine$slope,GradLine$center),x))
    lines(x,y, col = palette[2])

#   20-80 guess
    y = onesim$meanHI[onesim$meanHI > 0.2 & onesim$meanHI < 0.80]
    x = onesim$order[onesim$meanHI > 0.2 & onesim$meanHI < 0.80]
    if(length(x) > 1){
        mod8020 <- lm(y ~ x)
        x <- seq(min(x),max(x),by=0.01)
        y <- logit(predict(mod8020, list(x=seq(min(x),max(x),by=0.01))))
        lines(x,y, col = palette[5])
    } else {
        y = onesim$meanHI[onesim$meanHI > 0.15 & onesim$meanHI < 0.85]
        x = onesim$order[onesim$meanHI > 0.15 & onesim$meanHI < 0.85]
        if(length(x) > 1){
            mod8020 <- lm(y ~ x)
            x <- seq(min(x),max(x),by=0.01)
            y <- logit(predict(mod8020, list(x=seq(min(x),max(x),by=0.01))))
            lines(x,y, col = palette[5])
        }
    }
}
