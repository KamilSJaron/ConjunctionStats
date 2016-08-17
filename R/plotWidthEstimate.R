#' @title plotWidthEstimate
#'
#' @description
#' \code{plotWidthEstimate} 
#'
#' @param FileName
#'
#' @param GradTable
#'
#' @param sim
#'
#' @param i
#'
#' @param ran
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export

# TODO: header

plotWidthEstimate=function(FileName, GradTable, sim, i, ran = 5){
	pdf(FileName)
	plot(meanHI ~ order, data = sim[[i]],pch=20,xlim = c(GradTable[i,]$center - ran,GradTable[i,]$center + ran))
	
# MARS plot
	marsx <- sim[[i]]$order[c((sim[[i]]$meanHI != 0) & (sim[[i]]$meanHI != 1))]
	marsy <- logit(sim[[i]]$meanHI)[c((sim[[i]]$meanHI != 0) & (sim[[i]]$meanHI != 1))]
	marsmod <- earth(marsy ~ marsx)
# 	, degree = 1, nk = 5
	breakpoints <- unique(marsmod$cuts[-1])
	respond <- predict(marsmod,newdata=breakpoints)
	x <- seq(0.01,max(sim[[i]]$order),by=0.01)
	y <- inverse_logit(predict(marsmod,newdata=x))
	lines(x,y, col = 'red')
# 	points(breakpoints,respond, col='red', pch = 20)
	
# logistic plot
	GradTableLine <- GradTable[i,]
	binit <- 4 * (sqrt(8)*(sqrt(0.5) / sqrt(GradTableLine$s)))^-1
	y = sim[[i]]$meanHI
	x = sim[[i]]$order
	
	fitModel = nls(y ~ a / (1 + exp(-b * (x - c))), start = list(a = 1, b = binit, c = length(x) / 2))
	
	x <- seq(0.01,max(sim[[i]]$order),by=0.01)
	y <- sigmoid(summary(fitModel)$coefficients[,1],x)
	lines(x,y, col = 'blue')
	
# 	20-80 guess
	y = sim[[i]]$meanHI[sim[[i]]$meanHI > 0.2 & sim[[i]]$meanHI < 0.80]
	x = sim[[i]]$order[sim[[i]]$meanHI > 0.2 & sim[[i]]$meanHI < 0.80]
	if(length(x) > 1){
		mod8020 <- lm(y ~ x)
		abline(mod8020, col = 'green')
	} else {
		y = sim[[i]]$meanHI[sim[[i]]$meanHI > 0.15 & sim[[i]]$meanHI < 0.85]
		x = sim[[i]]$order[sim[[i]]$meanHI > 0.15 & sim[[i]]$meanHI < 0.85]
		if(length(x) > 1){
			mod8020 <- lm(y ~ x)
			abline(mod8020, col = 'green')
		}
	}
	dev.off()
}
