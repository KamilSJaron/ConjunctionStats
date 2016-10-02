#' @title plotCF
#'
#' @description
#' \code{plotCF} 
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

plotCF <- function(FileName, GradTable, sim, i, ran = 5){
	pdf(FileName)
		palette <- brewer.pal(5,"RdYlBu")
		par(mfrow=c(1,1))
		if(ran == 0){
			plot(logit(meanHI) ~ order, data = sim[[i]],pch=20)
		} else {
			plot(logit(meanHI) ~ order, data = sim[[i]],pch=20,xlim = c(GradTable[i,]$center - ran,GradTable[i,]$center + ran))
		}
	# MARS plot
		marsx <- sim[[i]]$order[c((sim[[i]]$meanHI != 0) & (sim[[i]]$meanHI != 1))]
		marsy <- logit(sim[[i]]$meanHI)[c((sim[[i]]$meanHI != 0) & (sim[[i]]$meanHI != 1))]
		marsmod <- earth(marsy ~ marsx)
		breakpoints <- unique(marsmod$cuts[-1])
		respond <- predict(marsmod,newdata=breakpoints)
		x <- seq(0.01,max(sim[[i]]$order),by=0.01)
		y <- predict(marsmod,newdata=x)
		lines(x,y, col = palette[1])
		points(breakpoints,respond, col=palette[1], pch = 20)
	# logistic plot
		GradTableLine <- GradTable[i,]
		binit <- 4 * (sqrt(8)*(sqrt(0.5) / sqrt(GradTableLine$s)))^-1
		y = sim[[i]]$meanHI
		x = sim[[i]]$order
		fitModel = nls(y ~ a / (1 + exp(-b * (x - c))), start = list(a = 1, b = binit, c = length(x) / 2))
		x <- seq(0.01,max(sim[[i]]$order),by=0.01)
		y <- logit(sigmoid(summary(fitModel)$coefficients[,1],x))
		lines(x,y, col = palette[2])
	# 	20-80 guess
		y = sim[[i]]$meanHI[sim[[i]]$meanHI > 0.2 & sim[[i]]$meanHI < 0.80]
		x = sim[[i]]$order[sim[[i]]$meanHI > 0.2 & sim[[i]]$meanHI < 0.80]
		if(length(x) > 1){
			mod8020 <- lm(y ~ x)
			x <- seq(min(x),max(x),by=0.01)
			y <- logit(predict(mod8020, list(x=seq(min(x),max(x),by=0.01))))
			lines(x,y, col = palette[5])
		} else {
			y = sim[[i]]$meanHI[sim[[i]]$meanHI > 0.15 & sim[[i]]$meanHI < 0.85]
			x = sim[[i]]$order[sim[[i]]$meanHI > 0.15 & sim[[i]]$meanHI < 0.85]
			if(length(x) > 1){
				mod8020 <- lm(y ~ x)
				x <- seq(min(x),max(x),by=0.01)
				y <- logit(predict(mod8020, list(x=seq(min(x),max(x),by=0.01))))
				lines(x,y, col = palette[5])
			}
		}
	dev.off()
}
