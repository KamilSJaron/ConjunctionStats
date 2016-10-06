#' @title plotWC
#'
#' @description
#' \code{plotWC}
#'
#' @param FileName
#'
#' @param GradTable
#'
#' @param SimTabName
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export

# TODO: header

plotWC <- function(FileName, GradFileName, SimTabName){
	library(RColorBrewer)
	sim <- ReadSummary(SimTabName)
	GradTable <- ReadSetting(GradFileName)
	GradTable <- FillSetting(sim,GradTable)
	sim <- FillSummary(sim,GradTable$center)
	palette <- brewer.pal(5,"RdYlBu")
	palette[3] <- brewer.pal(6,"RdYlBu")[3]
	pdf(paste(FileName,'.pdf',sep=''))
		x <-seq(0.01,0.99,by=0.01)
		CHs <- c(16,8,4,2,1)
		ylimt <- max(c(GradTable$width_l,GradTable$width_m,GradTable$width_e),na.rm = T)
		plot(x,twidth(x),type='l',xlim=c(min(GradTable$s),max(GradTable$s)),ylim=c(0,ylimt),xlab = 's',ylab = 'w',cex.lab=1.5, cex.axis=1.5)
		for(i in 1:5){
			if(all(GradTable$C == 1)){
				points(width_l ~ s, subset = L == CHs[i],pch = 17, cex = 0.6,data=GradTable, col = palette[i])
				lines(width_l ~ s, subset = L == CHs[i],data=GradTable, col = palette[i])
				points(width_e ~ s, subset = L == CHs[i],pch = 15, cex = 0.6,data=GradTable, col = palette[i])
				lines(width_e ~ s, subset = L == CHs[i],data=GradTable, col = palette[i])
			} else {
				points(width_l ~ s, subset = C == CHs[i],pch = 17, cex = 0.6,data=GradTable, col = palette[i])
				lines(width_l ~ s, subset = C == CHs[i],data=GradTable, col = palette[i])
				points(width_e ~ s, subset = C == CHs[i],pch = 15, cex = 0.6,data=GradTable, col = palette[i])
				lines(width_e ~ s, subset = C == CHs[i],data=GradTable, col = palette[i])
			}
		}
		if(min(GradTable$s) < 0.2){
			xleg <- 0.08
		} else {
			xleg <- 0
		}
		legend(0.585, ylimt,  c('Logistic','Endler'), pch = c(17,15),cex=1.5)
		legend(0.75 + xleg, ylimt, CHs, pch = 20,cex=1.5, col=palette)
	dev.off()
}
