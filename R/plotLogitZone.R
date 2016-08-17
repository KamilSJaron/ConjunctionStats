#' @title plotLogitZone
#'
#' @description
#' \code{plotLogitZone} 
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export

# TODO: header

plotLogitZone=function(FileName, GradTable, sim, sel, ran = 0, type = 'pdf', leg = T){
	if(type == 'pdf'){
		pdf(FileName)
	} else {
		if(type == 'png'){
			png(FileName)
		} else {
			return("Error: unknown type (pdf or png)")
		}
	}
	picked <- which(GradTable$s == round(sel,2))
	x <- logit(sim[[picked[length(picked)]]]$meanHI[order(sim[[picked[length(picked)]]]$order)])
	y <- sim[[picked[length(picked)]]]$centered[order(sim[[picked[length(picked)]]]$order)]
	palette <- rev(brewer.pal(8,"RdYlBu"))[c(-3,-4)]
	if(ran == 0){
		plot(x ~ y,type='l',col=palette[length(picked)],xlab = 'centralized indices', ylab = 'logit(average hybrid index)',lwd = 2,cex.lab=1.5, cex.axis=1.5)
	} else {
		plot(x ~ y,type='l',col=palette[length(picked)],xlab = 'centralized indices', ylab = 'logit(average hybrid index)',xlim = c(-ran,ran), lwd = 2,cex.lab=1.5, cex.axis=1.5)
	}
	for(i in 1:(length(picked))){
		x <- logit(sim[[picked[i]]]$meanHI[order(sim[[picked[i]]]$order)])
		y <- sim[[picked[i]]]$centered[order(sim[[picked[i]]]$order)]
		lines(x ~ y,col = palette[i],lwd = 2)
	}
	if(leg == T){
		legend(min(y) + 1,10, legend = c(1,2,4,8,16), col = palette, lty = 1,lwd = 2, title = 'Loci',cex=1.5)
	}
	dev.off()
}
