plotWC=function(FileName, GradFileName, SimTabName){
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
				points(width_m ~ s, subset = L == CHs[i],pch = 19, cex = 0.6, data=GradTable, col = palette[i])
				lines(width_m ~ s, subset = L == CHs[i],data=GradTable, col = palette[i])
			} else {
				points(width_l ~ s, subset = C == CHs[i],pch = 17, cex = 0.6,data=GradTable, col = palette[i])
				lines(width_l ~ s, subset = C == CHs[i],data=GradTable, col = palette[i])
				points(width_e ~ s, subset = C == CHs[i],pch = 15, cex = 0.6,data=GradTable, col = palette[i])
				lines(width_e ~ s, subset = C == CHs[i],data=GradTable, col = palette[i])
				points(width_m ~ s, subset = C == CHs[i],pch = 19, cex = 0.6, data=GradTable, col = palette[i])
				lines(width_m ~ s, subset = C == CHs[i],data=GradTable, col = palette[i])
			}
		}
		if(min(GradTable$s) < 0.2){
			xleg <- 0.08
		}else{
			xleg <- 0
		}
		legend(0.585, ylimt,  c('Logistic','Endler','MARS'), pch = c(17,15,19),cex=1.5)
		legend(0.75 + xleg, ylimt, CHs, pch = 20,cex=1.5, col=palette)
	dev.off()
}

plotCF=function(FileName, GradTable, sim, i, ran = 5){
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
	# 	, degree = 1, nk = 5
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
