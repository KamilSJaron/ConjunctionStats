ReadSetting=function(InputFile = './setting.txt'){
	gradientTable <- data.frame(run = 1)
	# 'C' = numeric(0),'L' = numeric(0),'r' = numeric(0),'s' = numeric(0),'b' = numeric(0)
	summaryFile <- readLines(InputFile)

	for(l in summaryFile){
		l <- strsplit(l,split='#')[[1]][1]
		l <- strsplit(l,split='=')[[1]]
		if(grepl('RECOMBINATIONrate',l[1])){
			if(grepl("\\[",l[2])){
				gradientTable <- merge(gradientTable,data.frame('r' = as.double(strsplit(strsplit(strsplit(l[2],split=c('\\['))[[1]][2],split='\\]')[[1]][1],split=',')[[1]])))
			} else {
				gradientTable <- merge(data.frame('r' = as.numeric(l[2])),gradientTable)
			}
		}
		if(grepl('SELECTION',l[1])){
			if(grepl("\\[",l[2])){
				gradientTable <- merge(gradientTable,data.frame('s' = as.double(strsplit(strsplit(strsplit(l[2],split=c('\\['))[[1]][2],split='\\]')[[1]][1],split=',')[[1]])))
			} else {
				gradientTable <- merge(data.frame('s' = as.numeric(l[2])),gradientTable)
			}
		}
		if(grepl('CHROM',l[1])){
			if(grepl("\\[",l[2])){
				gradientTable <- merge(gradientTable,data.frame('C' = as.double(strsplit(strsplit(strsplit(l[2],split=c('\\['))[[1]][2],split='\\]')[[1]][1],split=',')[[1]])))
			} else {
				gradientTable <- merge(data.frame('C' = as.numeric(l[2])),gradientTable)
			}
		}
		if(grepl('LOCI',l[1])){
			if(grepl("\\[",l[2])){
				gradientTable <- merge(gradientTable,data.frame('L' = as.double(strsplit(strsplit(strsplit(l[2],split=c('\\['))[[1]][2],split='\\]')[[1]][1],split=',')[[1]])))
			} else {
				gradientTable <- merge(data.frame('L' = as.numeric(l[2])),gradientTable)
			}
		}
		if(grepl('BETA',l[1])){
			if(grepl("\\[",l[2])){
				gradientTable <- merge(gradientTable,data.frame('b' = as.double(strsplit(strsplit(strsplit(l[2],split=c('\\['))[[1]][2],split='\\]')[[1]][1],split=',')[[1]])))
			} else {
				gradientTable <- merge(data.frame('b' = as.numeric(l[2])),gradientTable)
			}
		}
		if(grepl('NUMBERofSAVES',l[1])){
			if(as.numeric(l[2]) > 1){
				gradientTable <- merge(data.frame('Saves' = as.numeric(l[2])),gradientTable)
			}
		}
		if(grepl('DEMEsize',l[1])){
			if(grepl("\\[",l[2])){
				gradientTable <- merge(gradientTable,data.frame('D' = as.double(strsplit(strsplit(strsplit(l[2],split=c('\\['))[[1]][2],split='\\]')[[1]][1],split=',')[[1]])))
			} else {
				gradientTable <- merge(data.frame('D' = as.numeric(l[2])),gradientTable)
			}
		}
	# if("EDGE" %in% strsplit(l,split=' ')[[1]]){}
	}
	gradientTable$run <- c()
	
# normalization 1. delete useless demes.
	
	
	
	return(gradientTable)
}


FillSetting <- function(sim,GradTable){
	GradTable$slope <- 0
	GradTable$center <- 0
	GradTable$ss <- 0
	GradTable$total_demes <- 0
	GradTable$width_l <- 0
	GradTable$width_m <- 0
	GradTable$width_e <- 0
# 	GradTable$width_m2 <- 0
	
	for(i in 1:length(sim)){
		GradTable$total_demes[i] <- nrow(sim[[i]])
		
		if(any(sim[[i]]$meanHI == 1)){
			B1 <- min(sim[[i]]$order[sim[[i]]$meanHI == 1]):max(sim[[i]]$order[sim[[i]]$meanHI == 1])
			maxB1 <- max(c(B1[!(B1 %in% sim[[i]]$order[sim[[i]]$meanHI == 1])],min(sim[[i]]$order[sim[[i]]$meanHI == 1]) - 1))
		} else {
			maxB1 <- max(sim[[i]]$order)
		}
		
		if(any(sim[[i]]$meanHI == 0)){
			B0 <- min(sim[[i]]$order[sim[[i]]$meanHI == 0]):max(sim[[i]]$order[sim[[i]]$meanHI == 0])
			minB0 <- min(c(B0[!(B0 %in% sim[[i]]$order[sim[[i]]$meanHI == 0])]),max(sim[[i]]$order[sim[[i]]$meanHI == 0]) + 1)
		} else {
			minB0 <- 1
		}
		
		sim[[i]] <- sim[[i]][sim[[i]]$order %in% (minB0:maxB1),]
		ordervec <- sim[[i]]$order - minB0 + 1
		sim[[i]]$order = ordervec
		
		GradTable$slope[i] <- getSlope(sim[[i]],GradTable[i,])
		GradTable$center[i] <- getCenter(sim[[i]],GradTable[i,])
		meanf <- sim[[i]]$meanf[order(sim[[i]]$order)]
		GradTable$ss[i] <- getSstar(meanf, 0.05, 1)
		GradTable$width_m[i] <- getWidthM(sim[[i]])
		GradTable$width_e[i] <- getWidthE(sim[[i]])
# 		GradTable$width_m2[i] <- getWidthM2(sim[[i]])
# 		GradTable$width_h[i] <- getWidthH(sim[[i]],GradTable[i,])
	}
	GradTable$width_l <- 4 * (GradTable$slope^-1)
	return(GradTable)
}

Fill2DSetting <- function(sim,GradTable){
	lsize <- sim[[1]]$DEME[sim[[1]]$DOWN == 0] 
	
	for(i in 0:lsize){
		GradTable[[paste("width_",i,sep='')]] <- 0
		GradTable[[paste("center_",i,sep='')]] <- 0
	}
	GradTable$total_demes <- 0
	
	for(i in 1:length(sim)){
		B1s <- seq(max(sim[[i]]$order),max(sim[[i]]$order),length = (lsize + 1))
		B0s <- seq(0,0,length = (lsize + 1))
		GradTable$total_demes[i] <- nrow(sim[[i]])
		
		for(h in 0:lsize){
			selected <- seq(h,max(sim[[i]]$DEME),by = lsize+1)
			subtable <- sim[[i]][sim[[i]]$DEME %in% selected,]
			
			if(any(subtable$meanHI == 1)){
				B1 <- min(subtable$order[subtable$meanHI == 1]):max(subtable$order[subtable$meanHI == 1])
				B1s[h+1] <- max(c(B1[!(B1 %in% subtable$order[subtable$meanHI == 1])],min(subtable$order[subtable$meanHI == 1]) - 1))
			}
			
			if(any(subtable$meanHI == 0)){
				B0 <- min(subtable$order[subtable$meanHI == 0]):max(subtable$order[subtable$meanHI == 0])
				B0s[h+1] <- min(c(B0[!(B0 %in% subtable$order[subtable$meanHI == 0])]),max(subtable$order[subtable$meanHI == 0]) + 1)
			}
		}
		
		minB0 <- min(B0s)
		maxB1 <- max(B1s) 
		
		for(h in 0:lsize){
			selected <- seq(h,max(sim[[i]]$DEME),by = lsize+1)
			subtable <- sim[[i]][sim[[i]]$DEME %in% selected,]
			
			subtable <- subtable[subtable$order %in% (minB0:maxB1),]
			ordervec <- subtable$order - minB0 + 1
			subtable$order = ordervec
			
			GradTable[[paste("width_",h,sep='')]][i] <- 4 * (getSlope(subtable,GradTable[i,])^-1)
			GradTable[[paste("center_",h,sep='')]][i] <- getCenter(subtable,GradTable[i,])
		}
		
	}
	return(GradTable)
}
