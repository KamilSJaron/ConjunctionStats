ReadSummary=function(nameIn){
	summaryFile <- readLines(nameIn)
# 	index of red simulation
	i = 0
# 	sim is list of all simulations
	sim <- list()

	for(l in summaryFile){
		if("EDGE" %in% strsplit(l,split=' ')[[1]]){
# 			saving sequence only if it is not first occurance of key sequence EDGE
			if(i > 0){
				names(tableTemp) <- ColNames
				tableTemp$order <- 0
				tableTemp$order[which(!(tableTemp$LEFT %in% tableTemp$DEME))] <- 1
				nextRight <- tableTemp$RIGHT[tableTemp$order == 1]
				for(j in 2:nrow(tableTemp)){
					tableTemp$order[tableTemp$DEME == nextRight] <- j
					nextRight <- tableTemp$RIGHT[tableTemp$order == j]
				}
				sim[[i]] <- tableTemp
			}
# 			restart table for simulation and add 1 to index
			i = i + 1
			tableTemp <- data.frame()
		} else {
			newl <- strsplit(l, split = " ")[[1]][strsplit(l, split = " ")[[1]] != ""]
			if(newl[1] == 'DEME'){
				ColNames <- newl
				if(ColNames[4] == 'mean'){
					ColNames <- ColNames[-4]
					ColNames[4] <- 'meanf'
				}
				if(ColNames[6] == 'mean'){
					ColNames <- ColNames[-6]
					ColNames[6] <- 'meanf'
				}
			}
			newl <- strsplit(l, split = " ")[[1]][strsplit(l, split = " ")[[1]] != ""]
			if(grepl("[[:digit:]]",newl[1])){
				newl[1] <- gsub("[^0-9]", "", newl[1])
				tableTemp <- rbind(tableTemp,as.numeric(newl))
			}
		}
	}
	names(tableTemp) <- ColNames
	tableTemp$order <- 0
	tableTemp$order[which(!(tableTemp$LEFT %in% tableTemp$DEME))] <- 1
	nextRight <- tableTemp$RIGHT[tableTemp$order == 1]
	for(j in 2:nrow(tableTemp)){
		tableTemp$order[tableTemp$DEME == nextRight] <- j
		nextRight <- tableTemp$RIGHT[tableTemp$order == j]
	}	
	sim[[i]] <- tableTemp
	return(sim)
}

FillSummary <- function(sim,centers){
	for(i in 1:length(sim)){	
		B1max <- max(sim[[i]]$order)
		B0min <- 1
		
		if(any(sim[[i]]$meanHI == 1)){
			B1 <- min(sim[[i]]$order[sim[[i]]$meanHI == 1]):max(sim[[i]]$order[sim[[i]]$meanHI == 1])
			B1max <- max(c(B1[!(B1 %in% sim[[i]]$order[sim[[i]]$meanHI == 1])],min(sim[[i]]$order[sim[[i]]$meanHI == 1]) - 1))
		}
			
		if(any(sim[[i]]$meanHI == 0)){
			B0 <- min(sim[[i]]$order[sim[[i]]$meanHI == 0]):max(sim[[i]]$order[sim[[i]]$meanHI == 0])
			B0min <- min(c(B0[!(B0 %in% sim[[i]]$order[sim[[i]]$meanHI == 0])]),max(sim[[i]]$order[sim[[i]]$meanHI == 0]) + 1)
		}
		
		sim[[i]] <- sim[[i]][sim[[i]]$order %in% (B0min:B1max),]
		ordervec <- sim[[i]]$order - B0min + 1
		sim[[i]]$order = ordervec
		sim[[i]]$centered <- ordervec - centers[i]
	}
	return(sim)
}

Fill2DSummary <- function(sim,GradTable){
	lsize <- sim[[1]]$DEME[sim[[1]]$DOWN == 0] 

	for(i in 1:length(sim)){
		B1s <- seq(max(sim[[i]]$order),max(sim[[i]]$order),length = (lsize + 1))
		B0s <- seq(0,0,length = (lsize + 1))
	
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
		
		sim[[i]] <- sim[[i]][sim[[i]]$order %in% (minB0:maxB1),]
		ordervec <- sim[[i]]$order - minB0 + 1
		sim[[i]]$order = ordervec
	}
	
	return(sim)
}
