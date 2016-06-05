PartSums=function(vector){
	out <- c(vector[1])
	for(i in 2:length(vector)){
		out <- c(out,(out[i-1] + vector[i]))
	}
	return(out)
}

logit=function(vector){
	return(log(vector / (1 - vector)))
}

inverse_logit=function(vector){
	return(exp(vector) / (1 + exp(vector)))
}

sigmoid = function(params, x) {
	params[1] / (1 + exp(-params[2] * (x - params[3])))
}

twidth = function(s, sigma = sqrt(0.5), fact = sqrt(8)) {
	return((fact * sigma) / sqrt(s))
}

slope2w = function(slope){
	return(4 / slope)
}

ss2w = function(ss,sigma = sqrt(0.5)){
	return((2 * sigma) / sqrt(ss))
}

EquilG <- function(theta){
	return(1 - (1 / theta))
}

getPopulationSize = function(LogFile,generation){
	Js <- c()
	counter = 0
	for(l in LogFile){
		counter = counter + 1 
		if("Selection" %in% (strsplit(l,split=':')[[1]])){
			counter <- 0
		}
		if(counter == generation){
			if(generation == 0){
				Js <- c(Js,as.numeric(strsplit(ll,split=' ')[[1]][3]))
			} else {
				Js <- c(Js,as.numeric(strsplit(l,split=' ')[[1]][3]))
			}
		}
		ll = l
	}
	return(Js)
}

getPj = function(namePattern,save = 1,lim = 9){
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

getBlockNumber = function(namePattern,save = 1,lim = 9){
	nameIn <- c()
	BlockNumber <- c()
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
		BlockNumber <- c(BlockNumber,length(blocks$V1))
	}
	return(BlockNumber)
}

getSstar <- function(meanf,c,depth) {
	meanf <- 1 - meanf
	x <- c(1:length(meanf))
	a = max(meanf)
	b = mean(x)
	c = c
	out <- tryCatch(
		{
			return(coefficients(
				nls(meanf ~ a * exp(-((x - b)^2 / (2 * c^2)))
				, start=list(a=a,b=b,c=c)))['a']
				)
		},
		error=function(cond) {
			if(depth > 256){
				return(NA)
			} else {
				return(getSstar(meanf,c + 0.05,depth+1))
			}
		}
	)    
	return(out)
}

getSlope=function(simtab,GradTableLine){
	out <- tryCatch(
		{
		  if(GradTableLine$s == 0){
		    binit <- (2 +GradTableLine$total_demes) / (GradTableLine$total_demes^1.2)
		  } else {
		    binit <- 4 * (sqrt(8)*(sqrt(0.5) / sqrt(GradTableLine$s)))^-1
		  }
			y = simtab$meanHI
			x = simtab$order
			
			fitModel = nls(y ~ a / (1 + exp(-b * (x - c))), 
			start = list(a = 1, b = binit, c = length(x) / 2))
			return(coef(fitModel)[2])
		},
		error=function(cond) {
			return(NA)
		}
	)    
	return(out)
}

getWidthM=function(simtab){
	out <- tryCatch(
		{
			relevant <- which(!(simtab$meanHI == 0 | simtab$meanHI == 1))
			marsmod <- earth(logit(meanHI) ~ order, 
				data = simtab, degree = 1, nk = 5, subset = relevant)
			bps <- unique(marsmod$cuts[-1])
			bps <- c(bps[1] - 1, unique(bps), bps[length(bps)] + 1)
			respond <- predict(marsmod,newdata=bps)
			slopes <- diff(respond) / diff(bps)
			return(4 * max(slopes)^(-1))
		},
		error=function(cond) {
				return(NA)
		}
	)    
	return(out)
}

getCenter=function(simtab,GradTableLine){
	out <- tryCatch(
		{
			binit <- 4 * (sqrt(8)*(sqrt(0.5) / sqrt(GradTableLine$s)))^-1
			y = simtab$meanHI
			x = simtab$order
			
			fitModel = nls(y ~ a / (1 + exp(-b * (x - c))), 
			start = list(a = 1, b = binit, c = length(x) / 2))
			return(coef(fitModel)[3])
		},
		error=function(cond) {
			return(NA)
		}
	)    
	return(out)
}


BlocksDistribution <- function(x,S,R,M){
	theta = S / R
	y = ((M * theta) / (S * (1 + theta))) * 
		((2 * x^(-1 * ((3+theta)/(1+theta)))) / (1 + theta))
	return(y)
}

getWidthE=function(simtab){
	y = logit(simtab$meanHI[simtab$meanHI > 0.2 & simtab$meanHI < 0.80])
	x = simtab$order[simtab$meanHI > 0.2 & simtab$meanHI < 0.80]
	if(length(x) > 1){
		mod8020 <- lm(y ~ x)
		return(4 / mod8020$coefficients[2])
	} else {
		y = logit(simtab$meanHI[simtab$meanHI > 0.15 & simtab$meanHI < 0.85])
		x = simtab$order[simtab$meanHI > 0.15 & simtab$meanHI < 0.85]
		if(length(x) > 1){
			mod8020 <- lm(y ~ x)
			return(4 / mod8020$coefficients[2])
		} else {
			return(NA)
		}
	}
}

# getWidthM2=function(simtab){
# 	out <- tryCatch(
# 		{
# 			relevant <- which(!(simtab$meanHI == 0|simtab$meanHI == 1))
# 			marsmod <- earth(logit(meanHI) ~ order, data = simtab, subset = relevant)
# 			bps <- unique(marsmod$cuts[-1])
# 			bps <- c(bps[1] - 1, unique(bps), bps[length(bps)] + 1)
# 			respond <- predict(marsmod,newdata=bps)
# 			slopes <- diff(respond) / diff(bps)
# 			return(4 * max(slopes)^(-1))
# 		},
# 		error=function(cond) {
# 			return(NA)
# 		}
#   )    
#   return(out)
# }

getBoth=function(simtab,s){
	out <- tryCatch(
		{
			binit <- 4 * (sqrt(8)*(sqrt(0.5) / sqrt(s)))^-1
			y = simtab$meanHI
			x = simtab$order
			
			fitModel = nls(y ~ a / (1 + exp(-b * (x - c))),start = list(a = 1, b = binit, c = length(x) / 2))
			res <- c(coef(fitModel)[2],coef(fitModel)[3])
			return(res)
		},
		error=function(cond) {
			return(NA)
		}
	)    
	return(out)
}

computeBlocksDistribution <- function(x,blocks){
  y <- c()
  number_of_blocks <- 0
  for(k in (0:100)){
    number_of_blocks <- sum(blocks < ((7/8)^k) & blocks > ((7/8)^(k+1)))
    y <- c(y,log(number_of_blocks / (((7/8)^k)-((7/8)^(k+1)))))
  }
  return(y)
}

LogBlocksDistribution <- function(x,S,R,M){
  theta = S / R
  logy = log((M * theta * 2) / (S * (1 + theta)^2)) - (((3 + theta) * x) / (1 + theta))
  return(logy)
}

slopeToTheta <- function(slope){
  # slope = - (3 + theta) / (1 + theta)
  # slope * (1 + theta) + theta = - 3
  # theta  = -3 - slope / (slope + 1)
  
  return( (-3 - slope) / (slope + 1) )
}

interceptToM <- function(intercept, selection, theta){
  # intercept = log((M * theta * 2) / (S * (1 + theta)^2))
  # exp(intercept) = (M * theta * 2) / (S * (1 + theta)^2)
  # (exp(intercept) * (S * (1 + theta)^2)) / (theta * 2) = M 
  
  return( (exp(intercept) * (selection * (1 + theta)^2)) / (theta * 2) )
}