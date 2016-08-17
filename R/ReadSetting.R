#' @title ReadSetting
#'
#' @description
#' \code{ReadSetting} parses the settings file used for a simulation run using Conjunction software and converts it to a data.frame object.
#'
#' @param Inputfile A string of characters indicating the settings file to be parsed. If no file is specified via InputFile, \code{ReadSetting} reads the file \code{settings.txt} in the current working directory. 
#'
#' @return A data.frame object, featuring information about the settings used in the simulation run in a R-friendly format.
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @examples{
#'    myReadSetting = ReadSetting(InputFile='./mysetting.txt')
#' }
#'
#' @export

ReadSetting <- function(InputFile = './setting.txt'){
	gradientTable <- data.frame(run = 1)
	# 'C' = numeric(0),'L' = numeric(0),'r' = numeric(0),'s' = numeric(0),'b' = numeric(0)
	summaryFile <- readLines(InputFile)
	generations <- as.numeric(strsplit(summaryFile[grepl('NUMBERofGENERATIONS',summaryFile)], "= | #")[[1]][2])
	for(l in summaryFile){
		l <- unlist(strsplit(l,split='#'))[1]
		l <- unlist(strsplit(l,split='='))
		if(grepl('RECOMBINATIONrate',l[1]) | grepl('LAMBDA',l[1])){
			if(grepl("\\[",l[2])){
				gradientTable <- merge(gradientTable,data.frame('r' = GetVectorSplit(l)))
			} else {
				gradientTable <- merge(data.frame('r' = as.numeric(l[2])),gradientTable)
			}
		}
		if(grepl('SELECTION',l[1])){
			if(grepl("\\[",l[2])){
				gradientTable <- merge(gradientTable,data.frame('s' = GetVectorSplit(l)))
			} else {
				gradientTable <- merge(data.frame('s' = as.numeric(l[2])),gradientTable)
			}
		}
		if(grepl('CHROM',l[1])){
			if(grepl("\\[",l[2])){
				gradientTable <- merge(gradientTable,data.frame('C' = GetVectorSplit(l)))
			} else {
				gradientTable <- merge(data.frame('C' = as.numeric(l[2])),gradientTable)
			}
		}
		if(grepl('LOCI',l[1])){
			if(grepl("\\[",l[2])){
				gradientTable <- merge(gradientTable,data.frame('L' = GetVectorSplit(l)))
			} else {
				gradientTable <- merge(data.frame('L' = as.numeric(l[2])),gradientTable)
			}
		}
		if(grepl('BETA',l[1])){
			if(grepl("\\[",l[2])){
				gradientTable <- merge(gradientTable,data.frame('b' = GetVectorSplit(l)))
			} else {
				gradientTable <- merge(data.frame('b' = as.numeric(l[2])),gradientTable)
			}
		}
		if(grepl('NUMBERofSAVES',l[1])){
		  saves <- as.numeric(l[2])
			if(saves > 1){
				gradientTable <- merge(data.frame('G' = (generations / saves) * 1:saves) ,gradientTable)
			}
		}
		if(grepl('DEMEsize',l[1])){
			if(grepl("\\[",l[2])){
				gradientTable <- merge(gradientTable,data.frame('D' = as.double(strsplit(strsplit(strsplit(l[2],split=c('\\['))[[1]][2],split='\\]')[[1]][1],split=',')[[1]])))
			} else {
				gradientTable <- merge(data.frame('D' = as.numeric(l[2])),gradientTable)
			}
		}
	}
	gradientTable$run <- c()
	return(gradientTable)
}
