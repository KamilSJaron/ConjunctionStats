getBlockSummaries = function(path = '.', pattern = '.dat'){
  # 	sim is list of all simulations
  AblocksList <- list(list())
  BblocksList <- list(list())
  
  nameIn <- dir(path, pattern = pattern)
  nameIn <- paste0(path,nameIn)
  
  for(file_index in 1:length(nameIn)){
    
    blockFile <- readLines(nameIn[file_index]) # read the file with blocks
    last_deme_index = unlist(strsplit(blockFile[1],'\t'))[1]; # read first deme index
    
    AblocksList[[file_index]] <- list()
    BblocksList[[file_index]] <- list()
    AblocksDeme <- c() # init / clear vectors for blocks
    BblocksDeme <- c()
    
    for(AlineIndex in seq(1, length(blockFile), by = 2)){ # go thought odd lines (A lines)
      Aline <- unlist(strsplit(blockFile[AlineIndex],'\t'))
      deme_index <- Aline[1]
      
      if(last_deme_index != deme_index){
        AblocksList[[file_index]][[last_deme_index]] <- strtoi(AblocksDeme)
        BblocksList[[file_index]][[last_deme_index]] <- strtoi(BblocksDeme)
        
        AblocksDeme <- c() # new deme, clear vectors
        BblocksDeme <- c()
      }
      
      AblocksDeme <- c(AblocksDeme,Aline[-1]) # add block sizes to vector without deme index (first value of each row)
      BblocksDeme <- c(BblocksDeme,unlist(strsplit(blockFile[AlineIndex+1],'\t'))[-1]) # read the B line (even line)
      
      last_deme_index <- deme_index;
    }
    
    AblocksList[[file_index]][[last_deme_index]] <- strtoi(AblocksDeme)
    BblocksList[[file_index]][[last_deme_index]] <- strtoi(BblocksDeme)
  }
  return(list('A' = AblocksList, 'B' = BblocksList));
}


getNumberOfInvaders = function(sim, path = '.', pattern = '.dat'){

  #path = '../160525_1d_coupling/'
  #sim = sim_1M
  #pattern = '.dat'
  
    # 	sim is list of all simulations
  nameIn <- dir(path, pattern = pattern)
  nameIn <- paste0(path,nameIn)
  
  if(length(sim) != length(nameIn)){
    print('Not matching number of files with number')
    return(0)
  }
  invaders <- rep(0,length(nameIn))
  
  for(file_index in 1:length(nameIn)){
    
    blockFile <- readLines(nameIn[file_index]) # read the file with blocks
    blockFile <- strsplit(blockFile,'\t')
    
    simtab <- sim[[file_index]][order(sim[[file_index]]$order),] # take apropriate simulation table
    
    BinvaderDemes <- simtab$DEME[simtab$meanHI < 0.5 & simtab$meanHI != 0]
    AinvaderDemes <- simtab$DEME[simtab$meanHI > 0.5 & simtab$meanHI != 1]
    
    for(AlineIndex in seq(1, length(blockFile), by = 2)){ # go thought odd lines (A lines)
      Aline <- blockFile[[AlineIndex]]
      deme_index <- strtoi(Aline[1])
      
      if(deme_index %in% BinvaderDemes){
        Bline <- blockFile[[AlineIndex+1]]
        if(length(Bline) > 1){
          invaders[file_index] = invaders[file_index] + 1
        }
      }
      
      if(deme_index %in% AinvaderDemes & length(Aline) > 1){
        invaders[file_index] = invaders[file_index] + 1
      }
    }
  }
  return(invaders);
}