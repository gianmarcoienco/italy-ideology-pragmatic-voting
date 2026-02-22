# TARGET: load (or rename) df
LOAD <- function(dfinput, dfextension='.rda', pattdir=TEMP) {
 
  #######################
  #load main file (if not in global envir already)
  setwd(pattdir)
  
  if(class(dfinput)=='character'){
    load(file=paste0(dfinput,dfextension), .GlobalEnv)
    dfx <- as.data.frame(mget(ls(pattern = paste0(dfinput,'$'), envir = .GlobalEnv), envir = .GlobalEnv))
    colnames(dfx) <- sub(paste0(dfinput,"."), "", colnames(dfx))
  }
  
  if(class(dfinput)=='data.frame'){
    dfx <- dfinput
  }
  
  rm(list=dfinput, envir = .GlobalEnv)
  gc()
  return(dfx)
}

# MWE: 
# df <- LOAD("m6practice_databasics_ex1")