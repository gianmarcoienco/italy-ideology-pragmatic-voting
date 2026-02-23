# imtermediate refreshing of global envir ####
CLEARCOND <- function(objs) { 
    objs <- ls(pos = ".GlobalEnv")
    lcase <- objs[grep("[a-z]", objs)] 
    #remove lowercase objects that are not functions 
      #NOTE: lowercase functions are needed to correct for update-debited syntax changes
    rm(list = setdiff(lcase, lsf.str()),pos = globalenv())
}

 
