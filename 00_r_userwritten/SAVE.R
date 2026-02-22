# TARGET: dynamically save to disc  ####
SAVE <- function(dfx,namex=MAINNAME,pattdir=TEMP,pattdata='.rda'
  #,dfxdepend="makesurenomatch"
  ) {
    #set dir
    setwd(paste0(pattdir))
    #assign name
    assign(paste0(namex), dfx) 
    #attach extension    
    dfname <- paste0(namex,pattdata)
    #save
    save(list=paste0(namex), file = dfname) 
    # #delete downstream df s.t. it will be updated     
    # if(RESETDF==F){      
    #   if(exists("DFDEPEND")){          
    #     if(!grepl(DFDEPEND,MAINNAME)){
    #       #print('here')
    #       dfxdepend <- DFDEPEND
    #       #print(dfxdepend)
    #     }  
    #   }
    #   junk <- dir(path=A, pattern=dfxdepend) 
    #   #print(junk)
    #   file.remove(junk)          
    #   junk <- dir(path=TEMP, pattern=dfxdepend)    
    #   #print(junk)
    #   file.remove(junk)    
    # }
}

#MWE: SAVE(dftest)
