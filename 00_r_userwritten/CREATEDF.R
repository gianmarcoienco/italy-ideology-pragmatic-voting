#Target: create samle dummy data
CREATEDF <- function(nx=10,endogx=0.5,seedx=123){
  set.seed(seedx)
  z1 <- rnorm(nx)
  u <- rnorm(nx)
  # Create the independent variables
  x1 <- z1 + endogx*u + rnorm(length(z1))
  dfx <- cbind(z1,x1,u)
  #df_status(dfx)
  colnames(dfx) <- sub("var.","",colnames(dfx))  
  return(dfx)
}
# #MWE:
# dftest <- CREATEDF(nx=15)
# dftest