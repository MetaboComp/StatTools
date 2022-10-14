#' Two-stage random forest imputation
#'
#' This wrapper performs parallel processing to make a 2-stage rfImp
#' 1st stage has "sloppy" settings to provide a reasonable 1st guess
#' for the 2nd iteration which has "nicer" settings.
#'
#' @param MAT Matrix for imputation; samples in rows, variables in columns
#' @param guess Initial guess (see mvImp; defaults to mvImp default)
#' @param forceZero Boolean for whether to force a lower imputation limit to zero (see mvImp; defaults to mvImp default)
#' @param nCore Number of slave processes (defaults to detectCores()-1)
#' @param tol1 Tolerance in 1st iteration (defaults to 0.05)
#' @param n1 MaxIter for 1st iteration (defaults to 15)
#' @param tol2 Tolerance in 2nd iteration (defaults to 0.05)
#' @param n2 MaxIter for 2nd iteration (defaults to 15)
#' @param method 'PLS' or 'RF
#' @param nComp Number of PLS components (defaults to 2)
#' @param rfMeth Which RF implementation to choose ('rf' (randomForest; default), 'ranger' or 'Rborist')
#'
#' 
#' @export
#' @return A data matrix with imputed values
#' MAT=matrix(runif(100000),nrow=40) # Generate synthetic (random) data
#' MAT[sample(1:100000,size = 1000)] <- NA # Punch 1000 random holes in the data
#' MAT_Imp <- rfImpWrap(MAT = MAT) # Imputation using default values

mvImpWrap=function(MAT,guess=NULL,forceZero=FALSE,method=c('PLS','RF'),rfMeth=c('rf','Rborist','ranger'),nComp=2,nCore,tol1=0.05,n1=15,tol2=0.025,n2=60) {
  library(doParallel)
  if(missing(method)) method <- 'RF'
  method=match.arg(method)
  if(missing(rfMeth)) rfMeth <- 'ranger'
  if(missing(nCore)) nCore=detectCores()-1
  cl <- makeCluster(nCore)
  registerDoParallel(cl)
  time1 <- proc.time()[3]
  cat('\nFIRST ROUND: Imputation with "sloppy" settings\n')
  cat('Tolerance:',tol1,'\n')
  cat('maxIter:  ',n1,'\n')
  imp <- mvImp(MAT = MAT, forceZero=forceZero, method=method, rfMeth=rfMeth, nComp=nComp, guess = guess, maxIter = n1, tolerance = tol1, parallel = TRUE)
  time2 <- proc.time()[3]
  cat('\n\nSECOND ROUND: Imputation with "nicer" settings and 1st guess from FIRST ROUND\n')
  cat('Tolerance:',tol2,'\n')
  cat('maxIter:  ',n2,'\n')
  imp <- mvImp(MAT = MAT, method=method, rfMeth=rfMeth, nComp=nComp, guess = imp$peakTable, maxIter = n2, tolerance = tol2, parallel = TRUE)
  stopCluster(cl)
  time3 <- proc.time()[3]
  cat('\n\n1ST ROUND: ',(time2-time1)/60,'min\n')
  cat('2ND ROUND: ',(time3-time2)/60,'min\n')
  cat('TOTAL    : ',(time3-time1)/60,'min\n')
  return(imp$peakTable)
}