#' Perform random forest imputation of missing values
#'
#' @param MAT Indata matrix
#' @param maxIter Maximum number of iterations
#' @param tolerance Tolerance criterion (between two iterations) to accept imputed value
#' @param ntree Number of trees per forest
#' @param mtry Number of variables to choose from at each node
#' @param guess 1st guess (defaults to median imputation)
#' @param verbose Boolean for verbose output
#' @param parallel Whether to perform imputation in parallel (register backend if TRUE!)
#'
#' @return Object (list)
#' @export
rfImp=function(MAT,maxIter=15,tolerance=1e-2,ntree=100,mtry=5,guess,verbose=F,parallel=F) {
  library(doParallel)
  oldw <- getOption("warn")
  options(warn = -1)
  library(randomForest)
  NAs=whichNA=which(is.na(MAT),arr.ind = T)
  if (missing(guess)) {
    unikCol=sort(unique(whichNA[,2]))
    rfPT=MAT  # Perform median imputation as first guess
    for (c in unikCol) {
      vect=MAT[,c]
      median=median(vect,na.rm = T)
      vect[is.na(vect)]=median
      rfPT[,c]=vect
    }
    cat('First guess from feature median imputation \n')
  } else {
    rfPT=guess
  }
  nSamp=nrow(rfPT)
  sampInd=1:nSamp
  if (any(table(whichNA[,2])>(nSamp-2))) {
    cat('\nERROR: You cannot train a model with variables with less than 2 data points!\n')
    cat('\nReturn: NULL\n')
    return(NULL)
  }
  # Prep for iterations
  iter=T
  rep=1
  if (parallel) "%doVersion%"=get("%dopar%") else "%doVersion%"=get("%do%")
  while(iter==T) {
    whichFeat=sort(unique(NAs[,2]))  # Which features have missing values?
    cat('Iteration',rep,'\n')
    cat('',nrow(NAs),'NAs in',length(whichFeat),'features','\n')
    rfPTnew=rfPT
    iteration=foreach(fi=1:length(whichFeat), .packages="randomForest") %doVersion% {
      feat=whichFeat[fi]
      if (verbose) cat(fi,' (',feat,')... ',sep='')
      predInd=NAs[NAs[,2]==feat,1]
      trainInd=sampInd[!sampInd%in%predInd]
      rfMod=randomForest(x=rfPT[trainInd,-feat],y=rfPT[trainInd,feat],ntree=ntree,mtry=mtry)
      rfPred=predict(rfMod,newdata=rfPT[predInd,-feat])
      return(rfPred)
      if (verbose) cat('\n')
    }
    for (fi in 1:length(whichFeat)) {
      feat=whichFeat[fi]
      predInd=NAs[NAs[,2]==feat,1]
      rfPTnew[predInd,feat]=iteration[[fi]]
    }
    still=which((abs(rfPT[NAs]-rfPTnew[NAs])/rfPT[NAs])>tolerance)
    rfPT=rfPTnew
    if (length(still)==0) {
      iter=F
      cat('Tolerance met for all NAs. Imputation finished.')
    } else {
      NAs=NAs[still,,drop=F]
      if (rep==maxIter) {
        iter=F 
        cat('Maximum iterations reached. Still',length(still),'NAs not within tolerance criterion.')
      } else {
        rep=rep+1
      }
    }
  }
  options(warn = oldw)
  returnList=list(peakTable=rfPT,imputeNA=rfPT[whichNA])
  if (length(still)!=0) returnList$notMet=NAs
  return(returnList)
}
