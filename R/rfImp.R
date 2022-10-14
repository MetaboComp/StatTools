#' Perform random forest imputation of missing values
#'
#' @param MAT Indata matrix
#' @param maxIter Maximum number of iterations
#' @param tolerance Tolerance criterion (between two iterations) to accept imputed value
#' @param ntree Number of trees per forest
#' @param mtry Number of variables to choose from at each node
#' @param guess 1st guess ("median", i.e. median per feature; "minVar", minimum per variable (default); "minTotal", minimum of all variables; A matrix (imputed/full data matrix))
#' @param verbose Boolean for verbose output
#' @param parallel Whether to perform imputation in parallel (register backend if TRUE!)
#' @export
#' @return Object (list)
#' 
#' MAT=matrix(runif(100000),nrow=40) # Generate synthetic (random) data
#' MAT[sample(1:100000,size = 1000)] <- NA # Punch 1000 random holes in the data
#' library(doParallel)
#' nCore=detectCores()-1
#' cl=makeCluster(nCore)
#' registerDoParallel(cl)
#' ImputationObject <- rfImp(MAT = MAT, tolerance=0.05, maxIter=5) # Quick'n'Dirty settings for imputation
#' stopCluster(cl)
#' MAT_Imp <- ImputationObject$peakTable
rfImp=function(MAT,maxIter=15,tolerance=1e-2,ntree=100,mtry=5,guess=NULL,verbose=F,parallel=F) {
  MAT=as.matrix(MAT)
  library(doParallel)
  oldw <- getOption("warn")
  options(warn = -1)
  library(randomForest)
  NAs=whichNA=which(!is.finite(MAT),arr.ind = T)
  if (is.null(guess)) guess='minVar'
  if (is.null(dim(guess))) {
    cat('First guess from:',guess ,'(see ?rfImp for details) \n')
    rfPT=MAT
    if (guess=='minTotal') {
      rfPT[whichNA]=min(MAT,na.rm=T)
    } else {
      unikCol=sort(unique(whichNA[,2]))
      for (c in unikCol) {
        vect=MAT[,c]
        imput1=ifelse(guess=='median', median(vect, na.rm = T), min(vect, na.rm = T)) # median or minimum per variable
        vect[is.na(vect)]=imput1
        rfPT[,c]=vect
      }
    }
  } else {
    if(dim(guess)!=dim(MAT)) stop("matrix dimensions of guess don't match MAT")
    cat('First guess from user-specified matrix \n')
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
