#' Perform random forest imputation of missing values
#'
#' @param MAT Indata matrix
#' @param guess 1st guess ("median", i.e. median per feature; "minVar", minimum per variable (default); "minTotal", minimum of all variables; A matrix (imputed/full data matrix))
#' @param maxIter Maximum number of iterations
#' @param tolerance Tolerance criterion (between two iterations) to accept imputed value
#' @param forceZero Boolean for whether to force a lower imputation limit to zero (e.g. normal-scale concentrations & PLS; defaults to FALSE)
#' @param method 'PLS' or 'RF
#' @param nComp Number of PLS components (defaults to 2)
#' @param rfMeth Which RF implementation to choose ('rf' (randomForest; default), 'ranger' or 'Rborist')
#' @param ntree Number of trees per forest
#' @param mtry Number of variables to choose from at each node
#' @param verbose Boolean for verbose output
#' @param parallel Whether to perform imputation in parallel (register backend if TRUE!)
#'
#' 
#' @export
#' @return Object (list)
#' MAT=matrix(runif(100000),nrow=40) # Generate synthetic (random) data
#' MAT[sample(1:100000,size = 1000)] <- NA # Punch 1000 random holes in the data
#' library(doParallel)
#' nCore=detectCores()-1
#' cl=makeCluster(nCore)
#' registerDoParallel(cl)
#' ImputationObject <- rfImp(MAT = MAT, tolerance=0.05, maxIter=5) # Quick'n'Dirty settings for imputation
#' stopCluster(cl)
#' MAT_Imp <- ImputationObject$peakTable
mvImp=function(MAT,method=c('RF','PLS'),maxIter=15,tolerance=1e-2,guess=NULL,forceZero=FALSE,rfMeth='rf',ntree=100,mtry=5,nComp=2,verbose=FALSE,parallel=FALSE) {
  library(doParallel)
  if(missing(method)) method <- 'RF'
  method=match.arg(method)
  if(method=='RF' & missing(rfMeth)) rfMeth <- 'ranger'
  
  # Suppress warnings
  oldWarning <- getOption("warn")
  options(warn = -1)
  
  MAT=as.matrix(MAT) # Make sure MAT is in matrix format for increased speed
  if (is.null(colnames(MAT))) colnames(MAT) <- paste0('V',1:ncol(MAT))
  NAs=whichNA=which(!is.finite(MAT),arr.ind = TRUE) # Find missing values
  
  # Fix 1st guess 
  if (is.null(guess)) guess='minVar'
  if (is.null(dim(guess))) {
    cat('First guess from:',guess ,'(see ?mvImp for details) \n')
    impPT=MAT
    if (guess=='minTotal') {
      impPT[whichNA]=min(MAT,na.rm=T)
    } else {
      unikCol=sort(unique(whichNA[,2]))
      for (c in unikCol) {
        vect=MAT[,c]
        imput1=ifelse(guess=='median', median(vect, na.rm = T), min(vect, na.rm = T)) # median or minimum per variable
        vect[is.na(vect)]=imput1
        impPT[,c]=vect
      }
    }
  } else {
    if(dim(guess)!=dim(MAT)) stop("matrix dimensions of guess don't match MAT")
    cat('First guess from user-specified matrix \n')
    impPT=guess
  }
  
  nSamp=nrow(impPT) # Number of samples
  sampInd=1:nSamp # Sample indices
  
  # Need at least 2 data points per variable to make regression!
  if (any(table(whichNA[,2])>(nSamp-2))) {
    stop('You cannot train a model with variables with less than 2 data points!\n')
  }
  
  # Prep for iterations
  iterate=TRUE
  iteration=1
  if (parallel) "%doVersion%"=get("%dopar%") else "%doVersion%"=get("%do%") # Parallel vs serial
  
  if (method=='PLS') pkg <- 'mixOmics' else {
    if (rfMeth=='rf') pkg <- 'randomForest' else pkg <- rfMeth
  } 
  totalTime <- proc.time()[3]
  
  # Iterate
  while(iterate) {
    iterTime <- proc.time()[3]
    whichFeat=sort(unique(NAs[,2]))  # Which features have missing values?
    cat('Iteration',iteration,'\n')
    cat('',nrow(NAs),'NAs in',length(whichFeat),'features','\n')
    impPTnew=impPT
    
    if(length(whichFeat) == 0){
      cat('No missing values found in table, no imputation necessary.\nReturning original matrix.')
      return(MAT)
    }
    
    iterationResult=foreach(fi=1:length(whichFeat), .packages=pkg) %doVersion% {
      feat=whichFeat[fi] # Which feature to impute
      impVar <- colnames(MAT)[feat] # name of feature to impute
      if (verbose) cat(fi,' (',feat,': ',impVar,')... ',sep='')
      predInd=NAs[NAs[,2]==feat,1] # Which observations/samples to to impute
      trainInd=sampInd[!sampInd%in%predInd] # Observations/samples to build model upon
      # Actual imputation
      if (method=='RF') {
        if (rfMeth=='rf') {
          if(mtry>=ncol(MAT)) mtry <- ncol(MAT)-1
          # randomForest implementation
          rfMod=randomForest(x=impPT[trainInd,-feat],y=impPT[trainInd,feat],ntree=ntree,mtry=mtry)
          Pred=predict(rfMod,newdata=impPT[predInd,-feat,drop=F])
        } else if (rfMeth=='ranger') {
          # ranger implementation
          rfMod <- ranger(dependent.variable.name = impVar, data = impPT[trainInd,], num.trees = ntree, mtry = mtry)
          Pred <- predict(rfMod,data=impPT[predInd,,drop=F])$predictions
        } else {
          # Rborist implementation
          rfMod <- Rborist(x=impPT[trainInd,-feat], y=impPT[trainInd,feat], nTree=ntree, predFixed = mtry)
          Pred <- predict(rfMod,newdata=impPT[predInd,-feat,drop=F])$yPred
        }
      } else {
        plsMod <- pls(X=impPT[trainInd,-feat], Y=impPT[trainInd,feat], ncomp = nComp, mode = 'regression',all.outputs = FALSE)
        Pred <- predict(plsMod, newdata = impPT[predInd,-feat,drop=FALSE])$predict[,,nComp]
      }
      if (forceZero) Pred[Pred<0] <- 0
      if (verbose) cat('\n')
      return(Pred)
    }
    for (fi in 1:length(whichFeat)) {
      feat=whichFeat[fi]
      predInd=NAs[NAs[,2]==feat,1]
      impPTnew[predInd,feat]=iterationResult[[fi]]
    }
    iterTime <- proc.time()[3] - iterTime
    cat(' Time: ',iterTime/60,'min\n')
    whichConverged <- which((impPT[NAs]==impPTnew[NAs]) | (abs(impPT[NAs]-impPTnew[NAs])/impPT[NAs])<tolerance)
    stillLeft=which((impPT[NAs]!=impPTnew[NAs]) & (abs(impPT[NAs]-impPTnew[NAs])/impPT[NAs])>tolerance)
    impPT=impPTnew
    if (length(stillLeft)==0) {
      iterate=FALSE
      cat('\nTolerance met for all NAs. Imputation finished.')
    } else {
      NAs=NAs[stillLeft,,drop=F]
      if (iteration==maxIter) {
        iterate=FALSE
        cat('\nMaximum iterations reached. Still',length(stillLeft),'NAs not within tolerance criterion.')
      } else {
        iteration=iteration+1
      }
    }
  }
  totalTime <- proc.time()[3] - totalTime
  cat('\nTotal time: ',totalTime/60,'min\n')
  options(warn = oldWarning)
  returnList=list(peakTable=impPT,imputeNA=impPT[whichNA])
  if (length(stillLeft)!=0) returnList$notMet=NAs
  return(returnList)
}

