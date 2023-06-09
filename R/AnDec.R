#' ANOVA decomposition
#'
#' ANOVA decomposition of `inData` according to `levels`
#' @param inData A matrix/data.frame with inData (samples as rows, variables as columns)
#' @param levels A matrix/data.frame with levels in (one factor per column, multiple levels per factor)
#' @param scale If `no` (default) then no scaling. If `sd` scales to sd=1 per variable. If vector scales variables according to defined scaling factor.
#' @return An object containing ANOVA-decompositioned data
#' @export
AnDec=function(inData,levels,scale='no') {
  levels=data.frame(levels)
  retList=list()
  retList$inData=inData
  retList$levels=levels
  nSamp=nrow(inData)
  nVar=ncol(inData)
  nFact=ncol(levels)
  m=matrix(colMeans(inData),nrow=1,dimnames=list('mean',colnames(inData)))
  mMat=m[rep(1,nSamp),]
  rownames(mMat)=rownames(inData)
  retList$mean=m
  retList$meanMatrix=mMat
  if(scale[1]=='no') scale=rep(1,nVar)
  if(scale[1]=='yes') scale=apply(inData,2,sd)
  if(length(scale)!=nVar) {
    cat('Wrong length of scaling vector')
    return(NULL)
  }
  retList$scale=scale
  Resid=scale(inData,scale=scale)
  retList$scaledData=Resid
  factorMatrix=factorList=list()
  sumSq=numeric(nFact+2)
  names(sumSq)=c('total',colnames(levels),'residual')
  sumSq[1]=sum(abs(Resid^2))
  for (f in 1:nFact) {
    levelF=factor(levels[,f])
    levs=levels(levelF) # Unique levels
    nLev=length(levs)       # Number of unique levels
    lev=matrix(nrow=nLev,ncol=nVar,dimnames=list(levs,colnames(inData))) # Prepare matrix for level averages
    levMat=matrix(nrow=nSamp,ncol=nVar,dimnames=list(rownames(inData),colnames(inData))) # Prepare full level-averaged matrix
    for (l in 1:nLev) {
      wh.lev=levelF==levs[l] # Extract observations at this level
      lev[l,]=apply(Resid[wh.lev,,drop=F],2,mean) # average per level and store
      levMat[wh.lev,]=lev[rep(l,sum(wh.lev)),]
    }
    factorList[[f]]=lev
    factorMatrix[[f]]=levMat  
    sumSq[f+1]=sum(levMat^2)
    Resid=Resid-levMat
  }
  retList$factorList=factorList
  retList$factorMatrix=factorMatrix
  retList$residuals=Resid
  sumSq[nFact+2]=sumSq[1]-sum(sumSq[2:(length(sumSq)-1)])
  retList$sumSq=sumSq
  retList$sumSqProp=sumSq/sumSq[1]
  return(retList)
}