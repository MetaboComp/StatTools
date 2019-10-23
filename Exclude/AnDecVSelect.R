ADMUVR=function(inData, Y, levels, ID, varRatio=0.9, nRep=5, nOuter=8, scale='yes',method='RF', nPerm=100, fitness='MISS'){
  library(StatTools)
  AD=AnDec(inData, levels, scale=scale)
  ssqP=AD$sumSqProp[-1]
  DecData=AD$factorMatrix[[length(levels)]]+AD$residuals
  library(doParallel)
  nCore=detectCores()
  cl=makeCluster(nCore)
  registerDoParallel(cl)
  library(MUVR)
  Xmodel=MUVR(X=DecData, Y=Y, ID=ID, fitness=fitness, nRep=nRep, nOuter=nOuter, varRatio=varRatio, method=method)
  stopCluster(cl)
  VIPs=getVIP(Xmodel, model='min') #Takes out most important variables, variable names in column 2
  cVIPs=c()
  for (i in 1:ncol(DecData)){
    if (is.element(colnames(DecData)[i], as.character(VIPs[,2]))){
      cVIPs <- append(cVIPs, i)
    }
  }
  Xsub=subset(X2, select=cVIPs)
  ADsub=AnDec(Xsub, levels, scale=scale)
  ssqPsub=ADsub$sumSqProp[-1]
  DecDatasub=ADsub$factorMatrix[[length(levels)]]+ADsub$residuals
  cl=makeCluster(nCore)
  registerDoParallel(cl)
  Xsubmodel=MUVR(X=DecDatasub, Y=Y, ID=ID, fitness=fitness, nRep=nRep, nOuter = nOuter, varRatio = varRatio, method=method)
  stopCluster(cl)
  pFtest=numeric(length(levels))
  for(i in 1:length(levels)){ #makes an f-test for all levels
    subVar<-var.test(ADsub$factorMatrix[[i]], ADsub$residuals) #X eller AD?
    pFtest[i] <- subVar$p.value
  }
  #returning information to output
  modelReturn=list(call=match.call())
  modelReturn$Ftest$levels=colnames(levels)
  modelReturn$VIPs=VIPs
  modelReturn$Ftest$pvalue=pFtest
  modelReturn$Xmodel=Xmodel
  modelReturn$Xsubmodel=Xsubmodel
  modelReturn$Xmodel$ssq=ssqP
  modelReturn$Xsubmodel$ssq=ssqPsub
  return(modelReturn)
}

