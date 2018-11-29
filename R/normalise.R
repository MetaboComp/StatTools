#' Normalisation (ripped from some other package)
#'
#' @param X Data to normalise (samples in rows, variables in columns)
#' @param method Constant sum (method<-'CS'); Constant noise (method<-'CN'); Qoutient probabilistic method (method<-'PQN'); Linear baseline normalisation (method<-'LBN'); Auto-scaling (method<-'AS'); Pareto scaling (method<-'PS')
#' @param refIdx Parameter for PQN (hard setting reference sample; defaults to median)
#' @param noiseInt Parameter for method 'CN'
#'
#' @return Normalised data
#' @export
normalise <-
  function(X,method,refIdx,noiseInt=c(11,12))
  {
    # Removing dilutions between biofluid samples (normalisation of spectra)
    
    # Normalisation and scaling of spectra profiles
    
    # X [observation dimension]
    # method - Constant sum (method<-'CS')
    #        - Constant noise (method<-'CN')
    #        - Qoutient probabilistic method (method<-'PQN')
    #        - Linear baseline normalisation (method<-'LBN')
    #        - Auto-scaling (method<-'AS')
    #        - Pareto scaling (method<-'PS')
    # L. Hedjazi, ICAN, 2014
    
    if (nargs()<2)
    {
      stop('incorrect number of input parameters')
    }
    if (!is.matrix(X)){X<-as.matrix(X)}
    
    obs<-dim(X)[1]
    dimm<-dim(X)[2]
    
    
    # Normalisation
    
    factors<-rep(NaN,obs)
    
    
    switch(method,
           CS={
             print("Start constant sum normalisation:")
             factors<-apply(X,1,sum)
             med<-median(factors)
             data.cs<-med*(X/factors)
             return(list(Sp=data.cs, factors=factors))},
           
           CN={
             print("Start constant noise normalisation:")
             ppm<-as.numeric(sub("e\\.","e-",sub("ppm_","",sub("ppm_\\.","-", colnames(X)))))
             st<-which(ppm>=noiseInt[1] & ppm<=noiseInt[2])
             start<-min(st)
             end<-max(st)
             factors<-apply(X[,start:end],1,sd)
             data.cn<-(X/factors)
             return(list(Sp=data.cn, factors=factors))},
           
           LBN={
             print("Start linear baseline normalisation:")
             linear.baseline <- apply(X,2,median)
             baseline.mean <- mean(linear.baseline)
             sample.means <- apply(X,1,mean)
             factors <- baseline.mean/sample.means
             data.lbn<-(X*factors)
             return(list(Sp=data.lbn, factors=factors))},
           PQN={
             print("Start probabilistic quotient normalisation:")
             if (missing(refIdx)) {
               reference <- apply(X,2,median)
             }else{reference<-X[refIdx,]}
             X[0==X]<-0.00000001
             quotient <- t(X)/reference
             quotient.median <- apply(quotient,2,median)
             data.pqn<-(X/quotient.median)
             return(list(Sp=data.pqn, factors=quotient.median, NormSp=reference))},
           AS={
             print("Start Auto-scaling:")
             centered.data <- t(X) - apply(X,2,mean)
             scaling.auto <- apply(X,2,sd)
             auto.data <- t(centered.data/scaling.auto)
             return(list(Sp=auto.data, factors=scaling.auto))},
           PS={
             print("Start Pareto scaling:")
             centered.data <- t(X) - apply(X,2,mean)
             scaling.pareto <- sqrt(apply(X,2,sd))
             pareto.data <- t(centered.data/scaling.pareto)
             return(list(Sp=pareto.data, factors=scaling.pareto))})
  }