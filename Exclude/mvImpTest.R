rm(list=ls())
# Test on "small" matrix
MAT=matrix(runif(1e5),nrow=40) # Generate synthetic (random) data
MAT[sample(1:1e5,size = 1000)] <- NA # Punch 1000 random holes in the data
# Test on "large" matrix
MATLarge=matrix(runif(1e6),nrow=200) # Generate larger synthetic (random) data
MATLarge[sample(1:1e6,size = 40000)] <- NA # Punch 40000 random holes in the data
MAT=MATLarge
library(doParallel)
nCore <- detectCores()-1
cl <- makeCluster(nCore)
registerDoParallel(cl)
maxIter = 60
tolerance = 0.01
impRFrf=mvImp(MAT,maxIter = maxIter,tolerance = tolerance,parallel=T, method='RF', rfMeth = 'rf')
# impRFRb=mvImp(MAT = MAT, maxIter = maxIter, tolerance = tolerance, parallel=T, method='RF', rfMeth = 'Rborist', ntree=100, mtry=5, parallel =T, verbose=T) # Something not right; Look at this later...
impRFra=mvImp(MAT = MAT, maxIter = maxIter, tolerance = tolerance, parallel=T, method='RF', rfMeth = 'ranger')
impMV_PLS=mvImp(MAT,maxIter = maxIter,tolerance = tolerance,parallel=T, method='PLS')
stopCluster(cl)

impMat=cbind(impRFrf$imputeNA,impRFra$imputeNA,impMV_PLS$imputeNA)
pairs(impMat,xlim=c(0,1), ylim=c(0,1))


# Test mvImpWrap
# iRF=rfImpWrap(MAT)
library(StatTools)
iMV_rf=mvImpWrap(MAT,method='RF',rfMeth='rf')
iMV_ra=mvImpWrap(MAT,method='RF',rfMeth='ranger')
iMV_pls=mvImpWrap(MAT,method='PLS')
NAs=which(is.na(MAT))
impMat=cbind(iMV_rf[NAs],iMV_ra[NAs],iMV_pls[NAs])
png()
pairs(impMat,xlim=c(0,1), ylim=c(0,1))
dev.off()

